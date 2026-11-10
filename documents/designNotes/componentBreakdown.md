# Component Breakdown #

Kinetic Merge is structured into three SBT subprojects:

1. `coreLibrary` (published artifact `kinetic-merge-core`): Contains the Git-agnostic core data structures, algorithms,
   match analysis, block construction, LCS, and code motion merge algebras under package
   `com.sageserpent.kineticmerge.core`.
2. `gitCliApplication` (executable `kinetic-merge`): Contains the CLI application integrating with Git repositories
   under package `com.sageserpent.kineticmerge.gitCliApplication`.
3. `toolCliApplication` (executable `kinetic-merge-tool`): Contains the CLI application operating on arbitrary
   directories under package `com.sageserpent.kineticmerge.toolCliApplication`.

---

## 1. CLI Applications

Both `gitCliApplication` and `toolCliApplication` provide command-line entry points using `scopt` for argument parsing
and implement direct-style monadic workflows via `dotty-cps-async`.

### Monadic Workflows (`Workflow`)
The CLI layers encapsulate effectful, potentially failing operations inside a monad transformer stack:
```scala
type Workflow[Payload] = EitherT[WorkflowLogWriter, String @@ Tags.ErrorMessage, Payload]
type WorkflowLogWriter[Payload] = WriterT[IO, WorkflowLog, Payload]
```
Operations execute on Cats Effect `IO`, accumulating log messages in `WorkflowLog` and capturing failures as tagged
error messages. Direct-style syntax is enabled via `dotty-cps-async`:
- Asynchronous expressions are wrapped in `reify { ... }`.
- Monadic effects are evaluated and extracted within `reify` using `.reflect`.
- A dedicated non-recursive `CpsPureMonadInstanceContext[Workflow]` given instance drives the transformation safely
  without runtime recursion loops.

### `gitCliApplication.Main`
- Entry point for the `kinetic-merge` executable.
- Verifies that Git is installed and that the working directory is clean.
- Resolves the commit SHAs for the merge base, our branch (`HEAD`), and their branch.
- Inspects repository changes (`Main.Change`, `Main.MergeInput`) to classify files into additions, modifications, and
  deletions.
- Maps Git blob contents into `core.Sources` instances.
- Drives `SectionedCode.of` and `SectionedCodeExtension.merge`.
- Translates the resulting `MergeResult` instances back into Git index entries (stages 1, 2, and 3 for conflicts) and
  working tree files with conflict markers, or creates a merge commit if clean.
- Supports `--no-commit`, `--no-ff`, and `--no-progress`.

### `toolCliApplication.Main`
- Entry point for the `kinetic-merge-tool` executable.
- Operates on three directories: `baseDirectory`, `ourDirectory` (`left`), and `theirDirectory` (`right`).
- Reads raw files directly from disk into `core.Sources`.
- Executes `SectionedCode.of` and `SectionedCodeExtension.merge`.
- Writes merged outputs directly back to the target directory, emitting conflict markers or staging clean files.

---

## 2. Core Library (`com.sageserpent.kineticmerge.core`)

The core library is fully generic over `Path` and `Element`, allowing unit and property-based tests to exercise
algorithms using lightweight primitive types (e.g. integer offsets, characters) while production applications use
`RelPath` and `Token`.

### `Token`
- Represents a discrete token of source content (word, number, punctuation, or string literal).
- Folds trailing whitespace into the token as a suffix, while leading line indentation is folded in as a prefix.
- Equality and ordering ignore trailing whitespace suffixes and leading line indentation, providing built-in
  whitespace insensitivity across all matching and diffing operations.

### `Sources` & `MappedContentSources`
- Abstract representation of a codebase for a given side (`base`, `left`, `right`).
- Maps file paths to their content sequences.
- Acts as a factory for `Section` instances and maps any section back to its owning file path.
- `filesByPathUtilising(mandatorySections)` partitions each file into a contiguous sequence of sections such that all
  mandatory sections (from matches) are included, and any gaps are covered by filler sections.

### `Section`
- An immutable closed-open interval `[startOffset, onePastEndOffset)` of token offsets within a specific file on a
  specific side.
- Compared by its defining attributes (`path`, `startOffset`, `size`), *not* by object identity or content equality.
- Provides access to its slice of tokens via `.content`.

### `SectionsSeen`
- A persistent, high-performance Augmented Treap indexing sections by `startOffset` and augmented with the maximum
  `onePastEndOffset` of its subtrees.
- Replaces earlier fingertree structures to provide fast interval overlap and subsumption queries (`findOverlapping`,
  `overlappingOrSubsumedSections`).
- Each treap node caches its precomputed `hashCode` as a `val` via tuple hashing to ensure O(1) hash code calculation
  and stack-safe rotations.

### `Match`
- Represents matched sections across sides:
  - `Match.AllSides(base, left, right)`: Matches across all three sides.
  - `Match.BaseAndLeft(base, left)`: Pairwise match between base and left.
  - `Match.BaseAndRight(base, right)`: Pairwise match between base and right.
  - `Match.LeftAndRight(left, right)`: Pairwise match between left and right.
- Sections in a match need not reside at the same file path (supporting inter-file code motion).

### `RollingHash`
- Fast rolling polynomial hash (Rabin-Karp style) used to compute token window fingerprints.
- Factory instances (`RollingHash.Factory`) precompute window coefficients; session-local caches within
  `MatchAnalysis.of` cache factories by window size (`Cache[Int, RollingHash.Factory]`).

### `MatchAnalysis`
- The core engine for match discovery and reconciliation.
- Implements:
  1. Iterated binary search for surefire matches above the sure-fire window size.
  2. Linear scan downward through window sizes for small-fry matches.
  3. Discovery of tiny matches (down to window size 1) used strictly to break up gap fills.
  4. `parallelMatchesOnly`: Uses meta-matching over matched section sequences to discard isolated coincidental matches
     and retain coherent parallel matches.
  5. `reconcileOverlappingMatches`: Tail-recursively resolves overlapping matches by biting contested sections into
     smaller sub-match fragments.
  6. `reconcileSubsumingMatches`: Iteratively fragments larger pairwise matches subsumed by all-sides matches.
  7. `groupsOfParallelMatches`: Unifies all-sides and pairwise matches into `ParallelMatchesGroup`s, validating bridging
     pairs and sandwich structures.

### `SectionedCode`
- Represents the final breakdown of all three sides into `File`s containing non-overlapping contiguous `Section`s.
- `SectionedCode.Block`: Represents a contiguous sequence of sections in a file covered by a parallel matches group or
  filler.
- Companion factory `SectionedCode.of` coordinates match analysis, builds `baseFilesByPath`, `leftFilesByPath`, and
  `rightFilesByPath`, and constructs condensed `baseBlocks`, `leftBlocks`, and `rightBlocks`.

### `LongestCommonSubsequence`
- Computes three-way and pairwise alignments between sequences of elements.
- Optimized with:
  - Common affix trimming (`commonAffix`) to eagerly peel off common prefixes and suffixes taking into account pairwise
    and 3-way matches without dynamic programming overhead.
  - Matchability filtering to strip elements that do not occur on other sides.
  - Wavefront swathe-based dynamic programming evaluating subproblems concurrently via `Future` and Cats Effect `IO`.
- Used in `merge.scala` for standard sequence merges and in `SectionedCodeExtension` for local clump-level merges.

### `SectionedCodeExtension`
- Extension method `merge` on `SectionedCode`:
  1. `longestCommonSubsequenceOf(path)`: Extension method on `SectionedCode` that constructs the two-tier approximate
     LCS per file path using block-level merges, clump coalescence, local section merges, and crossover filtering
     (`matchesCross`).
  2. Initial per-path merge pass using `FirstPassMergeResult.mergeAlgebra`, recording merge operations and identifying
     speculative move sources and destinations.
  3. Evaluates global moves via `MoveDestinationsReport.evaluateSpeculativeSourcesAndDestinations`.
  4. Plays back operations into `secondPassMergeResultsByPath` using `ConflictResolvingMergeAlgebra`.
  5. Generates and applies substitutions (change migrations) and splices (splice migrations grouped by
     `ParallelMatchesGroupId`).
  6. In `resolveSections`, final unpicking transforms sections to content via `innerFlatMap(resolution)`.

### `MergeAlgebra` & `ConflictResolvingMergeAlgebra`
- An object algebra defining how the three-way merge algorithm handles preservation, insertions, deletions, edits, and
  conflicts across sides.
- `FirstPassMergeResult`: Accumulates speculative moves and records merge algebra steps.
- `ConflictResolvingMergeAlgebra`: Replays recorded steps, suppressing moved source sections and coincident moves in
  their original locations so they can be migrated cleanly to move destinations.

### `MergeResult`
- Algebraic data type representing merge outcomes:
  - `FullyMerged[Element]`: Cleanly merged sequence of elements.
  - `MergedWithConflicts[Element]`: Conflicted result with distinct `leftElements` and `rightElements` (and optional
    `baseElements`).
- Supports nested results, allowing intermediate splices and substitutions to be embedded and resolved.

### `MoveDestinationsReport`
- Analyzes speculative move sources and destinations across all paths.
- Distinguishes between coincident moves (moved to both left and right to matching locations), divergent moves (moved to
  conflicting locations), and one-sided moves.
- Produces `MoveEvaluation` containing surviving anchored moves, substitution mappings, and edit suppressions.
