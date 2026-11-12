# Match Analysis & Sectioned Code Construction

In earlier versions of Kinetic Merge, match analysis and code breakdown were combined in a single monolith
(`CodeMotionAnalysis`). In the current codebase, the responsibility is split into:

1. `MatchAnalysis`: The iterative discovery, thinning, overlap fragmentation, and subsumption reconciliation of
   matches across three sides.
2. `SectionedCode`: The final breakdown of files into contiguous sections, grouping of parallel matches and
   construction of blocks for alignment.

---

## The End-to-End Pipeline in `SectionedCode.of`

The factory method `SectionedCode.of` executes the following sequence of transformations:

```scala
val withAllMatchesOfAtLeastTheSureFireWindowSize =
  MatchAnalysis.of(baseSources, leftSources, rightSources)(configuration)

val withAllMatchesOfAtLeastTheMinimumWindowSize =
  withAllMatchesOfAtLeastTheSureFireWindowSize.withAllSmallFryMatches()

val withTinyMatchesIncluded =
  withAllMatchesOfAtLeastTheMinimumWindowSize.withTinyMatches

val parallelMatchesOnly =
  withTinyMatchesIncluded.parallelMatchesOnly

val withOverlapsReconciled =
  parallelMatchesOnly.reconcileOverlappingMatches(
    reconcileMatchesInvolvingOverlappingSections
  )

val matchesAndTheirSections =
  withOverlapsReconciled.reconcileSubsumingMatches
```

Let's examine each phase in detail.

---

## 1. Match Discovery: Binary Search and Linear Scan

Match discovery identifies identical token sequences across sides using rolling polynomial hashes (`RollingHash`).
To prevent match explosions and maintain efficiency, discovery progresses downwards from larger window sizes to
smaller ones:

### The Sure-Fire Threshold

A configuration parameter, `thresholdSizeFractionForMatching`, specifies a minimum fraction of a file's size that a
match must cover. This implies a *minimum sure-fire window size*—the smallest window size that is guaranteed not to be
rejected by the threshold in *any* file across the codebase.

### Iterated Binary Search (`MatchAnalysis.of`)

For window sizes above the minimum sure-fire window size, candidate window sizes can be searched safely using binary
search without risking partial threshold rejections:

- The binary search evaluates a candidate window size $W \in [W_{low}, W_{high})$, starting with $W_{low}$ set to the
  initial sure-fire minimum.
- For that window size, `matchesForWindowSize` fingerprints all files and identifies matching token sequences across
  sides. It returns discovered matches along with an estimate of the maximum attainable match size based on the latest
  discovered matches.
- If no matches are found, the search lowers the upper bound $W_{high} \leftarrow W$.
- If matches are found but the estimate indicates larger potential matches exist, the search raises the lower bound $W_
  {low} \leftarrow 1 + W$.
- If optimal matches are found at $W$, the matches are stored, and the binary search restarts
  with $W_{high} \leftarrow W$
  and $W_{low}$ reset to the initial sure-fire minimum.
- This continues until all surefire matches are extracted.

### Linear Scan for Small-Fry Matches (`withAllSmallFryMatches`)

Below the sure-fire window size, files may independently accept or reject candidate matches based on their individual
sizes. Binary search cannot be used here because missing a match at size $W$ does not guarantee that a valid match does
not exist at $W^{'} > W$ in a smaller file.

Therefore, `withAllSmallFryMatches` linearly steps downward through all integer window sizes from the sure-fire size
down to `configuration.minimumMatchSize`. Because all larger matches have already been registered, any match found
during this downward scan is immediately maximal.

### Tiny Matches (`withTinyMatches`)

Even below `minimumMatchSize`, tiny matches (down to window size 1) can occur. `withTinyMatches` identifies these small
matches. They are not used as standalone merge anchors; rather, they are used to fragment gap-filling sections in
`filesByPathUtilising`, preventing small unchanged code snippets inside edits from being misaligned.

---

## 2. Meta-Matching: `parallelMatchesOnly`

Raw match discovery produces many trivial ambiguous matches (e.g. repeated keywords, common boilerplate, or
isolated lines). These would overwhelm subsequent processing without contributing any value; they are noise.

To filter out this noise and retain only coherent runs of code motion, Kinetic Merge applies **meta-matching**:

1. All matched sections on each side are collected and sorted in order of appearance in their containing files.
2. These sequences of sections are treated as synthetic 'files' where each element is an entire `Section`. The content
   gaps in the original files between the matched sections don't appear at all.
3. `MatchAnalysis.of` is executed over these synthetic sources (*meta-base*, *meta-left* and *meta-right*).
4. Because the elements are sections, this meta-matching pass finds sequences of *matched* sections that appear in the
   exact same relative order across sides.
5. The resulting meta-matches are back-translated into original matches (`backTranslatedParallelMatches`), replacing the
   original matches.
6. This process generates redundant pairwise matches that are cleaned up via `cleanUpPairwiseMatches`.

---

## 3. Reconciling Overlapping Matches (`reconcileOverlappingMatches`)

A fundamental invariant of `SectionedCode` is that on any given side and path, all sections must be contiguous and
disjoint - neither overlaps nor subsumption is permitted. However, match discovery may produce overlapping matches where
rival matches contest the same token span.

`reconcileOverlappingMatches` resolves overlaps using a tail-recursive fragmentation strategy, where in each recursive
step, every match is checked for overlaps against the rest of the matches in that step, hiving off a non-overlapping
fragment if necessary and leaving remaining overlaps to be whittled down further. Any match that overlaps is removed
from the working set of matches and replaced by the fragment and overlaps for the next recursion step.

---

## 4. Reconciling Subsuming Matches (`reconcileSubsumingMatches`)

Often, a large pairwise match subsumes a smaller all-sides matches within its interior.

This a valid situation (unlike having a larger match subsuming a smaller match of the same kind, which is forbidden
during match discovery) - but in order to preserve the invariant, reconciliation has to take place. Dropping the larger
pairwise match leads to a poorer merge where only the localised all-sides matches feature, and dropping the all-sides
matches can lead to poor merges where, say, a base and left match overlaps a base and right match with the common ground
being covered by an all-sides match subsumed by both pairwise matches. What we want is to cover all of that content with
matches, but keep them disjoint.

`reconcileSubsumingMatches` resolves this:

1. It identifies pairwise matches that subsume one or more all-sides matches.
2. It iteratively "eats into" or fragments the larger pairwise match around the embedded all-sides matches.
3. The pairwise match is replaced by smaller pairwise fragments that abut the all-sides matches before and after.
4. Redundant pairwise matches that are completely shadowed across all sides by all-sides matches are removed
   (`cleanUpPairwiseMatches`).
5. The reconciliation iterates until the set of matches stabilises.

---

## 5. Groups of Parallel Matches & Block Construction

Once all matches are reconciled, `matchesAndTheirSections` satisfies the strong postcondition:

- Every participating section is unique and non-overlapping.
- Pairwise and all-sides matches are cleanly partitioned.

From here, `SectionedCode.of`:

1. Constructs `baseFilesByPath`, `leftFilesByPath`, and `rightFilesByPath` by calling
   `filesByPathUtilising(mandatorySections)`.
   Any remaining token gaps between matched sections are filled with contiguous filler sections.
2. Computes `groupsOfParallelMatches` by unifying parallel all-sides matches, pairwise matches, and bridging pairs.
3. Groups the sections for each file into `Block`s (`baseBlocksFor`, `leftBlocksFor`, `rightBlocksFor`), condensing
   coincident blocks and eliminating nested blocks to form the foundation of the approximate LCS provided by
   `SectionedCodeExtension`.
