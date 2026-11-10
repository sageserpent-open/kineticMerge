# Modelling of the Problem Space #

## Sources, Sides, Sections, and Tokens ##

Kinetic Merge abstracts merging away from version control systems. It operates on three **sources**:
1. `base`: The common ancestor version.
2. `left`: "Our" branch/tree.
3. `right`: "Their" branch/tree.

Each source contains a mapping from file paths (`Path`) to file contents. In the core library, path types and content
element types are generalized (`Path`, `Element`), allowing thorough unit testing with integer paths or simple
characters, while production CLI applications work with filesystem paths and `Token` instances.

### Tokens and Whitespace Insensitivity

Source files are tokenized upfront into sequences of `Token`s (`core.Token`). Tokens demarcate identifiers, numbers,
keywords, operator symbols, punctuation, or string literals. Whitespace is folded into adjacent tokens: in general,
whitespace is attached as a suffix of the preceding token, but leading indentation on a line is attached as a prefix
to make the tokens more natural to read.

When comparing tokens for equality (e.g. during hashing or dynamic programming), the attached whitespace is ignored,
delivering natural **whitespace insensitivity**. Only standalone leading whitespace at the very start of a file is
compared explicitly against other whitespace tokens. String literals are treated as atomic tokens (preserving internal
formatting and whitespace).

### Sections and Files

A file in a source is covered by an indexed sequence of contiguous, non-overlapping **sections** (`Section[Element]`).
A section represents a half-open interval `[startOffset, onePastEndOffset)` of token offsets into the file. Sections do
not have to align with newline boundaries; they can span parts of a line, full lines, or multiple functions.

Crucially:
- A `Sources` instance is a factory for sections and maps sections back to their file paths.
- Sections are compared by their defining attributes (`path`, `startOffset`, `size`), *not* by object identity or
  content equality. Two sections with identical text on different sides are distinct objects.
- A `File[Element]` is a synthetic container whose total content is formed by the concatenation of its contiguous
  constituent sections.

## Matches and Parallel Matches Groups ##

Match analysis (`core.MatchAnalysis`) searches for equivalent swathes of tokens across sides:
- **All-Sides Match (`Match.AllSides`)**: A three-way match covering a section in `base`, a section in `left`, and a
  section in `right`.
- **Pairwise Match**: A two-way match covering sections on two sides: `Match.BaseAndLeft`, `Match.BaseAndRight`, or
  `Match.LeftAndRight`.

Matches are discovered working downward in window sizes using rolling hashes. Once discovered, matches are reconciled
to resolve overlaps and eliminate redundancies.

### Parallel Matches Groups

Individual matches are not isolated; they frequently occur in sequences where successive matches move together in
parallel (for example, several functions moved together from one file to another, interspersed with minor local edits).

Kinetic Merge unifies such correlated matches into **parallel matches groups**, identified by a unique
`ParallelMatchesGroupId`.
1. Parallel sequences of all-sides matches are unified across all three sides.
2. Parallel sequences of pairwise matches are unified across their participating sides.
3. Bridges between all-sides groups and pairwise groups are established when they align continuously, including
   "sandwich" structures where a pairwise match fills an edit or deletion between all-sides matches.
4. Matches within each parallel matches group must strictly march upward in start offsets on all shared sides without
   collisions.

## Blocks: Partitioning Files for Scalable Alignment ##

In large codebases, running a full three-dimensional longest common subsequence (LCS) algorithm over individual
sections or tokens across all three versions of a file is computationally prohibitive ($O(N^3)$ complexity).

To make alignment fast and scalable, Kinetic Merge groups sections into higher-level **blocks** (`SectionedCode.Block`):
- A block covers a contiguous sequence of sections in a file spanned by a single parallel matches group.
- Intervening gaps between matched blocks are covered by **filler blocks** (which have empty parallel match group IDs).
- Blocks covering the same file content due to multiple parallel matches groups are coalesced together.
- All following nested blocks are suppressed, while overlapping blocks spanning different ranges are permitted.

## Two-Tier Approximate Longest Common Subsequence ##

Instead of running LCS directly on thousands of fine-grained sections across the entire file, Kinetic Merge uses a
**two-tier approximate LCS construction** (`SectionedCodeExtension.longestCommonSubsequenceOf`):

1. **Tier 1: Block-Level 3-Way Merge**:
   The coarse-grained blocks (`baseBlocks`, `leftBlocks`, `rightBlocks`) are merged using a standard 3-way merge algebra.
   Because blocks condense large runs of parallel matches into single units, this block-level merge executes almost
   instantaneously. The merge yields a sequence of **three-sided clumps** (`ThreeSidedClump[Block]`).
2. **Clump Coalescence**:
   The blocks within each three-sided clump are expanded back into their constituent sections. Adjacent clumps whose
   section spans overlap across any side are coalesced together to maintain ordering consistency.
3. **Tier 2: Local Section-Level Merging**:
   Within each coalesced three-sided clump, an exact section-level 3-way merge is performed. Because each clump covers
   only a small, localized region of the file, this step is fast and bounded in size.
4. **Disjoint Sets Unification and Crossover Filtering**:
   The putative alignments discovered by the local section-level merges are represented as temporary `Match` instances
   and concatenated into a candidate sequence. These temporary alignment matches are distinct from the matches found
   globally during match analysis: they represent tentative alignments within the local clumps.
   Temporary matches that share a section on at least one side are unified via `DisjointSets`, and the best
   representative alignment is chosen from each class (prioritizing all-sides alignments). Any crossover anomalies
   (where alignments jump backward or cross relative to another alignment across sides) are resolved via a meta-LCS,
   strictly eliminating crossing matches (`matchesCross`).
5. **Backbone Contribution Assignment**:
   The surviving aligned matches define the backbone `LongestCommonSubsequence` for the file:
   - Sections aligned across all three sides become `Contribution.Common`.
   - Sections aligned across two sides become `Contribution.CommonToBaseAndLeftOnly`,
     `Contribution.CommonToBaseAndRightOnly`, or `Contribution.CommonToLeftAndRightOnly`.
   - Unmatched sections become `Contribution.Difference`.

## Identifying Code Motion and Performing Migrations ##

The combination of global match analysis and per-file LCS alignment cleanly separates stationary code from moved code:

- **Stationary Preservations**: Sections that appear in the base and align with their corresponding sections in the
  same file's LCS on both left and right are stationary.
- **Code Motion (Moves)**: When a section in the base has matched content on the left or right, but does *not* align in
  the per-file LCS (or appears at a completely different file path), it has **moved**.
  - **Intra-file move**: Moved to a different offset within the same file.
  - **Inter-file move**: Moved to a file at a different path.
- **Change Migration**: If code moves from the base to a destination on one side, and the opposite side edits or deletes
  that code at its original location, Kinetic Merge migrates the edit or deletion to the move destination.
- **Splice Migration**: If new content is inserted or edited adjacent to the source of a move, Kinetic Merge performs a
  three-way splice merge and migrates the resulting splice adjacent to the moved section at its destination.
- **Recursive Unpicking**: Migrated edits and splices are nested into the merge result and resolved via bottom-up
  recursive flattening, preserving local conflict boundaries.
