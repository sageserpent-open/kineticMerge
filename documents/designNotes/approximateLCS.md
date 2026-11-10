# Two-Tier Approximate Longest Common Subsequence

In a three-way merge, finding the **Longest Common Subsequence (LCS)** across the base, left, and right versions of a
file forms the backbone of the merge. Sections that align in the LCS represent stationary, preserved code; sections
that do not align indicate edits, insertions, deletions, or code motion.

---

## 1. The Scalability Challenge of Exact 3D LCS

Computing the exact 3-way LCS between three sequences of lengths $L_{\text{base}}, L_{\text{left}}, L_{\text{right}}$
using classic dynamic programming requires a 3D grid with $O(L_{\text{base}} \times L_{\text{left}} \times L_{\text{right}})$
states.

While optimizations like prefix/suffix trimming and swathe-based memory bounding help for moderate sequences, running
exact 3D dynamic programming directly over hundreds or thousands of fine-grained sections (or tokens) for realistic
source files:
1. Causes high memory and CPU consumption.
2. Experiences severe stalls when large blocks of code are reordered or moved.
3. Can yield unnatural or fragmented alignments when multiple ambiguous small matches exist.

To solve this, Kinetic Merge employs a **Two-Tier Approximate LCS** architecture implemented in
`SectionedCodeExtension.longestCommonSubsequenceOf`.

---

## 2. Overview of the Two-Tier Architecture

The two-tier construction works by:
1. **Tier 1 (Block Level)**: Coarsely aligning files using `Block`s (which represent entire parallel matches groups or
   filler gaps).
2. **Clump Coalescence**: Expanding blocks into section clumps and coalescing overlapping intervals.
3. **Tier 2 (Section Level)**: Performing localized, exact 3-way merges only within each individual clump.
4. **Disjoint Sets Unification & Crossover Resolution**: Unifying section matches across clumps and eliminating any
   crossing matches (`matchesCross`).
5. **Backbone Contribution Assignment**: Classifying sections into `Common`, `CommonToBaseAndLeftOnly`,
   `CommonToBaseAndRightOnly`, `CommonToLeftAndRightOnly`, and `Difference`.

```
File Sections (base, left, right)
               │
               ▼
Partition into Blocks (matched & filler)
               │
               ▼
[Tier 1] 3-Way Merge of Blocks  ──>  Three-Sided Clumps of Blocks
               │
               ▼
Expand & Coalesce Overlapping Clumps  ──>  Non-Overlapping Coalesced Section Clumps
               │
               ▼
[Tier 2] Local 3-Way Merges of Sections within Clumps  ──>  Raw Match Sequence
               │
               ▼
Disjoint Sets Unification & Crossover Resolution (`matchesCross`)
               │
               ▼
Assign Backbone Contributions  ──>  Final LongestCommonSubsequence
```

---

## 3. Tier 1: Block-Level Merge

For a given file path:
- `baseBlocks`, `leftBlocks`, and `rightBlocks` are retrieved from `SectionedCode`. Each file is completely covered by
  non-overlapping blocks.
- Blocks are compared using an `Order[Block[Element]]` that distinguishes between matched blocks (which compare by their
  associated parallel matches groups and content) and filler blocks.
- A block-level merge is executed:
  ```scala
  val threeSidedClumps: ThreeSidedClumps[Block[Element]] =
    mergeOf(blockLevelMergeAlgebra)(baseBlocks, leftBlocks, rightBlocks, label = "Blocks merged:")
  ```
- Because a file typically breaks down into only dozens of blocks rather than thousands of sections, this step executes
  in milliseconds.
- The outcome is a sequence of `ThreeSidedClump[Block[Element]]`, where each clump pairs corresponding blocks across
  base, left, and right (or empty sequences where a side does not contribute).

---

## 4. Expanding & Coalescing Section Clumps

Blocks within each clump are expanded back into their constituent sections:
```scala
val sectionClumps: Vector[ThreeSidedClump[Section[Element]]] =
  threeSidedClumps.map { clump =>
    ThreeSidedClump(
      base = clump.base.flatMap(_.sectionsCoveredByGroup).distinct,
      left = clump.left.flatMap(_.sectionsCoveredByGroup).distinct,
      right = clump.right.flatMap(_.sectionsCoveredByGroup).distinct
    )
  }
```

Because blocks on different sides can disagree on filler boundaries or span slightly different section offsets, adjacent
section clumps may overlap in start/end offsets.

Kinetic Merge sweeps through `sectionClumps`, merging and coalescing any adjacent clumps whose section intervals
overlap on *any* side (`nonOverlappingCoalescedSectionClumps`). This guarantees that each resulting clump is strictly
disjoint from every other clump across all three sides.

---

## 5. Tier 2: Local Section-Level Merge

Within each coalesced clump, the base, left, and right section sequences are merged using
`sectionLevelMergeAlgebraExtractingAlignedMatchesOnly`:
- Preservations across all three sides yield `Match.AllSides`.
- Pairwise survivals yield `Match.BaseAndLeft`, `Match.BaseAndRight`, or `Match.LeftAndRight`.
- Differences and conflicts are ignored at this stage (they do not contribute aligned matches).

Because each clump is small and localized, running `mergeOf` here is fast, avoids whole-file dynamic programming grids,
and confines alignment decisions to locally related code.

---

## 6. Unification and Crossover Filtering

The matches produced across all clumps are concatenated into a candidate `matchSequence`. However, because clumps are
merged locally, rival matches may share sections or cross over one another across sides.

### Disjoint Sets Unification
Matches that share a section on at least one side are grouped into equivalence classes using Cats `DisjointSets`:
- If match $M_1$ shares a left section with $M_2$, they are unified into the same equivalence class.
- From each class, `representativeMatchesFrom` selects the best representative:
  - All-sides matches are prioritized over pairwise matches.
  - If an all-sides match has sections that conflict with earlier selections, it is demoted to the surviving pairwise
    components.

### Crossover Detection (`matchesCross`)
A fundamental rule of monotonic subsequence alignment is that matches must not cross: if match $M_1$ appears before
$M_2$ on the base side, it must not appear *after* $M_2$ on the left or right side.

Kinetic Merge detects crossovers across all three pairs of sides:
```scala
def matchesCross(m1: Match[Section[Element]], m2: Match[Section[Element]]): Boolean =
  val baseLeftCross = ...
  val baseRightCross = ...
  val leftRightCross = ...
  Seq(baseLeftCross, baseRightCross, leftRightCross).flatten.exists(identity)
```

To resolve crossovers:
1. Candidate matches are sorted into canonical order using their start offsets across sides.
2. A sequence-level LCS (`LongestCommonSubsequence.of(baseMatches, leftMatches, rightMatches)`) is executed over the
   matches themselves (weighted by match size).
3. The common matches extracted from this meta-LCS are guaranteed to be strictly non-crossing and monotonic across all
   sides.

---

## 7. Backbone Contribution Assignment

With the surviving `bestMatches` in hand, each section in the file is mapped to its `Contribution`:
- Sections in `Match.AllSides` $\longrightarrow$ `Contribution.Common`
- Sections in `Match.BaseAndLeft` $\longrightarrow$ `Contribution.CommonToBaseAndLeftOnly`
- Sections in `Match.BaseAndRight` $\longrightarrow$ `Contribution.CommonToBaseAndRightOnly`
- Sections in `Match.LeftAndRight` $\longrightarrow$ `Contribution.CommonToLeftAndRightOnly`
- All remaining unmatched sections $\longrightarrow$ `Contribution.Difference`

The final result is wrapped in `LongestCommonSubsequence(base, left, right)` for the path, providing the exact backbone
consumed by `FirstPassMergeResult` and the code motion detection engine.
