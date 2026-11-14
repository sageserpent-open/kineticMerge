# Two-Tier Approximate Longest Common Subsequence

In a three-way merge, finding the LCS across the base, left, and right versions of a file forms the backbone of the
merge; that in turn not only performs the basic merge activity, but also leads to discovery of code motion.

---

## 1. The Scalability Challenge of Exact 3D LCS

Computing the exact LCS between three sequences of lengths $L_{\text{base}}, L_{\text{left}}, L_{\text{right}}$ using
classic dynamic programming requires a 3D grid with $O(L_{\text{base}} \times L_{\text{left}} \times L_{\text{right}})$
states.

While optimisations like prefix/suffix trimming and swathe-based memory bounding help for moderate sequences, running
exact 3D dynamic programming directly over hundreds or thousands of fine-grained sections (or tokens) for realistic
source files suffers from a combinatoric explosion, running the risk of memory exhaustion and impracticably long
computation time.

To solve this, Kinetic Merge employs a *two-tier approximate LCS* algorithm in
`SectionedCodeExtension.longestCommonSubsequenceOf`.

---

## 2. Overview of the Two-Tier Architecture

The two-tier construction works by:

1. **Tier 1 (Block Level)**: coarsely aligning files using `Block`s (which represent entire parallel matches groups or
   filler gaps).
2. **Clump coalescence**: expanding blocks into section clumps and coalescing overlapping intervals.
3. **Tier 2 (Section Level)**: performing localized, exact 3-way merges only within each individual clump.
4. **Disjoint sets unification & crossover resolution**: unifying section matches across clumps and eliminating any
   crossing matches (`matchesCross`).
5. **Backbone contribution assignment**: classifying sections into `Common`, `CommonToBaseAndLeftOnly`,
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
  blocks.
- Blocks are compared using an `Order[Block[Element]]` that distinguishes between matched blocks and filler blocks.
- A block-level merge is executed (which uses the exact LCS under the hood, but at block-level):
  ```scala
  val threeSidedClumps: ThreeSidedClumps[Block[Element]] =
    mergeOf(blockLevelMergeAlgebra)(baseBlocks, leftBlocks, rightBlocks, label = "Blocks merged:")
  ```
- Because a file typically breaks down into only dozens of blocks rather than thousands of sections, this step executes
  quickly.
- The outcome is a sequence of `ThreeSidedClump[Block[Element]]`, where each clump associates groups of corresponding
  blocks across the base, left, and right.

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

Adjacent section clumps may overlap in start/end offsets: a sweep is made through `sectionClumps`, merging and
coalescing any adjacent clumps whose section intervals overlap on *any* side (`nonOverlappingCoalescedSectionClumps`).
This guarantees that each resulting clump is strictly disjoint from every other clump across all three sides.

---

## 5. Tier 2: Local Section-Level Merge

Within each coalesced clump, the base, left, and right section sequences are merged using
`sectionLevelMergeAlgebraExtractingAlignedMatchesOnly`:

- Preservations across all three sides yield `Match.AllSides`.
- One-sided edits or deletions yield `Match.BaseAndLeft`, `Match.BaseAndRight`.
- Coincident edits or insertions yield `Match.LeftAndRight`.
- Other outcomes are ignored at this stage (they do not contribute aligned matches).

Because each clump is small and localized, running `mergeOf` is fast, avoiding whole-file dynamic programming grids.

---

## 6. Unification and Crossover Filtering

The matches produced across all clumps are concatenated into a candidate `matchSequence`; they are section-level
alignments that will lead to the final LCS.

The LCS has a consistency invariant that is policed by a precondition in `LongestCommonSubsequencea.apply`. To meet this
precondition, some cleaning up of the matches has to be done.

### Disjoint Sets Unification

Matches that share a section on at least one side are grouped into equivalence classes using Cats `DisjointSets`:

- If match $M_1$ shares a left section with $M_2$, they are unified into the same equivalence class.
- From each class, `representativeMatchesFrom` selects the best representative:
    - All-sides matches are prioritised over pairwise matches.
    - If an all-sides match has sections that conflict with earlier selections, it is demoted to the surviving pairwise
      components.

### Crossover Detection (`matchesCross`)

For the matches to be true alignments, they must not cross over: if match $M_1$ appears before $M_2$ on one side, it
must not appear *after* $M_2$ on the other sides.

The algorithm's steps try (quite effectively, it turns out) to avoid these crossovers, policing for crossovers across
all three pairs of sides:

```scala
def matchesCross(m1: Match[Section[Element]], m2: Match[Section[Element]]): Boolean =
  val baseLeftCross =
...
val baseRightCross =
...
val leftRightCross =
...
Seq(baseLeftCross, baseRightCross, leftRightCross).flatten.exists(identity)
```

However, a few occasionally slip through; to deal with these:

1. Candidate matches are sorted into canonical order using their start offsets across sides.
2. An exact LCS (`LongestCommonSubsequence.of(baseMatches, leftMatches, rightMatches)`) is executed over the remaining
   alignment matches after unification. This LCS is very efficient, because only the crossover minority are not valid in
   an LCS, so the optimisations on the exact LCS computation kick in.
3. The common matches extracted from this match-level LCS are guaranteed not to cross over.

---

## 7. Backbone Contribution Assignment

With the surviving `bestMatches` in hand, each section in the file is mapped to its `Contribution`:

- Sections in `Match.AllSides` $\longrightarrow$ `Contribution.Common`
- Sections in `Match.BaseAndLeft` $\longrightarrow$ `Contribution.CommonToBaseAndLeftOnly`
- Sections in `Match.BaseAndRight` $\longrightarrow$ `Contribution.CommonToBaseAndRightOnly`
- Sections in `Match.LeftAndRight` $\longrightarrow$ `Contribution.CommonToLeftAndRightOnly`
- All remaining unmatched sections $\longrightarrow$ `Contribution.Difference`

The final result is wrapped in `LongestCommonSubsequence(base, left, right)` for the path, being subject to the
precondition check that the LCS is valid. Merging can then use this approximate LCS as a precomputed input.
