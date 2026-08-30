# Kinetic Merge Benchmark Conflict Analysis Report

## Executive Summary
During the manual benchmark procedure documented in `AGENTS.md` (merging commit `55fef2f785334b7bf99fc592dd101a462cbb2c6d` into benchmark repository state at commit `6e0b6821fa8fe53465c3355793b61ac425a10c1a`), Kinetic Merge reports a merge conflict in `src/main/scala/com/sageserpent/kineticmerge/Main.scala`.

A comparative investigation between Kinetic Merge built prior to commit `399aafdd` (at commit `7167586`) and Kinetic Merge post commit `399aafdd` (PR #408: Fuse parallel matches groups) reveals the exact mechanism behind the difference in merge quality:

1. **At commit `7167586`**, Kinetic Merge produces a **clean merge** of `Main.scala` (detecting 50 single section moves in `Main.scala`).
2. **At commit `399aafdd` (PR #408)**, Kinetic Merge reports a **merge conflict** in `Main.scala` (detecting 46 single section moves).

---

## Detailed Technical Analysis & PR #408 Mechanism

### 1. Role and Necessity of Parallel Group Fusion
Group fusion in `MatchesAndTheirSections.withoutRedundantPairwiseMatches` was introduced in PR #408 (`399aafdd`) to unify parallel matches groups across sides and prevent "crossed-over matches" (as tested by `SectionedCodeTest.reproduceCrossedOverMatches`).
Disabling group fusion breaks `SectionedCodeTest.reproduceCrossedOverMatches`, proving that group fusion is essential for Kinetic Merge to maintain a non-crossing match invariant.

### 2. How Group Fusion Works on Benchmark Scenario (Fusion #13)
In the benchmark run on `Main.scala`:
- **Pairwise Group 727** contains two `BaseAndRight` pairwise matches:
  1. `Main.scala` line 2298 (`if theirModificationWasTweakedByTheMerge ...`)
  2. `Main.scala` line 2299 (`then for _ <- ...`)
- **All-Sides Group 4490** contains a single `AllSides` match:
  1. `Main.scala` line 2298 (`if theirModificationWasTweakedByTheMerge ...`)
  *(Line 2299 was modified on "Theirs" side, so line 2299 has only a `BaseAndRight` pairwise match).*

When `withoutRedundantPairwiseMatches` evaluates Group 727:
1. Line 2298's pairwise match is recognized as redundant with Group 4490's `AllSides` match and is removed.
2. Line 2299's pairwise match is non-redundant and remains.
3. Because line 2298's redundant match pointed to Group 4490, Group 727's ID cutover `727 -> 4490` is applied.
4. Line 2299's non-redundant pairwise match is brought into Group 4490 alongside line 2298's `AllSides` match.

---

## Downstream Mechanism: Block Synthesis and Section-Level LCS

### 1. Block Synthesis in `SectionedCode.of`
When `SectionedCode` synthesizes `Block` instances for block-level Longest Common Subsequence (LCS) alignment:
- Contiguous sections sharing a `ParallelMatchesGroupId` are merged into a single block.
- On **Base** and **Right**, lines 2298 and 2299 are contiguous and both belong to Group 4490, so they form a single combined 2-line block for Group 4490.
- On **Left**, line 2299 is not part of Group 4490 (having been modified on "Theirs"). Thus, the block for Group 4490 on Left covers **only line 2298**, while line 2299 is placed in an adjacent filler/difference block.

### 2. Block-Level LCS Alignment and Contribution Explosion
- The 3-way block-level LCS aligns the combined block on Base and Right with the single-line block on Left because `laxMatchesFrom` filters out pairwise matches when `AllSides` matches are present in a group during key comparison.
- However, during section-level LCS / contribution assignment, the block-level merge algebra (`blockLevelMergeAlgebra`) treats the aligned blocks as a preservation/coincident block for Group 4490.
- Because line 2299 on Base/Right was absorbed into Group 4490's block rather than being in a separate filler block, the section-level LCS does not recognize line 2299 on Left as a left-edit of the pairwise match on Base/Right.
- Consequently, the pairwise contribution (`CommonToBaseAndRightOnly`) for line 2299 is suppressed, preventing section-level edit migration for line 2299 to land at line 1440 on "Ours", which results in the merge conflict in `Main.scala`.

---

## Conclusion & Recommendations

1. **Group Fusion is Correct & Intended**:
   Bringing non-redundant pairwise matches into fused parallel match groups is correct design intended to resolve crossed matches.

2. **Downstream Refinement Needed**:
   When an aligned block contains a mixture of `AllSides` matches and pairwise matches (due to group fusion), block explosion / section-level LCS needs to explicitly preserve the pairwise match contributions (`CommonToBaseAndRightOnly`, `CommonToBaseAndLeftOnly`, `CommonToLeftAndRightOnly`) alongside `Common` contributions, allowing adjacent edit migration on the un-matched side to function correctly.
