# Kinetic Merge Benchmark Conflict Analysis Report

## Executive Summary
During the manual benchmark procedure documented in `AGENTS.md` (merging commit `55fef2f785334b7bf99fc592dd101a462cbb2c6d` into benchmark repository state at commit `6e0b6821fa8fe53465c3355793b61ac425a10c1a`), Kinetic Merge reports a merge conflict in `src/main/scala/com/sageserpent/kineticmerge/Main.scala`.

A comparative investigation between Kinetic Merge built prior to commit `399aafdd` (at commit `7167586`) and Kinetic Merge post commit `399aafdd` (PR #408) reveals the exact mechanism behind the difference in merge quality:

1. **At commit `7167586`**, Kinetic Merge produces a **clean merge** of `Main.scala` (detecting 50 single section moves in `Main.scala`).
2. **At commit `399aafdd` (PR #408: Fuse parallel matches groups)**, Kinetic Merge reports a **merge conflict** in `Main.scala` (detecting 46 single section moves).

---

## Detailed Technical Analysis & PR #408 Mechanism

### 1. Role of Group Fusion & `SectionedCodeTest.reproduceCrossedOverMatches`
Group fusion in `MatchesAndTheirSections.withoutRedundantPairwiseMatches` was introduced in PR #408 (`399aafdd`) to resolve "crossed-over matches" (as tested by `SectionedCodeTest.reproduceCrossedOverMatches`). When pairwise matches (such as `BaseAndRight`) share sections with an `AllSides` match, group fusion re-assigns the pairwise group's `ParallelMatchesGroupId` to the corresponding `AllSides` match group ("groupIdCutovers"). This prevents crossed matches by unifying parallel alignments.

### 2. Why the Guard Code `1 == candidateReplacementGroupIds.size` Passes
In `withoutRedundantPairwiseMatches`, the candidate cutovers are filtered with:
```scala
accumulatedGroupIdCandidateCutovers.sets
  .collect {
    case (groupId, candidateReplacementGroupIds)
        if 1 == candidateReplacementGroupIds.size =>
      groupId -> candidateReplacementGroupIds.head
  }
```
The guard checks whether all *redundant* pairwise matches in `groupId` point to a single target `AllSides` group ID.
However, **it does NOT check whether the pairwise match group contains additional, NON-REDUNDANT pairwise matches!**

### 3. The Specific Benchmark Fusion Scenario (Fusion #13)
In the benchmark run on `Main.scala`:
- **Pairwise Group 727** contains two `BaseAndRight` pairwise matches:
  1. `Main.scala` line 2298 (`if theirModificationWasTweakedByTheMerge ...`)
  2. `Main.scala` line 2299 (`then for _ <- ...`)
- **All-Sides Group 4490** contains a single `AllSides` match:
  1. `Main.scala` line 2298 (`if theirModificationWasTweakedByTheMerge ...`)
  *(Line 2299 was modified on "Theirs" side, so line 2299 only has a pairwise match between Base and Right).*

When `withoutRedundantPairwiseMatches` evaluates Group 727:
1. Line 2298's pairwise match is recognized as redundant with Group 4490's `AllSides` match and is removed.
2. Line 2299's pairwise match is **not** redundant with any `AllSides` match (so it remains in `withoutRedundantMatches`).
3. Because line 2298's redundant match was the only redundant match evaluated for Group 727, `candidateReplacementGroupIds` for Group 727 contains only `Set(4490)` (`size == 1`).
4. The guard passes! Group 727's ID cutover `727 -> 4490` is applied to **all remaining matches in `parallelMatchesGroupIdsByMatch`**.
5. Consequently, line 2299's non-redundant pairwise match is re-assigned to Group 4490.

---

## Downstream Percolation: From Group Fusion to Block Alignment Failure

### 1. Asymmetric Group Composition
After Fusion #13, **Group 4490** contains:
- An `AllSides` match for line 2298 (covering Base, Left, and Right).
- A `BaseAndRight` pairwise match for line 2299 (covering Base and Right, but **NOT** Left).

### 2. Block Key Calculation in `SectionedCode` / `SectionedCodeExtension`
When `SectionedCode` constructs `Block` instances for block-level Longest Common Subsequence (LCS) alignment:
- Blocks are created for each `ParallelMatchesGroupId`.
- For Group 4490, the block on **Base** and **Right** covers lines 2298–2299 (size = 61 tokens).
- The block on **Left** covers line 2298 (size = 2 tokens), because line 2299 lacks a match on Left.

When `SectionedCodeExtension` evaluates `Order[Block[Element]]` during block-level LCS dynamic programming:
- Comparison keys (`blockKeys`) are pre-calculated for each side by concatenating match contributions in the block.
- Base block key for Group 4490: `[token sequence for line 2298 + line 2299]`
- Left block key for Group 4490: `[token sequence for line 2298]`
- Right block key for Group 4490: `[token sequence for line 2298 + line 2299]`

### 3. LCS Alignment Drop & Un-Migrated Conflict
Because the Left block key for Group 4490 differs significantly from the Base and Right block keys (2 tokens vs 61 tokens), **the 3-way block-level LCS fails to align Group 4490 between Base and Left**.

- As a result, line 2298 (`if theirModificationWasTweakedByTheMerge ...`) is dropped from the block-level LCS backbone.
- Kinetic Merge fails to detect that line 2298 was moved to line 1440 on the "Ours" branch (`6e0b6821`).
- Edits made by "Theirs" (`55fef2f7`) inside `TheirModificationAndOurDeletion` are applied at the original line location rather than being migrated to the refactored destination at line 1440.
- This creates an un-migrated edit overlap with "Ours" changes at line 1395, producing the merge conflict in `Main.scala`.

---

## Recommended Solution

To prevent group fusion from corrupting asymmetric parallel match groups while retaining its benefits for crossed-over matches:

`withoutRedundantPairwiseMatches` should verify that **all** matches in a pairwise group are redundant before re-assigning the group ID:
```scala
// Only re-assign group ID if EVERY match in the pairwise match group is redundant
val groupIsFullyRedundant = groupsOfParallelMatches(groupId).forall(redundantMatches.contains)
```
If a pairwise match group contains non-redundant matches (like line 2299), re-assigning its group ID conflates distinct match structures and breaks block-level LCS alignment.
