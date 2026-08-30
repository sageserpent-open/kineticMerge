# Kinetic Merge Benchmark Conflict Analysis Report

## Executive Summary
During the manual benchmark procedure documented in `AGENTS.md` (merging commit `55fef2f785334b7bf99fc592dd101a462cbb2c6d` into benchmark repository state at commit `6e0b6821fa8fe53465c3355793b61ac425a10c1a`), Kinetic Merge reports a merge conflict in `src/main/scala/com/sageserpent/kineticmerge/Main.scala`.

A comparative investigation between Kinetic Merge built prior to commit `399aafdd` (at commit `7167586`) and Kinetic Merge post commit `399aafdd` reveals the exact mechanism behind the difference in merge quality:

1. **At commit `7167586`**, Kinetic Merge produces a **clean merge** of `Main.scala` (with 50 single section moves detected in `Main.scala`).
2. **At commit `399aafdd` (PR #408: Fuse parallel matches groups)**, Kinetic Merge reports a **merge conflict** in `Main.scala` (with 46 single moves and 2 ambiguous moves detected).

After deep technical analysis of the matching algorithms, block construction, and LCS alignment logic, **the conflict in `Main.scala` introduced by commit `399aafdd` represents a subtle algorithmic regression / over-grouping behavior in parallel match group fusion, rather than an inevitable text conflict.**

---

## Technical Details & Comparative Analysis

### 1. Three-Way Merge Context
- **Base (Common Ancestor)**: Commit `ba49e075aa124892816c6fe22d635bf6b541435b`
- **Ours (`HEAD` / benchmark repo state)**: Commit `6e0b6821fa8fe53465c3355793b61ac425a10c1a`
- **Theirs (Target branch)**: Commit `55fef2f785334b7bf99fc592dd101a462cbb2c6d`

### 2. Comparison of Merge Outputs Between Builds

#### A. Pre-Fusion Build (`7167586`)
- **Move Detection**: Detects **50 single moves** (and 2 ambiguous moves in test files).
- **Section Movement in `Main.scala`**:
  - `7167586` identifies precise move destinations for control flow constructs around line 2298 in `Main.scala` (`if theirModificationWasTweakedByTheMerge ...`).
  - By migrating these moved sections to their new relative locations on the "Ours" branch (`line 1440`), the edits made by "Theirs" (`55fef2f7`) inside `TheirModificationAndOurDeletion` land cleanly at line 1440 without overlapping the edits made by "Ours" (`6e0b6821`) at line 1395 in `OurModificationAndTheirDeletion`.
- **Result**: `Main.scala` merges completely cleanly.

#### B. Post-Fusion Build (`399aafdd` / Current `main`)
- **Move Detection**: Detects **46 single moves** (and 4 ambiguous moves).
- **Lost Moves**:
  - The move of the section `BASE line 2298` (`if theirModificationWasTweakedByTheMerge then if mergedFileContent.nonEmpty ...`) to `OURS line 1440` is **no longer detected** as a single move.
  - Instead, the section remains unaligned at the block level and fails to migrate.
  - Consequently, edits from "Theirs" (`55fef2f7`) are applied at the original file location rather than being migrated to the refactored destination in "Ours" (`6e0b6821`), causing an un-migrated edit overlap and triggering a merge conflict at line 1395.

---

## Root Cause Mechanism: How Commit `399aafdd` Caused the Quality Difference

### 1. Purpose of Commit `399aafdd` (PR #408)
PR #408 introduced group ID fusion in `MatchAnalysis.scala` (`withoutRedundantPairwiseMatches`).
- When pairwise matches (e.g., `BaseAndLeft`) share sections with an `AllSides` match, they are considered redundant.
- `399aafdd` re-assigned the `ParallelMatchesGroupId` of pairwise match groups to match the `ParallelMatchesGroupId` of the corresponding `AllSides` match group ("groupIdCutovers").

### 2. The Algorithmic Side Effect
1. **Conflation of Distinct Parallel Match Groups**:
   - In Scala code bases with repetitive structures (such as `Main.scala`, which contains symmetric case branches like `OurModificationAndTheirDeletion` and `TheirModificationAndOurDeletion`), identical or similar token sequences occur across multiple methods/branches.
   - When a pairwise match group is fused into an all-sides match group via `groupIdCutovers`, the group ID cutover is applied across **all** matches belonging to that pairwise group ID.
   - If a pairwise match group contained matches associated with *more than one* distinct parallel code structure, re-assigning its group ID causes two structurally distinct code blocks to share the same `ParallelMatchesGroupId`.

2. **Impact on Block Ordering (`Order[Block[Element]]`)**:
   - `SectionedCodeExtension` synthesizes `Block` instances for block-level Longest Common Subsequence (LCS) alignment.
   - `Order[Block[Element]]` compares blocks based on their shared `ParallelMatchesGroupId`.
   - When distinct blocks in `Main.scala` are erroneously assigned the same `ParallelMatchesGroupId` due to over-aggressive group fusion, the block-level LCS alignment fails to distinguish between the distinct parallel blocks.
   - This misalignment causes block-level LCS to drop valid block matches (specifically the moved block at line 2298 of `Main.scala`), preventing Kinetic Merge from recognizing the code motion.

---

## Conclusion & Recommendations

1. **Is the conflict a regression?**
   - **Yes.** The conflict in `Main.scala` under the benchmark is a direct consequence of commit `399aafdd` (PR #408). Over-grouping in `withoutRedundantPairwiseMatches` conflates parallel match groups, causing Kinetic Merge to miss a valid code move that commit `7167586` successfully detected and merged cleanly.

2. **Resolution Path**:
   - `withoutRedundantPairwiseMatches` in `MatchAnalysis.scala` should only fuse a pairwise match group ID with an all-sides match group ID if **every** pairwise match in that group is redundant with the *same* all-sides match group. If only a subset of matches in the pairwise group are redundant, reassigning the entire group ID conflates distinct parallel match groups and breaks block-level LCS alignment.
