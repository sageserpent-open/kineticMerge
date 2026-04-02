# Kinetic Merge - Agent Memory

This document serves as a "memory" for agents working on the Kinetic Merge project. It summarizes the project's goals, architecture, and core algorithms.

## Project Goals
Kinetic Merge aims to merge Git branches holistically across the entire codebase, with a specific focus on **code motion awareness**. Traditional Git merges often fail when code is refactored (e.g., moved between files, extracted into methods, or reordered). Kinetic Merge tracks these moves and migrates changes to their new locations.

Key goals include:
- Handling file renames, splits, and concatenations.
- Tracking intra-file and inter-file code motion.
- Automating merges as much as possible while providing clear conflict markers when manual intervention is needed.

## Core Concepts
- **Sources**: Abstract representation of the codebase for a specific "side" (base, left, or right).
- **Sections**: Contiguous, closed-open intervals of content within a file. Files are broken down into sections for analysis.
- **Matches**: Associations between sections across different sides that share equivalent content. Matches can be "all-sides" (base, left, and right) or "pairwise" (any two sides).
- **Tokens**: The fundamental unit of content analysis. Tokens are used to implement whitespace insensitivity by folding trailing whitespace into the preceding non-whitespace token.

## Component Architecture
- `com.sageserpent.kineticmerge`: The outer layer, handling Git repository interaction and CLI.
- `com.sageserpent.kineticmerge.core`: The core logic, abstracted from Git.
  - `MatchAnalysis`: Finds an optimal set of matches across the three sides.
  - `SectionedCode`: Represents the codebase broken down into matched and gap-filler sections.
  - `LongestCommonSubsequence (LCS)`: Aligns sections within a file to form the backbone of a three-way merge.
  - `CodeMotionAnalysisExtension`: Performs the global merge, tracking code motion and migrating changes/insertions.
  - `merge`: Implements the three-way merge algorithm using an object algebra (`MergeAlgebra`).

## Key Algorithms & Processes
1. **Match Discovery**: Uses rolling hashes (fingerprints) to find potential matches of at least a minimum window size. It works downward from larger to smaller window sizes.
2. **Reconciliation**: Fragments and pares down matches to resolve overlaps and redundancies, prioritizing all-sides matches.
3. **Three-Way Merge**: An augmented three-way merge that uses LCS to align sections. If a section exists on all sides but cannot be aligned without disrupting the LCS, it is considered to have "jumped" (intra-file code motion).
4. **Code Motion & Migration**:
   - **Change Migration**: Edits or deletions of content that moved are applied to the destination of the move.
   - **Splice Migration**: Insertions adjacent to moved content are migrated to land adjacent to the move's destination.

## Development Tips
- **Whitespace Insensitivity**: Implemented at the `Token` level. String constants are treated as single tokens.
- **Testing**: Use `sbt test` to run the suite. The project uses `americium` for property-based testing and `expecty` for assertions.
- **Logging**: Use `scala-logging`. Debugging often involves inspecting complex match and section structures.
- **Performance**: LCS and match discovery are performance-critical. LCS uses a memory-efficient, parallelizable dynamic programming approach.

## Important Files
- `Main.scala`: Entry point and Git integration.
- `MatchAnalysis.scala`: The heart of match discovery and reconciliation.
- `SectionedCode.scala`: Higher-level analysis result and breakdown into files/sections.
- `CodeMotionAnalysisExtension.scala`: The global merge and migration logic.
- `merge.scala`: The core three-way merge implementation.
- `LongestCommonSubsequence.scala`: The alignment algorithm.
