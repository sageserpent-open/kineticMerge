# The Job Description #

Kinetic Merge's job is to merge branches for a codebase spread across a Git repository or directory tree.

Like Git's own merge command `git merge` does with standard three-way merges, it takes an implicit commit or state
(the branch head of the current working directory, or a target directory), an explicit commit or directory given by a
branch name, tag, commit SHA, or directory path, and attempts to produce a clean merge reflecting the history leading
up to the two versions being merged.

(From now on, let's call the histories or versions *branches* or *sides*.)

It is expected that the branches will share a common ancestor commit or common base directory. Kinetic Merge assumes
the existence of a single best common ancestor commit or explicit base directory that it compares subsequent changes
against. Unlike `git merge`, which may recursively pre-merge common ancestors when there are multiple merge bases,
Kinetic Merge currently operates with one best common ancestor.

The merge is performed holistically across the entire set of file paths in the codebase, seen from the point of view of
all three sides (base, left/our side, and right/their side).

So what is the essential difference from standard merge tools?

Kinetic Merge is **code-motion aware**: it takes into account the movement of swathes of code from one location to
another, both within a file (*intra-file*) and across different files (*inter-file*), typically arising from
refactorings.

Examples of code motion include:

1. **Subexpression hoisting**: Hoisting a complex subexpression into a variable declaration and referencing the variable
   where the expression used to be.
2. **Method/Function extraction**: Extracting a helper function or method from a larger block of code into a new
   definition elsewhere in the file.
3. **Superclass/Trait extraction**: Hoisting methods and fields from a concrete class into an extracted superclass or
   trait.
4. **Interface extraction**: Extracting interface declarations from existing classes, effectively copying method
   signatures into a new interface file.
5. **Class/Module movement**: Moving nested classes, inner functions, or modules to top-level definitions, or into
   dedicated files.
6. **Code reordering**: Alphabetizing methods, grouping access levels, or pairing accessors and mutators.
7. **File splits**: Carving a single large file into multiple smaller, focused modules.
8. **File joins/condensations**: Consolidating multiple small or deprecated files into a single unified file.
9. **File renames and directory moves**: Moving a file to a new path or renaming it.

Standard merge tools (such as Git's default recursive or ort merge strategies) only detect whole-file renames via
heuristic similarity hashing. If a file is split, or code is excised from one file into another, or methods within a
file are rearranged, Git interprets the move as unrelated deletions at the origin and unrelated additions at the
destination. If the other branch simultaneously edits or deletes code in that moved snippet, or inserts new code
adjacent to it, Git generates sprawling merge conflicts or maroons changes at the outdated location.

Kinetic Merge solves this by:

1. Detecting matched swathes of code across the base, left, and right sides across *all* modified, added, or deleted
   files.
2. Identifying which sections of code have moved (diverging from the backbone alignment of their containing files).
3. **Migrating changes** (edits or deletions) made on one side of a move to the corresponding move destination on the
   other side.
4. **Migrating splices** (insertions or localized edits adjacent to moved code) so they land at the corresponding
   boundary adjacent to the move destination.

## Deliverables

Kinetic Merge provides three primary deliverables:

1. **`kinetic-merge` (Git CLI application)**:
   A command-line executable integrating directly with Git repositories. It verifies a clean working tree and index,
   inspects Git commits to establish the base, left, and right sources, performs the merge, updates the working tree and
   index, and either commits the result or stages conflicts for manual resolution.
2. **`kinetic-merge-tool` (Directory CLI application)**:
   A command-line executable operating on three directory paths (`base`, `our`/`left`, `their`/`right`), independent of
   version control systems. Analogous to `git merge-file`, but operating over entire directory trees.
3. **`kinetic-merge-core` (Core Library)**:
   A standalone Scala library published to Maven Central (`com.sageserpent.kineticmerge:kinetic-merge-core_3`),
   containing all core abstractions: tokenization, match analysis, parallel match groups, blocks, 2-tier approximate
   longest common subsequence, and code motion migration algebras.

## Merge Outcomes

When running a merge, the possible outcomes mirror and extend Git's merge semantics:

1. **Clean merge, committed**:
   All files merge cleanly with all code motion and migrations resolved without contradiction. A merge commit with two
   parents is written to Git, the working tree is updated, and the index is clean.
2. **Clean merge, uncommitted (`--no-commit`)**:
   All files merge cleanly, but changes are left staged in the Git index (or written to target directories in
   `kinetic-merge-tool`) for inspection before committing.
3. **Conflicted merge**:
   When irreconcilable edits occur (such as conflicting modifications to the same section, or divergent moves
   contesting the same code), Kinetic Merge completes as much merging as possible—including all unambiguous code
   motion migrations. In Git, conflicted files are staged at index stages 1 (base), 2 (our/left), and 3 (their/right).
   Crucially, unlike standard Git, the stage-1 (base), stage-2 (our/left), and stage-3 (their/right) entries generated
   by Kinetic Merge are all updated to incorporate the non-conflicting parts of the merge and code motion migrations.
   This brings all three sides closer together, concentrating attention on the remaining conflicts. The working tree
   file is annotated with standard three-way conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`).
4. **Fast-forward merge**:
   If one branch's history is an ancestor of the other, the branch head is simply advanced to the descendant commit
   (unless `--no-ff` is passed, which forces the creation of a merge commit).
5. **Clean rollback on error**:
   If the merge cannot proceed due to environmental failure (dirty working tree, unreadable paths, permission errors),
   Kinetic Merge safely rolls back, leaving the working tree and index in their initial states.
6. **Program crash**:
   If an internal assertion or unhandled failure occurs, execution halts immediately with full diagnostics and stack
   traces preserved.

## Command-Line Configuration Options

Kinetic Merge CLI applications expose options to tune match discovery, ambiguity filtering, and progress reporting:

- `--no-commit`: Stages merged files in the index without making a merge commit.
- `--no-ff`: Prevents fast-forward merges, forcing a merge commit.
- `--no-progress`: Suppresses the interactive console progress bar while retaining final execution timing summaries
  (useful in CI).
- `--minimum-match-size <n>`: Minimum length (in tokens) for a match to be discovered across sides (default: `2`).
- `--match-threshold <fraction>`: Minimum fraction of a file's token size required for a section to qualify for
  matching (default: `0.0`). Specified as a fraction (e.g. `0.05`), percentage (`5%`), or decimal digits (`05`).
- `--minimum-ambiguous-match-size <n>`: Minimum token length required for content that occurs at multiple candidate
  locations on a side to be eligible for matching (default: `10`). Prevents small repetitive tokens from exploding match
  combinations.
- `--ambiguous-matches-threshold <n>`: Maximum number of candidate matches of the same kind that can refer to the same
  matched content before the content is disqualified from matching (default: `20`).
