# The Job Description #

Kinetic Merge's job is to merge branches for a codebase spread across a Git repository or directory tree.

Like Git's own merge command `git merge` does with standard three-way merges, it starts with the commit that is checked
out in the worktree, plus an explicit commit given by a branch name, tag or commit SHA. It then attempts to produce a
new commit that is the merge of the histories leading up to those two commits.

(From now on, let's be a little loose in our terminology and call all such histories branches, as let's face it, that
how we think of them.)

It is expected that the branches will share a common ancestor commit. Kinetic Merge assumes the existence of a single
best common ancestor commit that it compares subsequent changes against. Unlike `git merge`, which pre-merges common
ancestors when there are multiple merge bases, Kinetic Merge currently operates with one best common ancestor.

The merge is performed holistically across the entire set of file paths in the codebase, seen from the point of view of
all three sides - the base (common ancestor), the left (our) side, and the right (their) side.

So what is the essential difference from standard merge tools?

Kinetic Merge is **code-motion aware**: it takes into account the movement of swathes of code from one location to
another, both within a file (*intra-file*) and across different files (*inter-file*), typically arising from
refactorings.

Examples of code motion include:

1. **Subexpression hoisting**: hoisting a complex subexpression into a variable declaration and referencing the variable
   where the expression used to be.
2. **Method/Function extraction**: extracting a helper function or method from a larger block of code into a new
   definition elsewhere in the file.
3. **Superclass/Trait extraction**: hoisting methods and fields from a concrete class into an extracted superclass or
   trait.
4. **Interface extraction**: extracting interface declarations from existing classes, effectively copying method
   signatures into a new interface file.
5. **Class/Module movement**: moving nested classes, inner functions, or modules to top-level definitions, or into
   dedicated files - or vice versa.
6. **Code reordering**: alphabetising methods, grouping access levels, or pairing accessors and mutators.
7. **File splits**: carving a single large file into multiple smaller, focused modules.
8. **File joins/condensations**: consolidating multiple small or deprecated files into a single unified file.
9. **File renames and directory moves**: moving a file to a new path or renaming it.

Typically, refactorings may combine several of these basic moves, in addition to performing other changes into the
bargain.

Suppose there are one or more refactorings on one of the branches - pieces of code will have moved around, both within
files and across files. If we have changes to those same pieces of code on the other branch, then we would expect a good
merge to migrate the changes through the refactorings that are merged in; each change should end up in the same place as
the code it applies to.

If I excise a nested Java class right out of its enclosing class and into its own file on my branch, then what happens
when you change the name of a method in the original nested class? A good outcome would be for your rename to move out
to the excised class' new home in its own file. We wouldn't expect the merge to 'maroon' the rename all by itself
somewhere in the enclosing class back in the original file!

Where a piece of code has moved on one branch, but has been changed on the other (either edited or outright deleted),
the change is *migrated* to the *move destination*.

Where a piece of code has moved on one branch and has had new text inserted adjacent to it on the other, the insertion
is also migrated to the corresponding point adjacent to the move destination.

In essence, Kinetic Merge has to do the same thing as git merge, but additionally it has to recognise code motion in one
or both of the branch histories and migrate the corresponding changes and insertions. That's its job.

## Deliverables

The project provides three primary deliverables:

1. **`kinetic-merge` (Git CLI application)**:
   A command-line executable integrating directly with Git repositories. It verifies a clean working tree and index,
   inspects Git commits to establish the base, left, and right sources, performs the merge, updates the working tree and
   index, and either commits the result or stages conflicts for manual resolution. The introduction above is about this
   application.
2. **`kinetic-merge-tool` (Directory CLI application)**:
   A command-line executable operating on three directory paths - the base, the left and the right, independent of
   version control. Analogous to `git merge-file`, but operating over entire directory trees. It uses the same core
   abstractions as the Git CLI application, but knows nothing about commits or branches.
3. **`kinetic-merge-core` (Core Library)**:
   A standalone Scala library published to Maven Central (`com.sageserpent.kineticmerge:kinetic-merge-core_3`),
   containing all core abstractions: tokenization, match analysis, parallel match groups, blocks, 2-tier approximate
   longest common subsequence, and code motion migration algebras. This is shared between the two CLI applications.

## Merge Outcomes

When running Kinetic Merge, the possible outcomes mirror and extend Git's merge semantics:

1. **Clean merge, committed**:
   All files merge cleanly with all code motion and migrations resolved without contradiction. A merge commit with two
   parents is written to Git, the working tree is updated, and the index is clean.
2. **Clean merge, uncommitted (`--no-commit`)**:
   All files merge cleanly, but changes are left staged in the Git index (or written to target directories in
   `kinetic-merge-tool`) for inspection before committing.
3. **Conflicted merge**:
   When irreconcilable edits occur, Kinetic Merge completes as much merging as possible — including code motion
   migrations. In Git, conflicted files are staged at index stage-1 (base / common ancestor), stage-2 (left / our) and
   stage-3 (right / their). In contrast, the stage-1, stage-2 and stage-3 entries generated by Kinetic Merge are all
   updated to incorporate the non-conflicting parts of the merge *and* code motion migrations. This brings all three
   sides closer together, concentrating attention on the remaining conflicts. The working tree file is annotated with
   standard three-way conflict markers (`<<<<<<<`, `=======`, `>>>>>>>`).
4. **Fast-forward merge**:
   If one branch's history is an ancestor of the other, the branch head is simply advanced to the descendant commit
   (unless `--no-ff` is passed, which forces the creation of a merge commit).
5. **Clean rollback on error**:
   If the merge cannot proceed due to environmental failure (dirty working tree, unreadable paths, permission errors),
   Kinetic Merge safely rolls back, leaving the working tree and index in their initial states.
6. **Program crash**:
   If an internal assertion or unhandled failure occurs, execution halts immediately with full diagnostics and stack
   traces preserved and no clean-up. It's been a while since I last saw one of these, but you might get lucky!

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
