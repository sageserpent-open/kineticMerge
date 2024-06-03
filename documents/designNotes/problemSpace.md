# Kinetic Merge's view of the World #

Kinetic Merge's job is to merge branches for a codebase spread across a Git repository.

Like Git's own merge command line `git merge` does with the usual kinds of merge, it takes an implicit commit that is
the branch head of the current branch, an explicit commit given by a branch name, tag or commit SHA, and it attempts to
produce a new commit that is the merge of the history leading up to the two commits it works off.

(From now on, let's be a little loose in our terminology and call **all** such histories *branches*, as let's face it,
that how we think of them, regardless of whether the history is referenced by a branch name, tag or commit SHA.)

It is expected that the branches leading up to the two commits will be different, but will share a degree of common
commit ancestry - thus Kinetic Merge assumes the existence of a best common ancestor commit that it can compare
subsequent changes to. Unlike `git merge`, Kinetic Merge works with only one such common ancestor commit (even if there
are several) - it does not 'pre-merge' the best common ancestors if there are more than one.

The merge is taken across the entire set of file paths in the repository, seen from the point of view of both of the two
commits. Again, this is like `git merge` - all relevant files are merged 'at the same time' to make the merge commit.

So what is the essential difference?

Kinetic Merge takes into account code motion in the merge - this is the movement of swathes of code from one location to
another, usually as a result of refactorings.

Examples of code motion include:

1. Hoisting of a complex subexpression into the initialisation of a newly introduced variable, so that the variable is
   substituted where the subexpression used to be. This moves the subexpression out of an enclosing expression and up to
   the initializer of its naming variable.
2. Extraction of a method or function from a larger block of code. What forms the body of the method or function moves
   from the larger block to the method or function definition.
3. Extraction of a superclass from a class. Entire methods and fields are hoisted from the original class to the
   superclass.
4. Extraction of an interface from a class. This is an interesting variation of the previous example, because the
   methods are left unchanged in the original class, but their declarations are copied into new locations in the
   interface. In effect, these are duplicating moves.
5. Moving nested classes, modules or functions out of some enclosing construct and up to the top level of a file - or
   vice-versa.
6. Reordering constructs within a file, or within a class or module within a file, say into alphabetical order or by
   access restriction or some convention - eg. pairing getters and setters.
7. Moving a construct into its own new file or somewhere in another existing file, leaving other constructs behind in
   the original file.
8. Condensing previously separate files together to make one replacement file.
9. Moving a file from one path to another.
10. Renaming a file.

Typically, refactorings may combine several of these basic moves, in addition to performing other changes into the
bargain.

Suppose there are one or more refactorings on one of the branches - pieces of code will have moved around, both within
files and across files. If we have changes to those same pieces of code on the other branch, then we would expect a good
merge to *migrate* the changes through the refactorings that are merged in, so that they end up in the same place as the
code they changed.

If I excise a nested Java class right out of its enclosing class and into its own file on my branch, then what happens
when you change the name of a method in the original nested class? A good outcome would be for your rename to move out
to the excised class' new home in its own file. We wouldn't expect the merge to 'maroon' the rename all by itself
somewhere in the enclosing class back in the original file!

Kinetic Merge looks at all paths involving files that have been modified, added as new or deleted on one or both of the
branches being merged. A file move and / or rename is interpreted as a combination of deletion of the file at its old
path and addition at its new path; Git does that have any formal concept of file moves other than inference when
merging.

Code motion is detected only in the content of the paths referred to above where *changes* have taken place - it does
not look beyond these paths at incidental files, so if a piece of code is added in a modified or added file that also
exists in another unchanged file, it will not consider that to be a kind of duplicating move.

So the example above for superclass extraction would only be tracked for code motion if the file containing the original
class was modified (however, given that the superclass extraction refactoring adds or extends the class' inheritance
relationships, this will be the case in all mainstream OOP languages).

Where a piece of code has moved on one branch, but has been changed on the other (either edited or outright deleted),
the change is *migrated* to the *move destination*.

Where a piece of code has moved on one branch and has had new text inserted adjacent to it on the other, the insertion
is also migrated to the corresponding point adjacent to the move destination.

In essence, Kinetic Merge has to do the same thing as `git merge`, but additionally it has to recognise code
motion in one or both of the branch histories and migrate the corresponding changes and insertions. That's its job.

The outcome of a merge is either a clean merge committed as a Git *merge commit*, a conflicted merge, a clean
merge that is left uncommitted with changed files staged in the index, a fast-forward merge where the branch head is
moved to an existing commit that refers to all the commits in both branches' histories (but no other commits outside
those branches), an error - or a program crash.

Let's look at the outcomes in more detail. We'll use terminology from the man-page of `git merge` - the branch checked
out in our repository's working tree is called `our` branch, the branch we want to merge in is called `their` branch,
and the best common ancestor is called the `base`.

1. Clean merge, committed: a new merge commit is made with two parents; these refer to the branch histories being
   merged. The working tree is updated and the index is left clean. Just like what Git does, only with code motion taken
   into account.
2. Conflicted merge: Kinetic Merge tries to do as much merging as possible, including migration of changes and
   insertions through code motion. No commit is made. Each file that contains conflicting content is represented by two
   versions - our version and their version. The base version is written into the Git index as a stage-1 entry, likewise
   our version is written as a stage-2 entry and their version as a stage-3 entry (but only when the corresponding
   version exists). This mirrors what `git merge` does, but with the twist that our and their versions already
   incorporate the non-conflicting parts of the merge, including migrations through code motion. The working tree is
   updated with the conflicted version of the file; this contains the usual conflict markers that would be generated
   by `git merge`.
3. Fast-forward merge: one of the branches contains all the other branch's history as a prefix. In this case, merging is
   trivial - if our branch is a prefix of theirs, our branch head is moved up to match theirs, and if theirs is a prefix
   of ours, nothing needs to be done. There is also a trivial case where both branches refer to exactly the same
   history; this obviously leaves our branch unchanged. There is a command line option `--no-ff` that forces the
   creation of a merge commit. Again, just as `git merge` does.
4. An error - if the merge fails fundamentally, say due to divergent moves (we'll get to these later) or due to
   circumstances external to Kinetic Merge (eg: write protection on the working directory, lack of read permission, Git
   isn't installed, the working directory is not in a Git repository), then the merge is rolled back and the working
   tree is left in its original state. Kinetic Merge simplifies this rollback with a precondition that the working tree
   must have a clean index prior to it commencing a merge; that way it doesn't have to restore any staged changes in the
   index prior to merging, or even any unstaged changes in the working tree only.
5. A program crash - no attempt is made at cleaning up, the assumption is that this behaviour is not supposed to happen,
   but if it does, we want as much diagnostic information as possible, so the repository / index / working tree are left
   as they were at the point of crashing.

