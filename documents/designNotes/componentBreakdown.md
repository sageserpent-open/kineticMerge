# Component Breakdown #

Kinetic Merge has as of version 1.2.1 (Git commit SHA: 58e598) just two packages:

1. `com.sageserpent.kineticmerge` - The outer layer of the application, all interaction with the Git repository is in
   here.
2. `com.sageserpent.kineticmerge.core` - The core code motion detection and merging logic, abstracted away from Git.

## `com.sageserpent.kineticmerge` ##

### `Main` ###

This serves two purposes - it defines `Main.main`, which is the entry point for Kinetic Merge when run as a command-line
tool. It also provides `Main.apply` in two overloads - both may be used to drive Kinetic Merge as a library component,
allowing it to be integrated into the code of third party tools.

All of these end up delegating to `Main.mergeTheirBranch` - this takes a `Main.ApplicationRequest` parameter object that
bundles up the command line parameters as a case-class. This performs some minimal validation that Git is available and
that the working directory for the merge is the working tree of a Git repository, then constructs an instance
of `Main.InWorkingDirectory`; that instance is used to drive the merge.

### `Main.InWorkingDirectory` ###

This interacts with the Git repository, the working tree and the Git index to build up the inputs to the core merge
machinery - `CodeMotionAnalysis.of` and `CodeMotionAnalysisExtenstion.merge`, writing the merge results to the working
tree, the Git index and making a merge commit in the Git repository proper if appropriate. It also performs fast-forward
merges.

It translates between the content available from the Git repository and the inputs required
by `CodeMotionAnalysis.of` - `Sources`, as well as translating back from the output
of `CodeMotionAnalysisExtension.merge` - `MergeResult` - into content to write to the various places mentioned above.

The working directory is captured as context in the instance to simplify the method signatures.

### `Main.Workflow` ###

The majority of the methods in `Main.InWorkingDirectory` wrap their results in this type - it is monadic effect type
that allows exceptions to be cleanly handled as well as keeping a log of operations performed in the workflow. The aim
is for code to be written in for-comprehensions without having to worry about exception handling and logging. It
is `EitherT` layered on top of `WriterT` which in turn is layered on top of `IO`.

There are two helper methods defined in extensions that allow convenient translation of exceptions into error messages
and logging - `Main.labelExceptionWith` and `Main.logOperation`.

### `Main.MergeInput` ###

Each path considered by Kinetic Merge is given a `Main.MergeInput` to classify the kind of merge required. This
distinguishes between, say, a three-way merge were an existing file in the base has been modified by our branch and
their branch, or a new file has been added by our branch, or a file in the base has been deleted by their branch, etc...

### `Main.Change` ###

Each path considered by Kinetic Merge, is given a `Main.Change` to classify what happened to it from the point of view
or our and / or their branch. If both branches changed the same file, this is represented as two change instances.

### `ProgressRecording` ###

These are passed down from the entry points of `Main` to the core merge machinery. They allow the end user or third
party tools to get feedback of the merge machinery's progress at various long-running phases.

For each phase, the `ProgressRecording` instance is used as a factory to build a mutable `ProgressRecordingSession`;
that in turn serves as a callback trait, so the merge machinery can notify interested parties of its progress.

### `NecessaryEvil` ###

What it says on the tin! The plan is to remove it at some point. Please don't open it and let's move on...