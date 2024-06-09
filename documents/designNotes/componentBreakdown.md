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

All of these end up delegating to `Main.mergeTheirBranch` - this takes an `Main.ApplicationRequest` parameter object
that bundles up the command line parameters as a case-class. This performs some minimal validation that Git is available
and that the working directory for the merge is the working tree of a Git repository, then constructs an instance
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

## `com.sageserpent.kineticmerge.core` ##

**NOTE:** except `Token`, all of the components below are parameterised with types representing paths, content elements
and elements being merged. This makes it easier to test them in isolation by using, say simple integers for paths, or
makes it easier to view failing test outputs by using integer content elements or single-character upper and lowercase
merge input elements.

### `Token` ###

Each instance is a token of content - might be a single character, such as a parenthesis, brace, plus sign or equals
sign, or a run of non-whitespace characters such as an identifier, a number, or an entire string constant, or a run of
whitespace. A whitespace token is folded into any preceding non-whitespace token to build a single token. Only an
initial whitespace token starting a file's content is left standalone.

Tokens can be compared and ordered, but any trailing whitespace is ignored to implement whitespace insensitivity. A
standalone whitespace token however is examined when comparing with other tokens, including another standalone
whitespace token.

The importance of tokens is to cut down the workload of the rest of the core machinery, and to implement whitespace
insensitivity. Any other kind of insensitivity, say case insensitivity, should probably be implemented here.

The companion object provides `Token.tokens` that tokenizes string content.

### `Sources` ###

This provides a clean abstraction of a codebase from the point of view of a side. There are three instances in play -
one for the base, the left and the right. An instance:

1. yields a set of paths to the files in the codebase as seen from the relevant side,
2. acts as a factory for `Section` instances to refer to parts of the content in the codebase,
3. looks up the path to the file whose content is referred to by a section,
4. covers all of its content by `File` instances, taking into account mandatory sections provided by the caller and
5. provides an initial breakdown by `File` instances that default to having one section for each file.

A `Sources` instances is *not* a mutable holder of `File` instances, nor of `Section` instances - files and sections are
to some extent freestanding, although the latter retain an association with their parent sources. It is therefore
possible to request multiple and differing breakdowns of the same sources.

It is expected that callers do not pass sections associated with other `Sources` instances to a sources when looking up
a path or performing a breakdown.

### `File` ###

Represents a file, yielding an indexed sequence of `Section` instances that contiguously cover the entire file's
content. Has some convenience methods to work with the entire content without having to consult the sections.

**NOTE:** the content of a file is defined by its sections only. It is the responsibility of a sources to make sure the
right sections are packed into a `File` instance to cover the content. So the file's content is defined synthetically by
construction, not analytically by referring to an external source of content. To stress this point, a `File` instance
does *not* wrap an underlying file at the OS filesystem level.

### `Section` ###

Represents a continuous piece of content at a given path that is demarcated by a start offset and one past the end
offset - so a closed-open interval of offsets into the file content. It is possible (and expected) to ask a `Sources`
instance to generate several sections that may be non-contiguous due to either gaps or overlaps, but constructing
a `File` requires contiguous sections.

A section has a loose association to the sources used to create it - in contrast to a file, it is defined analytically
as a window into some content belonging to the sources at some path.

**NOTE:** sections are compared according to how they are defined, not by the content they contain. So if two sections
belong to different sides, or were made from different paths, or cover different regions of the same file's content,
then they are dissimilar.

### `Match` ###

Represents three or two items, all from different sides that match. These items are sections when built in the context
of `CodeMotionAnalysis`, but may be simpler constructs in tests.

A match can be consulted for its *dominant element* - this is one chosen to go through into the merge result. The
dominant element is taken to be the left side's contribution if it is available, otherwise it is from the right side. In
conjunction with whitespace insensitivity at the token level, this replicates how `git merge` selects from matching
content when merging.

Matches over sections are built by examining the *content* covered by a section - the matched sections cannot compare
equal using `Section.equals`, because they are from different sides.

Bear in mind that the same content may participate in multiple ambiguous matches with several locations on one, two or
all three sides. Each such match gets its own instance - given a set of ambiguous matches, we expect each match to
differ if we compare the elements. What this means when matches are made over sections is that at least one side's
section from each match must differ by intrinsic equality from the corresponding side's section in all the other
matches (although the content will agree).

Some handy jargon for later - a match across all three sides is an *all-sides match*, and one across just two side - be
they base plus left, base plus right or left plus right - is a *pairwise match*.

### `CodeMotionAnalysis` ###

The class is a result of the act of performing the analysis. It is a breakdown of files by path for each of the three
sides, together with a set of all the matches gleaned from the analysis.

The files will yield sections that are either parts of the matches, or are gap filler sections to cover the unmatched
content on all three sides.

The companion object defines `CodeMotionAnalysis.of` - this performs the analysis, being a factory for the instances.
Its job is find an optimal set of matches, matching as much of the content as possible across all the sides, but using
no more matches than is necessary. All the content across the sides should be covered by sections that either belong to
a match or are gap fillers.

This method takes a `Sources` instance for each of the three sides and a `CodeMotionAnalysis.Configuration`
that `Main.mergeTheirBranch` provides.

There is a lot of logic in this method; this is discussed further elsewhere.

### `RollingHash` ###

Instances of `RollingHash` are imperative sessions that are used to compute multiple windowed hashes, the window being
rolled over an implied sequence of bytes. Each windowed hash is referred to as a *fingerprint*.

Each session instance is created by an immutable instance of `RollingHash.Factory` that embodies the window size and the
expected number of fingerprints required; the factory instances themselves are expensive to create and are cached
within `CodeMotionAnalysis.of`.

Interaction with a `RollingHash` session is by repeatedly pushing in single bytes (this implies the byte sequence). Once
enough bytes have been pushed to fill the window, the session is *primed* and can be consulted for the latest
fingerprint. As each following byte is pushed, the fingerprint is recomputed to track the movement of the window through
the byte sequence.

**NOTE:** the expected number of fingerprints is merely a hint to help avoid collisions between fingerprints computed
from differing byte window content.

### `CodeMotionAnalysisExtension` ###

To avoid bloating `CodeMotionAnalysis`, an extension is provided that adds a global merge to it. In effect the analysis
is treated as a parameter object to the merge.

This performs file-by-file three-way merges across the sides, building up a picture of code motion and associated
changes (edits or deletions) or insertions that need to be migrated through the code motion. It then performs the
migrations on the merge results for each file as a single postprocessing step once the code motion is known globally.

Code motion that doesn't have associated changes or insertions does not feature in the postprocessing step - there is
nothing to do. It is however tracked as it is important to the process of merging.

The outcome is a map of `MergeResult` values keyed by path; the paths are seen from the point of view of the final
merge, so may be from either the left or right side. A report of the move destinations (and their sources) is also
produced - and this does include *all* code motion, with or without migrations.

### `MergeResult` ###

The result of a three-way merge in terms of content. It may be either fully-merged, in which case there is an indexed
sequence of merged elements, or may have conflicts, in which case there are two indexed sequences, these being the left
and right sides' interpretations of the merge result. Code motion is not modelled in the result, but the presence of
code motion may affect what goes into the result.

### `merge.MergeAlgebra` ###

An *object algebra* ([definition here](https://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf)) driven by a
three-way merge - its operations carry out decisions made by the three-way merge algorithm as to how to merge elements
from the left and right sides. If object algebras or use of *tagless-final* are unfamiliar, think of it as a glorified
strategy object with a flexible result state.

It is worth nothing that the three-way merge plays no favourites between the left and right sides when their
contributions match - this is reflected in the operation method signatures that take both contributions - namely
preservation of an element in the base that remains on both the left and right, and coincident insertion of the same
element on both the left and right.

Merge algebra implementations have to pick a side, or find a way of unifying the content of the rival elements.

A merge algebra implementation has an associated result type - when testing the three-way merge, this result type is
spartan, being a plain `MergeResult`. `CodeMotionAnalysisExtension.merge` drives the three-way merges with a richer
merge algebra that yields a `MatchesResult.MergeResultDetectingMotion`.

### `merge` ###

A standalone object that defines `merge.of`. This method takes three sequences of elements to be merged, one for each
side and a merge algebra that is driven by the merge algorithm's decisions. The result of the merge algebra is the
result of the merge.

### `MoveDestinations` ###

An instance represents the destinations a piece of code may move to. There are subtleties here:

1. There are potentially up to three kind of move destination for the same piece of code - the left side, the right side
   or a coincident move to the same place on both the left and right.
2. For each kind of move destination, there may be multiple entries due to ambiguous matches.
3. Again due to ambiguous matches, there may be several sources of the same piece of code.

This class is odd in that it isn't a single move destination, it also carries the sources of the moves, and it is
parameterised by an element type that itself doesn't expose any location in its abstract form. It is only
because `CodeMotionAnalysisExtension.merge` works in terms of sections that the location information is modelled. This
works perfectly well; the logic only needs to be able to identify a destination, it doesn't need any location when
migrating changes or insertions.

Nevertheless, the report shown to the user when running Kinetic Merge will refer to sections, and these do display
location information.

### `MatchesContext` ###

Provides context to `MatchesContext.MergeResultDetectingMotion` and its merge algebra implementation. The context is
simply a lookup of matches for a given section.

### `MatchesContext.MergeResultDetectingMotion` ###

A richer merge result that aggregates (and delegates to) a simpler merge result (in `CodeMotionAnalysisExtension.merge`,
this is a plain `MergeResult`). It has extra baggage that tracks changes (edits and deletions) to be migrated, move
destinations and insertions that may or may not need migration.

### `LongestCommonSubsequence` ###

Employed by `merge.of`, this represents the backbone of a three-way merge.

The companion object defines `LongestCommonSubsequence.of`, a factory that takes three indexed sequences of elements to
be compared, one from each side. While the algorithm computes the longest common subsequence, it is generalised so that
the result represents not only the longest common subsequence across all three sides, but additional contributions where
elements only align on two sides, augmented further with elements present only on one side.

In essence, it classifies the elements according to whether they are in the core longest common subsequence, or are
common on just two sides, or are just different. This classification is revealed separately for all three sides.

The algorithm used is the standard dynamic programming algorithm recast as a memoized recursive algorithm. It is not
robust against stack overflow, nor does it scale well - but as the three-way merge works over sections rather than
tokens, it works very well in practice, stack usage and performance being no problem. The focus was on getting something
comprehensible to pass its tests, rather than to hit optimisation goals prematurely.

There are theoretically better algorithms out there (as well as some crude heuristics to optimise the standard
algorithm), but they haven't been deemed necessary. Yet.
