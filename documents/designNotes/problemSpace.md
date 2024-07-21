# Modelling of the Problem Space #

## Sources, Sides, Sections and Matching ##

Kinetic Merge abstracts merging away from Git - it works in terms of three *sources*, one of which represents the
codebase as seen from the base commit, one as seen from our branch head commit and one as seen from their branch head
commit. The sources break down into *files*, each of which is associated with a *path*.

The terminology is blurred a little - thinking of the breakdown of the codebase content, we say 'sources', but when we
think of the contribution to the merge, we say *'side'* - ie, the base side, our side or their side.

There is no knowledge of Git concepts such as commits, merge parents or the index in the core of Kinetic Merge. Here,
*left* is used as synonym for our side and *right* for their side (base remains as it is).

In order to model both merging and code motion, a file in turn breaks down into an indexed sequence of *sections*. Each
section covers a continuous piece of content in a file that is demarcated by a start offset and one past the end
offset - so a closed-open interval of offsets into the file content. A section does not have to align with line boundaries - it may refer to just a part of a single line, or span several lines with the start and / or end being ragged, i.e. not spanning an entire line.

The idea is to cover each file in each of the three sources with contiguous sections, so that all the content belonging
to each of the three sources is covered by one, and only one section. A sources instance does not mandate any particular
breakdown of its content into sections - as a starting point, it simply assigns each file with its own section that
covers all the file's content.

The code is analysed to refine this breakdown - *matches* are sought between candidate sections across sides, so that
the content is considered equivalent. The matches are optimised so that:
a) the matched content in each match is as long as possible and
b) as many matches as is practical are found.

The end goal is to cover as much of the content of all three sides as possible with sections that belong to matches
while keeping the number of matches down and avoiding overlapping matches; sections from the same side are not intended
to share content.

The sections referred to by a match may be across all three sides, or may involve just the base and the left, or the
base and the right - or the left and the right. Matched sections do not have to share exactly the same content - some
allowance is made for whitespace insensitivity.

**NOTE:** the sections in a match *don't* have to belong to files at the same path in each of their respective sources -
while this is valid, we should also expect matches to involve files with differing paths across the sides.

Once an optimal set of matches has been found, each side's sources instance is broken down using a mixture of the
matched sections and 'filler' sections that cover any content gaps in each file - possibly even the entire file if none
of its content was matched. Once we have this breakdown, merging can proceed...

## Merging, Longest Common Subsequence, Code Motion ##

Merging takes place both at a global level, looking at all the paths and taking code motion into account, and also at a
file-by-file level, performing an augmented *three-way merge* on each file path. What is merged is not the raw content
of the sources' files, rather it is the sections that are rearranged by the merge at both global and file-by-file level.
While a section belongs to a sources instance for one of the three sides, it can be queried for the content it covers -
so rearranging sections is tantamount to rearranging the underlying content.

The file-by-file three-way merge itself depends on a more fundamental notion, that of finding the *longest common
subsequence*. The longest common subsequence, in concert with matching, contributes in two ways to the big picture of
merging:

1. it provides a skeleton to hang the three-way merge around for each file path and
2. it identifies code motion.

Bear in mind that in a merge, both the left and right side will see changes to the content of a file, so even if a piece
of code doesn't move to a completely different location from the point of view of how the code is logically
structured, it may still move from one offset to another if we compare content for the same file across different sides.
So to detect genuine code motion, we need a more flexible notion of movement - and this is what the combination of the
longest common subsequence and matching gives us.

The longest common subsequence for a file path is made from the sections taken from all three sides, aligning
corresponding sections across the sides, throwing away sections that don't correspond across the sides and respecting
the original order of the sections that are aligned.

Let's see an example, where sections with matching content across the three sides are given the same letter:

Given a base file for `example.txt` that breaks down into sections *A*, *B*, *C*, *D*, *E*,
a left file for `example.txt` that breaks down into sections *A*, *B*, *C*, **F**, *E* and
a right file for `example.txt` that breaks down into sections *A*, **E**, *B*, *C*, *D*,
this then yields the longest common subsequence of *A*, *B*, *C*.

*D* is not present on the left - it has been edited into *F*, and *E* has moved forwards on the right to come in between
*A* and *B*.

In fact, we can extend this by considering *D* to be part of the longest common subsequence, but in a weaker role - it
only aligns on the base and right side.

What drives the correspondence of three sections from across the sides is whether they all belong to the same match (or
just two of them for the weaker situation). The resulting longest common subsequence then forms the backbone of the
three-way merge, the shared content for a full alignment across all three sides making it through directly into the
output of the three-way merge. Weaker alignments across just two sides could denote either an edit or a deletion (if
between the base and either the left or the right), or a coincident edit or insertion (if between the left and right).
Where there is no alignment at all indicates a conflict.

What happens if the sections are matched across three or two sides, but can't be brought into alignment by the longest
common subsequence algorithm? This is the case for section *E* - it is present on all three sides, but attempting to
align it would throw *B* and *C* out of kilter, thus resulting in a suboptimal common subsequence.

In this situation, we take this as precisely the definition of intra-file code motion - because *E* can't join the
backbone of the merge, it is considered to have 'jumped around'. This can be generalised to include inter-file code
motion - when a matched section isn't part of the longest common subsequence (or is only weakly aligned in it), we look
for a section participating in the same match that belongs to a file at a different path on another side. This indicates
that the current file path is either the source of destination of a move.

So if in the example above, *D* was part of a match involving a section in yet another file `destination.txt`, we would
interpret it is moving out from `example.txt` into `destination.txt` - **not** being edited into *F*. Conversely, if *F*
was part of a match involving two sections in `source.txt`, we would interpret it as moving in to `example.txt` - **not
** being an edit of *D*.

Changes and insertions can be migrated through code motion by using the three-way merge to associate changes or
insertions with sections that are deemed to have moved. Once all the three-way merges have been performed, a
post-processing phase migrates the associated changes / insertions to the code motion destinations as a last-minute
edit.

Like `git-merge`, Kinetic Merge has a degree of whitespace insensitivity (and may in the future generalise this). The
file content is tokenized upfront, folding whitespace trailing a token into the token itself as an addendum that does
not count when comparing two tokens for equality. There is a subtlety in that string constants of the Java/C#/C/C++ ilk
are treated as one big token - although where there are alternate representations of the same string content (for
example Scala's raw strings versus one with escapes), these are treated as distinct.

Because file content that is tokenised can match with whitespace insensitivity, we have to pick a winner when copying
aligned sections into the merged results - do we choose the base, the left or the right? To fall in line with
what `git-merge` does, the merge code favours the left-side's section in a match, if it is available, and the right-side
otherwise.
