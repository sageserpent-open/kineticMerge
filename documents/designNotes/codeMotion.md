# Code Motion #

Code motion may be:

1. A move of some content from one place in a file to another in the same file that can't form part of the longest
   common subsequence for that file's three-way merge. This is *intra-file* code motion.
2. A move of some content from within one file to somewhere within another file with a different path (the filename
   itself may or may not change). As degenerate cases, the moved content may occupy all of either the source or
   destination file.

In both cases, the move is said to have a *source* and a *destination*, the source being the location of the content in
the base side and the destination being the location of the content on either the left or the right side.

A single move has precisely one source and one destination, but it is expected that the same content may be matched
ambiguously in the base and on either the left or right, so it is possible to have multiple moves sharing the same
content; we may have multiple sources converging on a single destination, or a single source fanning out to multiple
destinations, or a mixture of multiple sources and destinations. In that third situation, these *might* represent
parallel moves of the same snippet of code, but could also have either convergence or fan-out for some moves. Kinetic
Merge doesn't try to resolve such moves - it works with the totality of sources and destinations involving a given piece
of content.

# Moves to both Sides

It is also possible for content from even just a single source to move to a destination on the left **and** a
destination on the right.

If both such destinations are aligned from the point of view of the three-way merge of a file containing both
destinations, then the move is termed *coincident*: this represents a situation where the same moves have been made on
both the left and right histories since the base ancestor commit, typically due to cherry-picking of changes, or less
formal sharing of patches via email (or indeed just plain eyeballing changes and manually duplicating them).

If the destinations either go to distinct file paths or land in the same file path without being aligned by the
three-way merge, then the move is termed *divergent*. Kinetic Merge does **not** attempt to resolve divergent moves,
considering both the left and right moves as equally valid choices; rather it honours both, allowing the content to be
duplicated in the final results. As we will see, this is important when considering migration.

# Migration #

Work on code on one side of a merge needs to follow code motion of the original code on the other side; this is referred
to as *migration*. This breaks down into two forms, *change migration* and *splice migration*.

# Change Migration #

This pertains to migrating an edit or an outright deletion of some content that already exists in the base side and
which moves elsewhere on either the left or right side; the edit or deletion takes place on the opposite side to the
move.

Thinking in terms of sections, we have a source section in the base that moves to a destination section on the left or
the right, and an associated *substitution* from the opposite side that is either an edit (thus a non-empty sequence of
sections that replaces the destination section in the final merged output) or a deletion (an empty sequence of
sections).

The job of change migration is to associate a substitution with a move, remove any **edit** sequence from its original
location in the merged output (because it is being migrated, so the original location becomes the wrong place) and to
apply the substitution to the destination section.

A subtlety here is the interplay between detecting moves and producing the final merged output. Moves are detected by
collecting information from across all the per-file three-way merges and then evaluating the moves using a global
picture; that takes place in [
`MoveDestinationsReport.evaluateSpeculativeSourcesAndDestinations`](https://github.com/sageserpent-open/kineticMerge/blob/acfd8239370d6a0d8fa9a235801af47aed77d868/src/main/scala/com/sageserpent/kineticmerge/core/MoveDestinationsReport.scala#L57)

Once this has built up the picture of moves and their associated substitutions, the per-file merges have to replayed
because the sources of moves need to be reinterpreted to take the moves into account; this is done
via [
`ConflictResolvingMergeAlgebra`](https://github.com/sageserpent-open/kineticMerge/blob/acfd8239370d6a0d8fa9a235801af47aed77d868/src/main/scala/com/sageserpent/kineticmerge/core/ConflictResolvingMergeAlgebra.scala#L12)
which is responsible for suppressing the edit sequences in their original locations; that has the effect of changing the
merge outcome.

## Migrating a deletion:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/migratingADeletion.excalidraw.svg)

## Migrating an edit:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/migratingAnEdit.excalidraw.svg)

## Simple move with a deletion on the move side:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/simpleMoveWithADeletionOnTheMoveSide.excalidraw.svg)

## Simple move with an edit on the move side:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/simpleMoveWithAnEditOnTheMoveSide.excalidraw.svg)

## Migrating a deletion with an edit on the move side:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/migratingADeletionWithAnEditOnTheMoveSide.excalidraw.svg)

## Migrating an edit with an edit on the move side:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/migratingAnEditWithAnEditOnTheMoveSide.excalidraw.svg)

## Migrating a deletion with a coincident edit:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/migratingADeletionWithACoincidentEdit.excalidraw.svg)

When there are moves from the same source to both sides, be they coincident or divergent, then Kinetic Merge takes the
view that there is no change associated with either side's move: because the original content has turned up on both
sides, it cannot be considered as being deleted or edited on the other side; any such change is simply left as-is in the
final merge without being migrated.

## Coincident move:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/coincidentMove.excalidraw.svg)

## Divergent moves:

![](https://github.com/sageserpent-open/kineticMerge/blob/main/documents/designNotes/diagrams/divergentMoves.excalidraw.svg)

# Splice Migration #

This is motivated by having to migrate an insertion of new content adjacent to some content that already exists in the
base side and which moves elsewhere on either the left or right side; the insertion takes place on the opposite side to
the move.

In contrast with change migration, the destination section should *not* be substituted; rather the inserted content
should migrate to land adjacent to the destination section, either preceding or succeeding it depending on how it was
originally inserted.

Again, the inserted code has to be suppressed at its original location, because it has migrated.

In practice, it is not enough to simply transplant the inserted sections, because there may be content adjacent to the
destination section that is sitting in exactly the place that the migrated insertion needs to go.

There is also the wrinkle that if we have a move, not only can content be *inserted* adjacent to source of the move, but
there may be *deletions or edits to existing content in the base side* adjacent to the source of the move; these are not
applied to the moved section, because that section still exists adjacent to them. So it is potentially a mixture of
insertions, deletions and edits that require such migration.

At this point some terminology is useful - for a move, we speak of it as having *anchors*, these are the sections
involved in a move, namely the base section at the source of the move, the unmoved section on the opposite side of the
move (with the insertions, deletions and edits adjacent to it) and the destination section on the side of the move.

The three anchors are used to set up three-way merges between the adjacent content on the base side, the side opposite
to the move and the move destination side. This is done for both content preceding the anchors and succeeding the
anchors, so there are two merged outcomes; these are referred to as *splices*, and it is these that are actually
migrated.

The content adjacent to the base anchor disappears naturally from the merge output, and both the opposite side's
adjacent content **and** the move destination side's adjacent content are suppressed. Once this is done, the splices are
simply inserted, preceding or succeeding the move destination anchor.

There is a subtlety in that a splice may succeed one anchor and precede a later anchor, being bracketed by the two. If
the corresponding moves are in parallel, the splice must be inserted only once, being shared between the bracketing move
destination anchors.

Another subtlety is that a migrated edit implies a special anchor where the edit sequence acts as a stand-in for the
anchor opposite to the move: this is handy because it prevents migrated edits from blurring into splices and thus being
migrated twice by both the change and the splice migration mechanisms.
