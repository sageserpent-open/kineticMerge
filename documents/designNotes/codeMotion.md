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

# Change Migration #

# Splice Migration #

