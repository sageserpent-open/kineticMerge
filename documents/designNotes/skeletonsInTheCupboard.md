# Skeletons in the Cupboard #

## `NecessaryEvil` ##

This is a temporary workaround for [this bug](https://github.com/scala/scala-collection-contrib/issues/239) in a
third-party dependency. At time of writing, there is an open PR to fix this; in the meantime this stands in.

It is used in `MatchesContext.MoveDestinationsReport.mergeWith` to fuse maps together as an outer-join.

## `Sources.filesByPathUtilising` ##

Given a set of mandatory sections, a `Sources` instance representing a side provides a breakdown by path to files so
that all the side's content is covered by non-overlapping sections belonging to those files; additionally, the mandatory
sections are included in this breakdown, augmented if necessary with gap-filler sections.

The method signature looks like this:

```scala
  def filesByPathUtilising(
                            mandatorySections: Set[Section[Element]],
                            candidateGapChunksByPath: Map[Path, Set[IndexedSeq[Element]]] = Map.empty
                          ): Map[Path, File[Element]]
```

The `candidateGapChunksByPath` argument is a hint to the implementation to try to split gap filler sections into smaller
pieces; if no such gap chunks are provided, the gap filler sections will be maximal, either covering an entire file
standalone, or stretching from one mandatory section to the next, or filling the gap between the beginning / end of file
and the respective following / preceding mandatory section.

When gap chunks are provided the implementation is expected to look for matching content within the gaps, choosing a
single matching gap chunk and splitting the gap fill into a section that covers the matched chunk content plus any
preceding or succeeding sections if necessary. No attempt is made to use more than one chunk, or to try several matches
of the same chunk in the same gap.

The motivation for this additional level of breakdown is
described [here](https://github.com/sageserpent-open/kineticMerge/issues/43).

Briefly, because matches have to clear a certain minimum size to be considered, small runs of content can fail to match,
resulting in a poor merge.

`CodeMotionAnalysis` breaks down the base sources without any gap chunks and notes the resulting gap filler sections.
These are used as candidate gap chunks when breaking down the left and right sources; because the gap chunks come from
the base, any edit that contains the original edited text will end up being split with the original edited text as a gap
chunk. That in turn allows the downstream three-way merges to align the gap chunk sections, which leads to a good merge.

It's a nasty hack that uses two separate major components to coincidentally work together, but it works
around `CodeMotionAnalysis` being blind to legitimate match opportunities at low match window sizes. Low match window
sizes are covered in the discussion elsewhere about blind alleys. In practice, it's quite effective as a stop-gap
solution.

## `merge.of` - Checks against marooned Edits or Deletions ##

The three-way merge algorithm was intended to systematically walk through the three sequences of contributions
from `LongestCommonSubsequence`, so any element on any of the base, left or right is either processed completely, moving
on it successor, or is kept on hold when one or both of the other sides moves to the next element.

This isn't always possible - there is a certain amount of lookahead scanning done to detect *marooned edits /
deletions*.
See [`rightEditNotMaroonedByPriorCoincidentInsertion`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/merge.scala#L90),
[`leftEditNotMaroonedByPriorCoincidentInsertion`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/merge.scala#L107),
[`rightEditNotMaroonedByPriorLeftDeletion`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/merge.scala#L124)
and
[`leftEditNotMaroonedByPriorRightDeletion`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/merge.scala#L137).

So far, this lookahead scanning is either rare enough or doesn't scan fare enough before aligning the edit across the
base and left / right sides to cause a noticeable performance hit, so in it stays.

## Miscellaneous ##

1. [`MatchesContext` defines too much as being class-scoped rather than companion object-scoped](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/MatchesContext.scala#L34)
2. [The recursive helper `of` within `LongestCommonSubsequence.of`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/LongestCommonSubsequence.scala#L146)
   uses memoization as part of a non-tail recursive approach. This works OK when merging sections, but fails completely
   due to stack overflow when individual tokens are merged. For now this has been kicked into the long grass, but it
   might need rewriting as a dynamic programming algorithm, or the memoization should be rewritten with a pure
   functional approach, maybe using Cat's `Eval` to defeat stack overflow.