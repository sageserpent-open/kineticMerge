# `CodeMotionAnalysis.of`

This method takes three sources representing the base, left and right side of the merge, yielding a `CodeMotionAnalysis`
instance containing a breakdown of files by path for each of the three sides, together with a set of all the matches
gleaned from the analysis.

This is a big method with its own world of local abstractions useful only to its internals.

## The Big Picture ##

The overall plan followed by the implementation is to:

1. Systematically look for matches across the sides, gathering matches for a given window size en-masse across all the
   files in all the sources. The aim of the enclosing method is to match as much content as possible across the sources,
   without using any more matches than is necessary. The search for matches does that in a loose fashion, allowing a
   certain amount of conflicting matches for reasons explained below.
2. Reconcile conflicts by a mixture of fragmenting pairwise matches and paring down or outright elimination of matches
   that are in conflict, along with a removal of pairwise matches that are redundant in the face of all-sides matches
   that cover the same content.
3. Purging of any overlapping matches, because the content needs to be covered uniquely by sections.
4. Filling of gaps where content has not been covered by a matched section with unmatched sections.
5. The final matches are both packed into the resulting `CodeMotionAnalysis`, and their sections on each side are used
   to formulate a breakdown of the corresponding `Sources` instance - these breakdowns go into the resulting
   `CodeMotionAnalysis` too.

## Matches ##

Breaking down the first step, the idea is to work downwards in match sizes - once matches at a given larger match size
have been found, these should block smaller matches that either overlap with or are subsumed by them - we prefer a
smaller number of larger matches to lots of smaller ones, we do not want to have overlapping matches contesting for the
same piece of content, and we do not want redundant small matches covering parts of larger ones.

Now, at first glance, given a possible large pairwise match, it seems like a good idea to go with it because longer
matches are good, even if it subsumes smaller all-sides matches. Things can get tricky, though - in essence, while it is
fine to prefer a single long pairwise match over a mixture of smaller pairwise matches and all-sides matches covering
the same content, this only holds when that long pairwise match is **isolated**. What can happen is that there may be
multiple overlapping pairwise matches involving different combinations of sides vying for the same content, and in this
situation it is better to take all of these into account and then break them down into fragments, using all-sides
matches to pick out the overlaps.

See [this ticket here](https://github.com/sageserpent-open/kineticMerge/issues/23) for a full discussion.

The consequence is that while we should go with the long pairwise match, we still have to look for smaller all-sides
matches subsumed in it to guide a finer-grained merge. If we don't find any such all-sides matches, we keep the long
pairwise match, but if we do, said long pairwise match has to be fragmented by the smaller all-sides matches. Any
content left over from the long pairwise match is expressed as one or more small pairwise matches that abut the
all-sides matches.

It is for this reason that the search for matches is a little loose, requiring following reconciliation.

## Search Strategy for Matches ##

Matches are found working down in match size - this is done in two different ways, one is an iterated binary search
performed
within [
`withAllMatchesOfAtLeastTheSureFireWindowSize`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L176)
and the other is an exhaustive linear scan down candidate window sizes performed
within [
`withAllSmallFryMatches`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1042).

### Iterated Binary Search ###

The binary search approach tries various candidate window sizes between an inclusive lower bound and an exclusive upper
bound. If there is at least one match found at a candidate window size, an estimate of the window size of the optimal
match (if there is just one) is also yielded.

If no match is found, this taken to mean the binary search should go low and tighten the upper bound (if it can go any
lower - otherwise we're done).

If a single match is found, we have an optimal match and note it.

If more than one match is found and the estimate comes out to be the same as the current candidate window size, we have
found multiple optimal matches.

Otherwise, there is more than one match and the estimate is higher than the current candidate window size, so the binary
search should go high and tighten the lower bound.

Each time the search finds an optimal match (or several optimal matches) at a given candidate window size, the binary
search is restarted with the lower bound relaxed to its initial value, and the exclusive upper bound set to the window
size of the latest match(es). There is a subtlety here in that the binary search may terminate because there are
multiple optimal matches, but the estimate is higher than their candidate window size, so the binary search keeps on
looking, ultimately failing to improve on those matches. This situation can happen when the optimal matches refer to
differing content, but have overlaps on one but not all the relevant sides; thus they can't be replaced by a single
larger match.

The solution is to preserve any matches that are not conclusively optimal as a fallback
solution, and to use these when the binary search terminates unsuccessfully.

**NOTE:** we may have to revisit candidate window sizes as the binary search keeps restarting, because once we have
larger matches in the bag, these influence whether smaller matches are permitted or not as mentioned above. So a
candidate window size that led to multiple undersized matches in a previous binary search may yield an optimal match or
matches next time around.

### Linear Scan ###

The reason for using two approaches is down to the optional use of a match threshold - this is a fraction of a file's
size that must be achieved or exceeded for a section to be considered as part of a match.

Using a match threshold leads to a *minimum sure-fire window size* - this is the smallest window size that couldn't be
rejected by the match threshold in any of the files across all three sides.

When candidate window sizes go below the minimum sure-fire window size, there is a possibility that potential matches
can fizzle out because their window size falls short of what the match threshold would allow at least one side of the
match. That in turn means that if a match exists at a larger window size below the minimum sure-fire window size, and we
are searching slightly under that size, we may not see the partial matches that would lead us back up to the optimal
match size, because they fail to meet the threshold criterion.

Therefore, the iterated binary search is only good down to the minimum sure-fire window size.

Rather than jumping around candidate window sizes and missing partial matches, the exhaustive linear scan plods down
through all the candidate window sizes below the minimum sure-fire window size, recording any successful matches as it
finds them.

We don't have to worry about underestimating the candidate window sizes - we already have all the larger
matches in the bag that might exclude whatever is being looked for at the current window size, so as soon as we see
matches, we know they are optimal.

## Checking Sections for Overlaps and Subsumption ##

`MatchesAndTheirSections` does what it says on the tin, accumulating matches and allowing a match to be found from any
of its participating sections. It breaks down into three maps from paths to instances of `SectionsSeen`, one map for
each side, as well as a multi-dictionary implementing the mapping from sections back to their matches -
a `MatchedSections`.

`SectionsSeen` is a range-search data structure layered over a fingertree provided
by [this implementation](https://codeberg.org/sciss/FingerTree). It stores sections belonging to the same side and path
by the range defined by a section's start offset and one-past-end offset. It supports efficient searches for overlapping
and subsuming sections, underpinning several helper methods in the companion object for `MatchesAndTheirSections`.

While it is efficient, it is hammered by `CodeMotionAnalysis.of`; anything that can yield more optimal performance is
likely to provide a good performance boost.

## Finding the Matches for a candidate Window Size ##

This is the job
of [
`MatchesAndTheirSections.matchesForWindowSize`](https://github.com/sageserpent-open/kineticMerge/blob/a9c639c98c78af9ba848243471f51c364845f5d1/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1152).
It packages the matches found together with their count and the estimated window size for an optimal match in
a `MatchingResult`.

It delegates to three helper functions:

1. [
   `fingerprintStartIndices`](https://github.com/sageserpent-open/kineticMerge/blob/a9c639c98c78af9ba848243471f51c364845f5d1/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1167)
   collects fingerprints from the content of each file, along with the start offset of the piece of content that the
   fingerprint refers to. It is expected that fingerprints can collide (and they do occasionally).
2. [
   `sectionsByPotentialMatchKey`](https://github.com/sageserpent-open/kineticMerge/blob/a9c639c98c78af9ba848243471f51c364845f5d1/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1225)
   drives `fingerprintStartIndices` to gather instances of `PotentialMatchKey` on a given side. That class aggregates
   the raw fingerprint, supporting an ordering that a) respects the fingerprint and b) uses the referenced piece of
   content as a tiebreaker if two fingerprints with differing content collide. The content is implied via a section to
   avoid memory bloat from copied content. These keys map to their associated sections via a multi-dictionary instance,
   spanning all the files in that side's sources. We use a multi-dictionary because it is entirely possible for the same
   key to refer to equivalent pieces of content repeated both in the same file and across files. A subtlety is that when
   this happens, we may have a *section* with a single specific location referring to several sections that all differ
   in location - the one in the `PotentialMatchKey` is really an exemplar that refers to the piece of content in common
   to the mapped sections. Bear this in mind when debugging!
3. [
   `matchKeysAcrossSides`](https://github.com/sageserpent-open/kineticMerge/blob/a9c639c98c78af9ba848243471f51c364845f5d1/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1294)
   drives the creation of matches; it combines sets of `PotentialMatchKeys` by intersection across the sides to make
   four sets of keys for pairwise and all-sides matches. It can then work through the sets, using the keys to retrieve
   the associated sections from the relevant sides' multi-dictionaries to build matches. Once the matches are found, it
   calls the helper [
   `withMatches`](https://github.com/sageserpent-open/kineticMerge/blob/a9c639c98c78af9ba848243471f51c364845f5d1/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1645)
   to build the final `MatchingResult` for the candidate window size out of those matches. It is here that the estimate
   for the optimal match size is requested.

## Reconciling Matches ##

This has gone through a *lot* of changes, and code has shuttled back and forth between the reconciliation and the
initial production of matches. Names have changed several times too, and there was a time when `MatchesAndTheirSections`
had a much tighter invariant with match fragmentation happening on the fly as matches were found by the searches;
although prior to that there was a fairly hokey mixture of on-the-fly fragmentation followed by a final cleanup.

Put it this way - think twice before rewriting this, because a lot of alternatives have been tried - and bugs uncovered!
That said, there are lots of weird one-off scenario tests in `CodeMotionAnalysisTest` that were added to reproduce these
bugs, so there is a safety net if you want to get hacking. **Just don't casually delete those weird tests!**

`CodeMotionAnalysisTest.sourcesCanBeReconstructedFromTheAnalysis` is also a surprisingly powerful test these days in
this regard, despite the fact that it looks a lot simpler than the more weighty
`CodeMotionAnalysisTest.matchingSectionsAreFound`.

Because `MatchesAndTheirSections` has loosened the invariant to allow reconciliation to take place at the end, there is
a [weak invariant](https://github.com/sageserpent-open/kineticMerge/blob/5404bdf9b4ebf1178f24dfa29f420e586b0fe01b/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L847)
that holds in general and
a [stronger post-condition that holds after reconciliation](https://github.com/sageserpent-open/kineticMerge/blob/5404bdf9b4ebf1178f24dfa29f420e586b0fe01b/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1001).

Reconciliation takes place
in [
`MatchesAndTheirSections.reconcileMatches`](https://github.com/sageserpent-open/kineticMerge/blob/5404bdf9b4ebf1178f24dfa29f420e586b0fe01b/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1807).

This uses an iterative approach, working with a set of all-sides matches to fragment larger subsuming pairwise matches.
After each iteration discovers new fragments and pares down the matches, this affects the set of all-sides matches
available to cause more fragmentation, so the reconciliation is iterated with each change in the pared down all-sides
matches until it stabilizes.

This iterative approach is vital, see [this ticket](https://github.com/sageserpent-open/kineticMerge/issues/144)
and [this ticket](https://github.com/sageserpent-open/kineticMerge/issues/146) for background.