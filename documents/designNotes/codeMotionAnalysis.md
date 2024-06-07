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
   certain amount of additional matches for reasons explained below.
2. Once the matches have been found, a cleanup takes place where certain larger pairwise matches are eaten into by
   smaller all-sides matches, leaving the all-sides matches as they are and replacing the original pairwise matches with
   smaller leftover pairwise matches (if any are left over).
3. These matches are cleaned up further, removing any leftover pairwise matches that are accidentally subsumed by other
   pairwise matches.
4. The final matches are both packed into the resulting `CodeMotionAnalysis`, and their sections on each side are used
   to formulate a breakdown of the corresponding `Sources` instance - these breakdowns go into the
   resulting `CodeMotionAnalysis` too.

## Matches ##

Breaking down the first step, the idea is to work downwards in match sizes - once matches at a given larger match size
have been found, these should block smaller matches that either overlap with or are subsumed by them - we prefer a
smaller number of larger matches to lots of smaller ones, we do not want to have overlapping matches contesting for the
same piece of content, and we do not want redundant small matches covering parts of larger ones.

However, there is a subtlety here - given a possible large pairwise match, it seems a good idea to go with it because
longer matches are good, even if it subsumes smaller all-sides matches. Things can get tricky, though -
see [this ticket here](https://github.com/sageserpent-open/kineticMerge/issues/23). The consequence is that while we
should go with the long pairwise match, we still have to look for smaller all-sides matches subsumed in it to guide a
finer-grained merge. If we don't find any such all-sides matches, we keep the long pairwise match, but if we do, said
long pairwise match has to be eaten into by the smaller all-sides matches. Any content left over from the long pairwise
match is expressed as one or more small pairwise matches that abut the all-sides matches.

It is for this reason that the search for matches is a little loose, requiring following cleanup.

The rules for deciding whether a smaller match should be permitted given potential conflict with a larger match are
summarised
here: [matchFrom](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1562).
Please don't remove that comment!

There are some more nasty corner cases to watch out for concerning ambiguous and / or overlapping matches that need
cleanup - tests to exercise these (as well as an explanation) are to be found
here: [anAmbiguousAllSidesMatchSubsumedOnOneSideByALargerAllSidesMatchIsEliminatedCompletely](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/test/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysisTest.scala#L677),
here: [eatenPairwiseMatchesMayBeSuppressedByACompetingAmbiguousPairwiseMatch](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/test/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysisTest.scala#L772)
and
here: [eatenPairwiseMatchesMayBeSuppressedByACompetingOverlappingAllSidesMatch](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/test/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysisTest.scala#L870).

The various cleanups are found
here: [withoutRedundantPairwiseMatchesIn](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L839)
(this is done upfront as matches are discovered during the search),
here: [withPairwiseMatchesEatenInto](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L727)
(this is step #2 from above) and
here: [cleanedUp](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1016)
(this is step #3 from above).

## Search Strategy for Matches ##

Matches are found working down in match size - this is done in two different ways, one is an iterated binary search
performed
within [withAllMatchesOfAtLeastTheSureFireWindowSize](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L176)
and the other is an exhaustive linear scan down candidate window sizes performed
within [withAllSmallFryMatches](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1042).

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

Each time a match (or several matches) is found at a given candidate window size, the binary search is restarted with
the lower bound relaxed to its initial value, and the exclusive upper bound set to the window size of the latest match(
es). There is a subtlety here in that the binary search may terminate because there are multiple optimal matches, but
the estimate is higher than their candidate window size, so the binary search keeps on looking, ultimately failing to
improve on those matches. This situation can happen when the optimal matches refer to differing content, but have
overlaps on one but not all the relevant sides; thus they can't be replaced by a single larger match.

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
of [matchesForWindowSize](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1095).
It packages the matches found together with their count and the estimated window size for an optimal match in
a `MatchingResult`.

It delegates to three helper functions:

1. [fingerprintStartIndices](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1103)
   collects fingerprints from the content of each file, along with the start offset of the piece of content that the
   fingerprint refers to. It is expected that fingerprints can collide (and they do occasionally).
2. [fingerprintSections](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1161)
   drives `fingerprintStartIndices` to gather instances of `PotentialMatchKey`. That class aggregates the raw
   fingerprint, supporting an ordering that a) respects the fingerprint and b) uses the referenced piece of content as a
   tiebreaker if two fingerprints with differing content collide. The content is implied via a section to avoid memory
   bloat from copied content. These keys map to their associated sections via multi-dictionary instances that are built
   for each side, spanning all the files in that side's sources. We use a multi-dictionary because it is entirely
   possible for the same non-colliding fingerprint to refer to equivalent pieces of content repeated both in the same
   file and across files. A subtlety is that when this happens, we may have a *section* with a single specific location
   referring to several sections that all differ in location - the one in the `PotentialMatchKey` is really an exemplar
   that refers to the piece of content in common to the mapped sections. Bear this in mind when debugging!
3. [matchingFingerprintsAcrossSides](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1201)
   drives the creation of matches; it works its way tail-recursively through three sorted sequences
   of `PotentialMatchKey`, one for each side, aligning the keys on either all three sides or failing that, just in
   pairs. Each alignment leads to the Cartesian product of sections associated with the aligned key across the relevant
   sides - the Cartesian product is thinned out with some overlap checking and flat-mapped through either `matchFrom` or
   more specialised helpers to make specific pairwise matches only (`matchFrom` will build either an all-sides or a
   pairwise match, depending on the context of existing larger matches). The tail recursion simply collects the matches
   found - once it terminates, it calls the
   helper [withMatches](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L814)
   to build the final `MatchingResult` for the candidate window size out of those matches. It is here
   that `withoutRedundantPairwiseMatchesIn` is used to thin out some redundant pairwise matches - this used to be done
   earlier as matches were created, but this led to some surprisingly inefficient as well as difficult to maintain code,
   so it is now done as a post-processing step. The estimate for the optimal match size is also requested here.

