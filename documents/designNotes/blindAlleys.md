# Genetic Algorithm for Matching #

`CodeMotionAnalysis` used to use a genetic algorithm to drive the discovery of matches. A small framework for
implementing genetic algorithms was written and debugged against its unit tests, using a model problem of sorting random
integers into ascending order.

This code still exists, excised into its own project that shares commit history for that of Kinetic Merge -
see [actinium](https://github.com/sageserpent-open/actinium).

It worked after a fashion - although Hamming Walls were a real concern for the model problem and were likely occurring
in the determination of optimal matches by `CodeMotionAnalysis`.

Here's
the [original plan to use a genetic algorithm](https://github.com/sageserpent-open/kineticMerge/issues/19#issuecomment-1806822492),
here's the
[thinking about pursuing a systematic search to do the heavy lifting](https://github.com/sageserpent-open/kineticMerge/issues/19#issuecomment-1855867054)
and here's the decision
to [get rid of it altogether](https://github.com/sageserpent-open/kineticMerge/issues/19#issuecomment-1875093907).

Something that wasn't explored was the first possibility
from [thinking about pursuing a systematic search to do the heavy lifting](https://github.com/sageserpent-open/kineticMerge/issues/19#issuecomment-1855867054).

This would have had a chromosome directly encode the matches, the crossover and mutation operations allowing additional
matches to be acquired, removed, coalesced, expanded and contracted. This was the original implementation plan, but once
I'd written `PartitionedThreeWayTransform` and used Rabin fingerprinting, it seemed natural to encode window sizes
instead in the chromosome representation.

So there may still be life in this idea yet! To anyone who fancies trying this out, my advice is - read up on Hamming
Walls and be aware that your chromosome representation and crossover / mutation approach will have to deal with them.
Getting [EvolutionTest](https://github.com/sageserpent-open/actinium/blob/main/src/test/scala/com/sageserpent/actinium/EvolutionTest.scala)
to pass involved several attempts with varying chromosome representations, not all of which have been kept in the
history - there were a lot of blind alleys there. Genetic algorithms are *not* a silver bullet!

# Rabin Fingerprinting #

Fingerprinting of content is a key part of finding matches in `CodeMotionAnalysis`. The initial implementation
used [Rabin Fingerprinting](https://github.com/themadcreator/rabinfingerprint), brought in as direct source dependency
and compiled as a subproject of the overall SBT project - that repository does not publish Maven builds, and I didn't
want to get into maintenance of all the third party codebase, just the bits required for Kinetic Merge.

It worked very well - in some respects, too well, because the collision resistance masked a bug in `CodeMotionAnalysis`
provoked by fingerprint collisions between unrelated content. What did for it was the substantial overhead of
precomputing the polynomial representation that is required to bootstrap a fingerprinting session.

Using a classic rolling hash algorithm, as would typically be used by the Rabin-Karp search algorithm (which is *not*
the same as Rabin fingerprinting), yielded far better performance at the expense of encountering collisions - trying
this out was the point at which the aforementioned collision but was discovered and fixed.

If collision resistance is a primary concern though, Rabin fingerprinting is worth looking at in more detail.

# PartitionedThreeWayTransform #

This was a stop-gap measure when an attempt was made to launch Kinetic Merge in an interim release that would do just
three-way merging without code motion. The intent was to get something out there and produce a robust application shell
into the bargain. `CodeMotionAnalysis`, `File`, `Section` and `Sources` were temporarily removed from the codebase. This
hit
severe [performance snags](https://github.com/sageserpent-open/kineticMerge/issues/1#issuecomment-1720910504)
straightaway, leading to abandoning merging over characters in favour of merging over tokens. That helped, but wasn't
enough to scale beyond quite small files, so `PartitionedThreeWayTransform` was devised as a way of breaking down the
three-ways merges into something tractable for `merge.of`.

The idea was to capitalise on changes on either side of a merge being localised to small sections of the original base
content; thus most of the merge could be implemented by copying through common content. `PartitionedThreeWayTransform`
looked for a run of common content between the base, left and right content, using the common content to partition each
side's content into a prefix, the common content and a suffix. This partitioning was repeated recursively for the
prefixes across the sides and likewise the suffices, leading to an implied decomposition at runtime into small slices of
content that did not match across all three sides, and common slices that did.

A three-way transform was applied to the disagreeing slices - this being the three-way merge in non-test code, and the
results of the merges were combined with the common content to yield an overall three-way merge.

This approach worked well, leading to [the first releases](https://github.com/sageserpent-open/kineticMerge/issues/15).

`PartitionedThreeWayTransform` used Rabin fingerprinting to find common content across the sides. Given that it was
carried out in the context of getting an interim release out, once `CodeMotionAnalysis` and its friends were reinstated,
it was excised out in Git commit SHA: 17105a839997c1bb65b25c5039ea78d547884dcd - but its use legacy lives on in terms of
the code
in [`matchesForWindowSize`](https://github.com/sageserpent-open/kineticMerge/blob/63ea2b5cf44d553bf9d49412cc321fc219874d9a/src/main/scala/com/sageserpent/kineticmerge/core/CodeMotionAnalysis.scala#L1095).
initially being a crib from `PartitionedThreeWayTransform`, albeit with a lot more baggage added since then.