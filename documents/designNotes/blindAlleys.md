# Blind Alleys

During the design and evolution of Kinetic Merge, several architectures, algorithms, and optimization strategies were
explored, implemented, benchmarked, and ultimately discarded. Recording these blind alleys prevents future maintainers
from repeating past mistakes.

---

## 1. Genetic Algorithm for Match Discovery

`SectionedCode` originally attempted to find optimal matches across three sides using a genetic algorithm. A custom
genetic algorithm framework was implemented and tested (now excised into the standalone library
[actinium](https://github.com/sageserpent-open/actinium)).

### Why it was discarded:
- **Hamming Walls**: In practice, small mutations to window sizes or candidate match lists repeatedly hit Hamming Walls,
  getting trapped in local optima where no single-gene mutation could reach the globally optimal match configuration.
- **Unpredictable Latency**: Run times varied wildly across runs, requiring excessive generations to converge even on
  modest files.
- **Replacement**: Iterated binary search for large matches combined with exhaustive linear scan for smaller matches
  guarantees finding maximal matches deterministically in predictable time.

---

## 2. Rabin Fingerprinting

The initial rolling hash implementation used algebraic Rabin fingerprinting over irreducible polynomials
([rabinfingerprint](https://github.com/themadcreator/rabinfingerprint)).

### Why it was discarded:
- **Initialization Cost**: Generating and calculating operations over irreducible polynomials incurred significant
  startup and CPU overhead.
- **False Sense of Security**: Rabin fingerprinting's near-zero collision rate masked bugs in downstream code that
  assumed hash equality implied content equality.
- **Replacement**: A classic rolling polynomial hash (`RollingHash`) proved order-of-magnitude faster. Hash collisions
  are handled explicitly and safely using `PotentialMatchKey` tie-breaking.

---

## 3. `PartitionedThreeWayTransform`

An interim release attempted three-way merging without code motion by recursively partitioning files around common runs
of content across all three sides (`PartitionedThreeWayTransform`).

### Why it was discarded:
- **Fragility**: Local edits on either side immediately broke the alignment of surrounding common swathes, causing
  cascading recursive splits and poor merge decisions.
- **Replacement**: The core architecture reinstated full sectioned match analysis, blocks, and longest common
  subsequences.

---

## 4. Concurrent Futures and Cats Effect IO in Dynamic Programming

Earlier versions of `LongestCommonSubsequence.of` parallelized the 3D dynamic programming grid using Scala `Future`s and
Cats Effect `IO` to compute sub-problems concurrently within wavefront swathes.

### Why it was discarded:
- **Allocation and Scheduling Overhead**: Allocating thousands of `Future` / `IO` instances, thread context switches,
  and monadic binds vastly outweighed the microsecond cost of cell comparisons in the DP table.
- **GC Pressure**: The high rate of task allocations triggered frequent garbage collection pauses.
- **Replacement**: A purely sequential, swathe-based dynamic programming loop using direct zero-allocation comparisons,
  three-index affix trimming (`commonPrefixContributions` / `commonSuffixContributions`), and matchability filtering.

---

## 5. Direct 3D LCS Over Whole Files

An early approach ran exact 3-way longest common subsequence dynamic programming directly across all sections of a file.

### Why it was discarded:
- **Complexity Explosion**: For files with hundreds of sections, a 3D table ($O(N^3)$) consumed hundreds of megabytes of
  RAM and several minutes of CPU time per file.
- **Misaligned Crossovers**: Minor ambiguous token repeats caused the global DP to jump across moved code blocks,
  creating unnatural, scattered alignments.
- **Replacement**: The **Two-Tier Approximate LCS** architecture. Files are partitioned into coarse-grained `Block`s,
  merged coarsely in Tier 1, and only locally refined within non-overlapping clumps in Tier 2.

---

## 6. Whole-File Slices for Splices

Earlier iterations of splice migration attempted to scan preceding and succeeding sections across the entire file
around every moved anchor, using binary search and slicing over file sections.

### Why it was discarded:
- **Collision & Overlap**: Scanning outward without boundaries caused splices from different moves to overlap or
  duplicate content.
- **Replacement**: Grouping anchored moves by `ParallelMatchesGroupId`. Intermediate splices are extracted cleanly
  between adjacent moves ($M_i$ to $M_{i+1}$) within the group, while boundary scanning is restricted to group margins.
