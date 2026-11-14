# Parallel Matches Groups & Block Construction

A central innovation in Kinetic Merge is the representation of code motion via **Parallel Matches Groups** and the
construction of **Blocks**. This document details how parallel matches groups are formed, how bridging between
all-sides and pairwise matches is validated, and how files are partitioned into contiguous blocks for scalable
alignment.

---

## 1. Motivation: Why Parallel Matches?

Code refactoring rarely moves a single isolated token or expression. Typically:

- An entire class or function moves, containing internal sections that matched across all three sides interspersed with
  minor edits or local deletions.
- A block of code is moved, and a comment or statement within it was modified on one branch.
- Multiple related methods are rearranged together.

Treating every match independently loses the collective context of the move. More seriously, calculating the LCS
directly on fine-grained sections when large swathes have moved in parallel suffers from a combinatorial explosion of
effort.

A **Parallel Matches Group** (`ParallelMatchesGroupId`) binds together a sequence of matches that move together in the
same direction and order across participating sides.

---

## 2. Unification into Groups of Parallel Matches

In `MatchAnalysis.groupsOfParallelMatches`, matches are unified into groups using `cats.collections.DisjointSets`:

### In-Order Match Pairs

For each side and path, `precedingAndSucceedingMatchPairsFrom` scans the sorted sections of `SectionsSeen`. Whenever a
section from match `M1` is immediately followed by a section from match `M2` on that side (with
`M1.startOffset < M2.startOffset`), the pair `(M1, M2)` is recorded.

Intersections of these pairs across sides establish parallel continuity:

1. **Parallel All-Sides Pairs**: pairs present across base, left, and right
   (`basePairs intersect leftPairs intersect rightPairs`). Each pair is of all-sides matches that succeed each
   other in the same order across base, left, and right. These are unified directly via `DisjointSets.union`.
2. **Parallel Pairwise Pairs**: pairs present across just two sides. Each pair is of pairwise matches of the same kind
   that succeed each other across their shared sides. These are also unified directly.

### Bridging Pairs: Sinks, Sources, and Sandwiches

Real-world code motion frequently alternates between all-sides matches and pairwise matches. For instance, when a moved
method contains an edit on the left, an all-sides match is followed by a base-and-right pairwise match (the unedited
original) and then another all-sides match.

Kinetic Merge evaluates candidate **bridging pairs** between all-sides matches and pairwise matches:

- **Sink Bridge**: an all-sides match leading into a pairwise match.
- **Source Bridge**: a pairwise match leading into an all-sides match.
- **Sandwich Structure**: a pairwise match sandwiched between an all-sides predecessor and an all-sides successor:

  `AllSidesMatch_1 -> PairwiseMatch -> AllSidesMatch_2`

  A sandwich is valid if and only if `AllSidesMatch_1` directly leads to `AllSidesMatch_2` on the side missing from the
  pairwise match.
- Surviving bridges are used to further unify the parallel matches groups; so in our example above, whatever groups the
  flanking all-sides matches belong to would be unified together with that of the sandwiched pairwise match, making one
  large group.

### Invariants of a Parallel Matches Group

Every group of parallel matches must satisfy:

1. **Monotonicity**: matches within a group strictly march upward in start offsets across all shared participating
   sides without collisions.
2. **Non-overlapping**: no two matches within the same group overlap on any side.
3. **No alien matches**: on each side, all sections between the minimum start offset and maximum end offset of a group
   must either belong to that group or be gap fillers. It is permitted for multiple groups to share some or all of the
   same sections, but one group cannot interpose within another's sections.

---

## 3. Constructing Blocks (`SectionedCode.Block`)

Once parallel match groups are formed, `SectionedCode.of` condenses each file into an indexed sequence of contiguous
blocks:

```scala
case class Block[Element](
                           parallelMatchesGroupIds: SortedSet[ParallelMatchesGroupId],
                           sectionsCoveredByGroup: IndexedSeq[Section[Element]]
                         )
```

For each side (`base`, `left`, `right`) and file path:

### Step 1: Spanning Matched Blocks

For every parallel matches group present in the file:

- The earliest section start offset and latest section end offset covered by the group in this file are determined.
- The file's section breakdown is sliced between these indices (`startingSectionIndex` to `endingSectionIndex`).
- This forms a block covering all sections in that span, tagged with the `parallelMatchesGroupId`.

### Step 2: Inserting Filler Blocks

Any gaps in the file not covered by matched blocks are identified. For each gap between matched blocks (and at the start
and end of the file), a **filler block** is created:

- Its `sectionsCoveredByGroup` contains the contiguous sequence of gap sections.
- Its `parallelMatchesGroupIds` is empty (`SortedSet.empty`).

### Step 3: Sorting and Condensing Coincident Blocks

All matched blocks and filler blocks for the file are sorted primarily by `startOffset`. For blocks sharing the same
start offset, a descending order on `onePastEndOffset` is used:

```scala
blocks.sortBy(block => (block.startOffset, -block.onePastEndOffset))
```

Adjacent blocks covering the exact same token interval `[startOffset, onePastEndOffset)` are condensed together:

- Their `parallelMatchesGroupIds` sets are united.
- Their underlying section sequence is retained.

### Step 4: Eliminating Following Nested Blocks

If a smaller block is entirely nested within an earlier enclosing block (`outer.startOffset <= inner.startOffset` and
`outer.onePastEndOffset >= inner.onePastEndOffset`), the inner nested block is eliminated via a tail-recursive pass
(`eliminateFollowingNestedBlocks`). Overlapping blocks spanning different ranges are permitted.

---

## 4. Outcome

The resulting sequences (`baseBlocksFor(path)`, `leftBlocksFor(path)`, `rightBlocksFor(path)`):

- Form an exact, contiguous partition of each file.
- Compress thousands of individual tokens and sections into a modest number of coherent blocks.
- Enable the **Two-Tier Approximate LCS** algorithm to align files rapidly and accurately.
