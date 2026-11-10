# Skeletons in the Cupboard

Every mature, real-world software system contains pragmatic compromises, stop-gap solutions, and areas of technical
debt that work effectively but deserve careful attention. Here are the skeletons in Kinetic Merge's cupboard.

---

## 1. Gap Chunks in `Sources.filesByPathUtilising`

When breaking down files into contiguous sections, `Sources.filesByPathUtilising` takes optional
`candidateGapChunksByPath`:

```scala
def filesByPathUtilising(
    mandatorySections: Set[Section[Element]],
    candidateGapChunksByPath: Map[Path, Set[IndexedSeq[Element]]] = Map.empty
): Map[Path, File[Element]]
```

### The Pragmatic Hack
Because matches must clear minimum window sizes to be discovered, small runs of content (such as single tokens or short
expressions) can fail to match. When an edit on one side preserves a short piece of base text, the base text in the
other side's gap could remain unaligned.

`SectionedCode` breaks down the base sources first and extracts unmatched gap sections. These are passed as candidate gap
chunks to the left and right breakdowns. If a gap chunk matches text within a gap on the left or right, the gap section
is split around the matched chunk.

### Why it is a Skeleton
It uses two separate components (`Sources` breakdown and `SectionedCode`) to coincidentally coordinate match discovery
outside the regular match pipeline. While it effectively prevents poor merges on small edits, a more unified multi-scale
matching engine would be cleaner.

---

## 2. Lookahead Scanning for Marooned Edits and Deletions in `merge.scala`

The core three-way merge algorithm in `merge.scala` walks monotonically through contributions from the longest common
subsequence. Ideally, each step would depend only on the immediate head of the base, left, and right contributions.

However, to prevent edits or deletions from being prematurely resolved before a coincident insertion or deletion has
passed, `merge.scala` performs bounded lookahead scanning:
- `rightEditNotMaroonedByPriorCoincidentInsertion`
- `leftEditNotMaroonedByPriorCoincidentInsertion`
- `rightEditNotMaroonedByPriorLeftDeletion`
- `leftEditNotMaroonedByPriorRightDeletion`

### Why it is a Skeleton
Lookahead breaks the pure local stream processing of the merge algebra. While bounded and fast in practice, it is an
ad-hoc mechanism to compensate for alignment ambiguities in the LCS output.

---

## 3. Strict Synchronization in `LongestCommonSubsequence`

`LongestCommonSubsequence.apply` enforces strict synchronization: if corresponding 'common' contributions from different
sides have mismatched content or inconsistent counts, it throws an `IllegalArgumentException("Failed to synchronise...")`.

### Why it is a Skeleton
Any flaw in upstream block construction, clump coalescence, or crossover elimination manifests not as an ordinary
merge conflict, but as a hard synchronization exception. Crossover filtering (`matchesCross` and meta-LCS) was introduced
specifically to guard against this invariant violation.

---

## 4. Single Ancestor Assumption

Kinetic Merge currently assumes a single best common ancestor commit (`merge-base`). When branches have criss-cross
merge histories with multiple virtual common ancestors, Kinetic Merge selects one rather than recursively pre-merging
them (as Git's `merge-ort` does).
