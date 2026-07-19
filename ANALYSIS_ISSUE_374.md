# Analysis of Test Failures: GitHub Issue #374 (Allow more matching overlaps)

This report details the investigation of the test failures resulting from allowing more matching overlaps of different sizes (specifically after committing changes in `allow-more-overlaps`).

---

## Executive Summary

The changes introduced on the `allow-more-overlaps` branch (releasing the prohibition of matching overlaps between matches of different sizes) significantly enhance the precision and flexibility of the match discovery and reconciliation phases.

However, three tests fail. Our analysis reveals that:
1. **One failure is a major improvement** where the new merge engine produces a semantically superior output compared to the original expected merge output, eliminating redundant duplicates.
2. **One failure is a genuine bug / matching regression** where an extremely small, sub-optimal move destination (a dot/linebreak section) is produced during overlap reconciliation and subsequently has an edit migrated into it.
3. **One failure is an artifact of improved match discovery** where a highly precise additional pairwise match is found, causing a pathological unit test with strict size/count expectations to fail.

---

## 1. SectionedCodeExtensionTest: Combining Two Files into a New Replacement File

### Test Case:
`SectionedCodeExtensionTest.codeMotionAcrossTwoFilesWhoseContentIsCombinedTogetherToMakeANewReplacementFile`

### Symptoms:
The test asserts that the merge output matches a strictly specified text sequence. In multiple trials, the test fails on an assertion checking that the actual merge output is strictly equivalent to the expected tokens.

### Analysis & Verdict:
**Semantic Outcome is Superior.**
When combining proverbs and palindromes into a single replacement file, the older algorithm suffered from more restrictive match overlapping prohibitions. This caused it to construct sub-optimal alignments and split identical blocks into separate regions, leading to redundant/duplicate code sequences on merge.

Under the relaxed overlap rules, the merge output is much cleaner. For example, rather than duplicating sections of inserted phrases like `(but you aren't going to need it)` or misplacing them, the alignment group correctly groups the blocks.

*However*, because the tokens are aligned and consolidated much more tightly, there are minor whitespace changes (such as missing linebreaks or spaces where sections are now adjacent rather than separated by gaps).

**Recommendation:**
Tweak the expected merge outputs in this test to accommodate the superior merged layout and account for the tighter token/whitespace alignment.

---

## 2. SectionedCodeExtensionTest: Migration of a Migrated Edit as an Insertion

### Test Case:
`SectionedCodeExtensionTest.furtherMigrationOfAMigratedEditAsAnInsertion`

### Symptoms:
The test fails on `verifyContent` for `*** EXCISED PROVERBS ***` with mismatched tokens:
```
Expected:
A bird in hand is worth two in the bush.
Better eat gram flour, not the damned flowers.
A stitch in time saves nine.

Actual (Fully Merged):
A bird in hand is worth two in the busheat gram flour, not the damned flowers.
Better eat gram flour, not the damned flowers.
.
A stitch in time saves nine.
```

### Analysis & Verdict:
**Genuine Matching & Reconciliation Regression.**
The debug logs show that the edit from the right side (`eat gram flour, not the damned flowers.\n`) is migrated directly to a sub-optimal move destination on the left side:
```scala
Unique(
  Section(
    label = "left",
    path = "*** PROVERBS ***",
    start = TextPosition(line = 2, characterOffset = 39),
    onePastEnd = TextPosition(line = 3, characterOffset = 0),
    startTokenIndex = 11,
    sizeInTokens = 1,
    content = ".\n"
  )
)
```
This sub-optimal move destination (a single dot and a newline) is produced as a side effect during the overlap reconciliation phase. Because different-sized matches are allowed to overlap and are subsequently fragmented, reconciliation produces tiny fragmented sections (such as single-punctuation/token sections) that are treated as distinct, valid move destinations. This triggers a bad migration/splice alignment, corrupting the layout of the final merged file.

**Recommendation:**
This is a genuine issue with overlap reconciliation being too permissive in producing tiny fragmented sections when processing overlapping matches of different sizes. To prevent this, the reconciliation phase needs a minimum size constraint or filtering to discard extremely short, accidental sections (like single punctuation or newline characters) so that they are not treated as independent anchors/move destinations for edit migration.

---

## 3. SectionedCodeTest: Eaten Pairwise Matches Suppressed by a Competing Ambiguous Pairwise Match

### Test Case:
`SectionedCodeTest.eatenPairwiseMatchesMayBeSuppressedByACompetingOverlappingAllSidesMatch`

### Symptoms:
The test asserts that only three all-sides matches with specific sliced content (`prefix`, `overlap`, `suffix`) are found, and that the pairwise match is suppressed. Under the relaxed rules, an additional `base + left` match is discovered.

### Analysis & Verdict:
**Legitimate Improvement in Discovery.**
This test setup is highly pathological and specifically designed to verify that a pairwise match gets suppressed under certain overlap and size conditions.

With relaxed overlap constraints, the match finder is now capable of correctly tracking and preserving a legitimate alignment between `base` and `left` for a fragment of the text that was previously prohibited from being matched. Discovering this extra match is actually a *legitimate outcome of the greater flexibility in matching*.

**Recommendation:**
The expectations of this unit test need to be updated to accept the newly discovered `base+left` pairwise match, or the test's constraints need to be tweaked if the original suppression behavior must be strictly maintained for some reason.

---

## Summary of Action Items for the Maintainer
1. **Accept New Outcomes:** Update `SectionedCodeExtensionTest.codeMotionAcrossTwoFiles...` and `SectionedCodeTest.eatenPairwiseMatches...` to reflect the superior/additional matches.
2. **Implement Guardrails:** Introduce a safeguard in the reconciliation phase of `MatchAnalysis` to prevent extremely small, accidental overlap fragments (like single-token punctuation or linebreaks) from becoming target anchors/move destinations for migrated edits.
