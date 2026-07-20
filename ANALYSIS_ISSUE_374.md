# Analysis of Test Failures: GitHub Issue #374 (Allow more matching overlaps)

This report details the investigation of the test failures resulting from allowing more matching overlaps of different sizes, including an in-depth analysis of the impact of blocking overlap reconciliation when resulting fragments fall below `minimumMatchSize`.

---

## Part 1: Initial Investigation (With Relaxed Overlaps)

When overlaps are relaxed, three main test categories fail. Our analysis of those failures is as follows:

### 1. Combining Two Files into a New Replacement File (`SectionedCodeExtensionTest.codeMotionAcrossTwoFiles...`)
* **Symptom:** Mismatched tokens/whitespace on merge.
* **Verdict: Semantic Outcome is Superior.** Under relaxed rules, the merge output is much cleaner. Duplicate/redundant blocks of inserted phrases like `(but you aren't going to need it)` are correctly unified rather than duplicated. However, because tokens align and consolidate more tightly, there are minor whitespace changes (such as missing linebreaks or spaces where sections are now adjacent).
* **Recommendation:** Update test expectations to accept the superior semantically-merged layout with minor whitespace/token boundary adjustments.

### 2. Migration of a Migrated Edit as an Insertion (`SectionedCodeExtensionTest.furtherMigrationOfAMigratedEdit...`)
* **Symptom:** Corrupted layout where right-side edits are spliced into the middle of unrelated words.
* **Verdict: Genuine Reconciliation Regression.** Overlap reconciliation produces tiny fragmented sections (such as a single dot `.` or newline) because different-sized matches overlap and are fragmented. These tiny sections are then treated as distinct, valid move destinations, attracting migrated edits to sub-optimal locations and corrupting the merged layout.
* **Recommendation:** The reconciliation phase needs a minimum size constraint or filtering to discard extremely short, accidental sections so they are not treated as valid anchors/move destinations for edit migration.

### 3. Pairwise Match Suppression by a Competing All-Sides Match (`SectionedCodeTest.eatenPairwiseMatches...`)
* **Symptom:** An additional `base + left` match is discovered, breaking strict count/size expectations.
* **Verdict: Legitimate Improvement.** With relaxed constraints, tracking a legitimate alignment between `base` and `left` for a fragment of text is now possible. Finding this extra match is a valid and correct outcome of the relaxed overlap rules.
* **Recommendation:** Update the test's strict expectations to accept the newly discovered pairwise match.

---

## Part 2: Impact of Blocking Reconciliation for Fragments Below `minimumMatchSize`

We implemented a blocking mechanism in `MatchAnalysis.scala` (`hiveOffNonOverlappedMatchFrom`) so that if a split/hive-off would result in hived-off or remaining contested matches below `configuration.minimumMatchSize`, the split is blocked. To prevent unresolved overlaps from triggering downstream `Overlapping section detected` runtime exceptions, matches that could not be safely split due to this minimum size constraint were cleanly suppressed (removed from the match analysis).

Here is the impact of this change across the test suite:

### 1. Downstream Safety
* **Impact:** 100% successful compile and run. All downstream `Overlapping section detected` runtime exceptions are completely resolved.

### 2. Elimination of Malformed Duplicates
* **Impact: Positive.** In `furtherMigrationOfAMigratedEditAsAnInsertion`, the sub-optimal overlap fragment that caused the malformed text (`A bird in hand is worth two in the busheat gram flour...`) was cleanly suppressed. The merge finished without corrupting the first sentence.

### 3. Loss of Legitimate Alignments & Edit Migrations
* **Impact: Negative.** By suppressing matches that fell below `minimumMatchSize` during overlap hiving-off, we also discarded many legitimate, highly valuable match alignments.
  * In `furtherMigrationOfAMigratedEditAsAnInsertion`, the right-side edit (`Better eat gram flour, not the damned flowers.`) was **not migrated at all** because the match supporting its migration was suppressed. The output defaulted back to the left-side text (`Better a gramme than a damn.`), losing the edit entirely.
  * 10 tests across `SectionedCodeExtensionTest` failed because legitimate code motions, splits, and file renames were blocked or reverted to default layouts, sometimes leading to clean merge failures (conflicts).

### Conclusion & Verdict on Blocking Reconciliation
While blocking and suppressing under-sized fragments is safe and avoids malformed duplicates, it is **too blunt of an instrument** because it throws the baby out with the bathwater. Suppressing the entire match because a single overlapped fragment is too small causes a significant loss of precision, reverting many legitimate code motion migrations and splitting alignments.

Instead of completely suppressing the match, a more surgical approach is required:
* Overlap reconciliation should proceed, but we should refine the **migration anchor/move destination selector** in `SectionedCodeExtension` to refuse tiny single-character/token fragments (like `.` or linebreaks) as eligible destinations for edit/insertion migration.
