package com.sageserpent.kineticmerge.core

import cats.Eq
import com.google.common.hash.Hashing
import com.sageserpent.kineticmerge.core.Merge.Result.MergedWithConflicts
import com.sageserpent.kineticmerge.core.MergingTextTest.{emsworth, jobsworth, merge, wordsworth}
import com.sageserpent.kineticmerge.core.PartitionedThreeWayTransform.Input
import org.junit.jupiter.api.Test
import pprint.*

class MergingTextTest:
  @Test
  def proseCanBeMerged(): Unit =
    val Right(MergedWithConflicts(leftElements, rightElements)) =
      merge(base = wordsworth, left = jobsworth, right = emsworth)(
        _ == _
      ): @unchecked

    pprintln(leftElements.mkString)
    pprintln(rightElements.mkString)
  end proseCanBeMerged
end MergingTextTest

object MergingTextTest:

  private val wordsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |Continuous as the stars that shine
      |And twinkle on the milky way,
      |They stretched in never-ending line
      |Along the margin of a bay:
      |Ten thousand saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And dances with the daffodils.
      |""".stripMargin

  private val jobsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |I thought, 'Was this part of the job role?'.
      |'Should I be expected to deal with flowers?'
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but thought only of
      |raising this in the next Zoom meeting.
      |
      |For oft, when on my Aeron I slouch
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sends an email to human resources.
      |""".stripMargin

  private val emsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of small fishing boats;
      |Astride the sea, beneath the quay,
      |Rocking and swaying in the breeze.
      |
      |Why this allusion?
      |I Havant a clue!
      |Along the margin of a bay:
      |Ten thousand (well, maybe not quite) saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sashays with the fishing boats.
      |""".stripMargin

  def merge(
      base: String,
      left: String,
      right: String
  )(
      equality: Eq[Char]
  ): Either[Merge.Divergence.type, Merge.Result[Char]] =
    def threeWayTransform(
        input: Input[Char]
    ): Either[Merge.Divergence.type, Merge.Result[Char]] =
      if input.isCommonPartition then
        Right(Merge.Result.FullyMerged(input.left))
      else Merge.of(input.base, input.left, input.right)(equality)

    PartitionedThreeWayTransform(base, left, right)(0.1, equality, elementHash)(
      threeWayTransform,
      reduction
    )
  end merge

  private def elementHash(element: Char): Array[Byte] =
    val hasher = Hashing.murmur3_32_fixed().newHasher()

    hasher.putChar(element)

    hasher.hash().asBytes()
  end elementHash

  private def reduction(
      lhs: Either[Merge.Divergence.type, Merge.Result[Char]],
      rhs: Either[Merge.Divergence.type, Merge.Result[Char]]
  ): Either[Merge.Divergence.type, Merge.Result[Char]] =
    for
      lhsMerge <- lhs
      rhsMerge <- rhs
    yield lhsMerge -> rhsMerge match
      case (
            Merge.Result.FullyMerged(lhsElements),
            Merge.Result.FullyMerged(rhsElements)
          ) =>
        Merge.Result.FullyMerged(lhsElements ++ rhsElements)
      case (
            Merge.Result.FullyMerged(lhsElements),
            Merge.Result.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        Merge.Result.MergedWithConflicts(
          lhsElements ++ rhsLeftElements,
          lhsElements ++ rhsRightElements
        )
      case (
            Merge.Result.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            Merge.Result.FullyMerged(rhsElements)
          ) =>
        Merge.Result.MergedWithConflicts(
          lhsLeftElements ++ rhsElements,
          lhsRightElements ++ rhsElements
        )
      case (
            Merge.Result.MergedWithConflicts(lhsLeftElements, lhsRightElements),
            Merge.Result.MergedWithConflicts(rhsLeftElements, rhsRightElements)
          ) =>
        Merge.Result.MergedWithConflicts(
          lhsLeftElements ++ rhsLeftElements,
          lhsRightElements ++ rhsRightElements
        )

end MergingTextTest
