package com.sageserpent.kineticmerge.core.genetic

import cats.kernel.Order
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.randomEnrichment.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.genetic.EvolutionTest.{*, given}
import org.junit.jupiter.api.TestFactory

import scala.collection.immutable.TreeSet
import scala.util.Random

object EvolutionTest:
  type Phenotype  = Vector[Long]
  type Chromosome = Vector[Int]
  val ascendingValueSequences: Trials[Phenotype] =
    trialsApi
        .integers(1, 20)
        .flatMap(size =>
          trialsApi
      .choose(0L until 9L)
            .lotsOfSize[Vector[Long]](size)
        )
      .flatMap(increments =>
        trialsApi
          .longs(1L, 10L)
          .map(lowest => increments.scanLeft(lowest)(_ + _))
      )

  given Order[Phenotype] =
    Order.by(numberOfCorrectlyOrderedAdjacentElements)

  def numberOfCorrectlyOrderedAdjacentElements(phenotype: Phenotype) =
    phenotype.zip(phenotype.tail).count(_ <= _)
  end numberOfCorrectlyOrderedAdjacentElements

  def evolution(startingPhenotype: Phenotype) =
    new Evolution[Chromosome, Phenotype]:
      override def mutate(chromosome: Chromosome)(using
          random: Random
      ): Chromosome =
        require(1 < chromosome.size)

        if random.nextBoolean() then
          // Swap a pair of indices...
          val indexOne = random.nextInt(chromosome.size)

          val indexTwo =
            (indexOne + 1 + random.nextInt(
              chromosome.size - 1
            )) % chromosome.size

          val withTwoIndicesSwapped = chromosome
            .updated(indexOne, chromosome(indexTwo))
            .updated(indexTwo, chromosome(indexOne))

          withTwoIndicesSwapped
        else
          // Cyclic permutation...
          val rotateLeftBy = 1 + random.nextInt(chromosome.size - 1)

          val (prefix, suffix) = chromosome.splitAt(rotateLeftBy)

          suffix ++ prefix
        end if
      end mutate

      override def breed(
          first: Chromosome,
          second: Chromosome
      )(using random: Random): Chromosome =
        require(first.size == second.size)

        random.pickAlternatelyFrom(Iterable(first, second)).distinct.toVector

      end breed

      override def initialChromosome: Chromosome =
        (0 until startingPhenotype.size).toVector

      override def phenotype(chromosome: Chromosome): Phenotype =
        chromosome.map(startingPhenotype.apply)

end EvolutionTest

class EvolutionTest:
  @TestFactory
  def evolutionSticksWithTheSortedSequence(): DynamicTests =
    ascendingValueSequences
      .withLimit(1000)
      .dynamicTests { (targetSortedSequence: Phenotype) =>

        given Evolution[Chromosome, Phenotype] =
          evolution(startingPhenotype = targetSortedSequence)

        end given

        val fittest: Phenotype =
          Evolution.of(
            maximumNumberOfRetries = 1,
            maximumPopulationSize = 200
          )

        assert(
          fittest == targetSortedSequence
        )
      }
  end evolutionSticksWithTheSortedSequence

  @TestFactory
  def evolutionSelectsTheSortedSequence(): DynamicTests =
    ascendingValueSequences
      .flatMap(ascendingValueSequence =>
        trialsApi
          .indexPermutations(ascendingValueSequence.size)
          .map(ascendingValueSequence -> _.map(ascendingValueSequence.apply))
      )
      .withLimit(1000)
      .dynamicTests { (targetSortedSequence: Phenotype, mess: Phenotype) =>

        given Evolution[Chromosome, Phenotype] =
          evolution(startingPhenotype = mess)

        val fittest: Phenotype =
          Evolution.of(
            maximumNumberOfRetries = mess.size * 3,
            maximumPopulationSize = 120
          )

        assert(
          fittest == targetSortedSequence
        )
      }
  end evolutionSelectsTheSortedSequence

end EvolutionTest
