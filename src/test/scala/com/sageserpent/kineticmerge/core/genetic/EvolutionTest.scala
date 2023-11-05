package com.sageserpent.kineticmerge.core.genetic

import cats.kernel.Order
import com.sageserpent.americium.Trials
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.TestFactory

import scala.collection.immutable.TreeSet
import scala.util.Random

class EvolutionTest:
  // TODO - suppose we start with an initial chromosome that is the optimal one?
  // Need a test for that...
  @TestFactory
  def evolutionSelectsTheSortedSequence(): DynamicTests =
    type Phenotype = Vector[Long]

    val ascendingValueSequences: Trials[Phenotype] =
      trialsApi
        .choose(0L until 9L)
        .several[Vector[Long]]
        .filter(_.nonEmpty)
        .flatMap(increments =>
          trialsApi
            .longs(1L, 10L)
            .map(lowest => increments.scanLeft(lowest)(_ + _))
        )

    ascendingValueSequences
      .flatMap(ascendingValueSequence =>
        trialsApi
          .indexPermutations(ascendingValueSequence.size)
          .map(ascendingValueSequence -> _.map(ascendingValueSequence.apply))
      )
      .withLimit(1000)
      .dynamicTests { (targetSortedSequence: Phenotype, mess: Phenotype) =>
        type Chromosome = Vector[Int]

        given Evolution[Chromosome, Phenotype] with
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

            val firstMapsToEven = random.nextBoolean()

            val chosenUniqueValues = (first zip second).map {
              (firstIndex, secondIndex) =>
                val chooseTheFirst = random.nextBoolean()

                if chooseTheFirst then
                  if firstMapsToEven then 2 * firstIndex
                  else 2 * firstIndex + 1
                else if firstMapsToEven then 2 * secondIndex + 1
                else 2 * secondIndex
                end if
            }

            val translationToNewPermutationIndices =
              chosenUniqueValues.sorted.zipWithIndex.toMap

            chosenUniqueValues.map(translationToNewPermutationIndices.apply)
          end breed

          override def initialChromosome: Chromosome =
            (0 until mess.size).toVector

          override def phenotype(chromosome: Chromosome): Phenotype =
            chromosome.map(mess.apply)

        end given

        def numberOfCorrectlyOrderedAdjacentElements(phenotype: Phenotype) =
          phenotype.zip(phenotype.tail).count(_ <= _)
        end numberOfCorrectlyOrderedAdjacentElements

        given Order[Phenotype] =
          Order.by(numberOfCorrectlyOrderedAdjacentElements)

        val fittest: Phenotype =
          Evolution.of(
            maximumNumberOfRetries = mess.size * 3,
            maximumPopulationSize = 200
          )

        assert(
          fittest == targetSortedSequence
        )
      }
  end evolutionSelectsTheSortedSequence
end EvolutionTest
