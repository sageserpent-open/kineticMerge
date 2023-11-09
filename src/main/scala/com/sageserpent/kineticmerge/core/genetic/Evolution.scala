package com.sageserpent.kineticmerge.core.genetic

import cats.Order
import cats.implicits.catsKernelStdOrderForSortedSet

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.Random

trait Evolution[Chromosome, Phenotype]:
  def mutate(chromosome: Chromosome)(using random: Random): Chromosome
  def breed(first: Chromosome, second: Chromosome)(using
      random: Random
  ): Chromosome
  def initialChromosome: Chromosome
  def phenotype(chromosome: Chromosome): Phenotype
end Evolution

object Evolution:

  def of[Chromosome, Phenotype](
      maximumNumberOfRetries: Int,
      maximumPopulationSize: Int
  )(using
      evolution: Evolution[Chromosome, Phenotype],
      ascendingFitnessOrder: Order[Phenotype]
  ): Phenotype =
    val initialChromosome = evolution.initialChromosome

    given Random = new Random(initialChromosome.hashCode())

    val ascendingChromosomeFitnessOrder =
      Order.by[Chromosome, Phenotype](evolution.phenotype)

    val descendingChromosomeFitnessOrdering =
      ascendingChromosomeFitnessOrder.toOrdering.reverse

    @tailrec
    def lifecycle(
        populationChromosomes: IndexedSeq[Chromosome],
        fittestChromosome: Chromosome,
        numberOfRetries: Int,
        maximumNumberOfRetries: Int
    ): Chromosome =
      require(1 < populationChromosomes.size)

      val populationSize = populationChromosomes.size

      val maximumRank = populationSize - 1

      val ranks = 0 to maximumRank

      val diagonalStripes = ranks.flatMap(rank =>
        ((1 + rank) / 2 to rank).map(indexAlongStripe =>
          indexAlongStripe -> (rank - indexAlongStripe)
        )
      ) ++ ranks.reverse
        .drop(1)
        .flatMap(rank =>
          ((1 + rank) / 2 to rank).map(indexAlongStripe =>
            (maximumRank - indexAlongStripe) -> (maximumRank - (rank - indexAlongStripe))
          )
        )

      val offspringIterator =
        LazyList
          .from(diagonalStripes)
          .flatMap((rowIndex, columnIndex) =>
            val first  = populationChromosomes(rowIndex)
            val second = populationChromosomes(columnIndex)

            val offspring = evolution.breed(first, second)
            val mutant    = evolution.mutate(offspring)

            Seq(offspring, mutant)
          )
          .iterator
          .distinct

      val offspringChromosomes =
        Vector
          .from(
            Iterator.unfold(false)(hasImproved =>
              Option.unless(hasImproved || !offspringIterator.hasNext) {
                val offspring = offspringIterator.next()
                val improvement =
                  ascendingChromosomeFitnessOrder
                    .gt(offspring, fittestChromosome)
                offspring -> improvement
              }
            )
          )
          .sorted(descendingChromosomeFitnessOrdering)
          .take(maximumPopulationSize)

      val fittestOffspringChromosome = offspringChromosomes.head

      val noImprovement = ascendingChromosomeFitnessOrder.lteqv(
        fittestOffspringChromosome,
        fittestChromosome
      )

      if noImprovement && maximumNumberOfRetries == numberOfRetries
      then fittestChromosome
      else
        if noImprovement then
          lifecycle(
            populationChromosomes = offspringChromosomes,
            fittestChromosome, // The previous record holder still stands.
            numberOfRetries = 1 + numberOfRetries,
            maximumNumberOfRetries = maximumNumberOfRetries
          )
        else
          lifecycle(
            populationChromosomes = offspringChromosomes,
            fittestChromosome =
              fittestOffspringChromosome, // All hail the new pretender.
            numberOfRetries = 0,
            maximumNumberOfRetries =
              if 0 == numberOfRetries then maximumNumberOfRetries
              else
                // Increase the maximum number of retries in anticipation of the
                // next improvement being harder to find.
                maximumNumberOfRetries + numberOfRetries
          )
        end if
      end if
    end lifecycle

    val initialPopulationChromosomes =
      Vector(initialChromosome, evolution.mutate(initialChromosome))
        .sorted(descendingChromosomeFitnessOrdering)

    val fittestChromosome = lifecycle(
      populationChromosomes = initialPopulationChromosomes,
      fittestChromosome = initialPopulationChromosomes.head,
      numberOfRetries = 0,
      maximumNumberOfRetries = maximumNumberOfRetries
    )

    evolution.phenotype(fittestChromosome)
  end of
end Evolution
