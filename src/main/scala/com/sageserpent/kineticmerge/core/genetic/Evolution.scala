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

    def ensureMoreThanOneChromosomeInPopulation(
        population: IndexedSeq[Chromosome]
    ): IndexedSeq[Chromosome] =
      require(population.nonEmpty)
      if population.size > 1 then population
      else
        val sole = population.head
        Vector(sole, evolution.mutate(sole))
          .sorted(descendingChromosomeFitnessOrdering)
      end if
    end ensureMoreThanOneChromosomeInPopulation

    @tailrec
    def lifecycle(
        population: IndexedSeq[Chromosome],
        numberOfRetries: Int
    ): Chromosome =
      require(1 < population.size)

      val fittestChromosome = population.head

      val populationSize = population.size

      println((numberOfRetries, populationSize, population.take(5)))

      val ranks = 0 until populationSize

      val diagonalStripes = ranks.flatMap(rank =>
        ((1 + rank) / 2 to rank).map(indexAlongStripe =>
          indexAlongStripe -> (rank - indexAlongStripe)
        )
      )

      val offspringIterator =
        LazyList
          .from(diagonalStripes)
          .flatMap((rowIndex, columnIndex) =>
            val first  = population(rowIndex)
            val second = population(columnIndex)

            val offspring = evolution.breed(first, second)
            val mutants =
              Seq.fill(1 + numberOfRetries)(evolution.mutate(offspring))

            offspring +: mutants
          )
          .iterator
          .distinct

      val survivingOffspring =
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
          .distinct
          .sorted(descendingChromosomeFitnessOrdering)
          .take(maximumPopulationSize)

      val noImprovement = survivingOffspring.headOption.fold(true)(
        ascendingChromosomeFitnessOrder.gteqv(fittestChromosome, _)
      )

      if noImprovement && maximumNumberOfRetries == numberOfRetries
      then fittestChromosome
      else
        // The new generation takes over...
        val ongoingPopulation = ensureMoreThanOneChromosomeInPopulation(
          survivingOffspring
        )
        lifecycle(
          ongoingPopulation,
          numberOfRetries = if noImprovement then 1 + numberOfRetries else 0
        )
      end if
    end lifecycle

    val initialPopulation = ensureMoreThanOneChromosomeInPopulation(
      Vector(initialChromosome)
    )

    val fittestChromosome = lifecycle(
      initialPopulation,
      numberOfRetries = 0
    )

    evolution.phenotype(fittestChromosome)
  end of
end Evolution
