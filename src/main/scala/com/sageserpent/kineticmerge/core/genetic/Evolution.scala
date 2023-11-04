package com.sageserpent.kineticmerge.core.genetic

import cats.Order
import cats.implicits.catsKernelStdOrderForSortedSet

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.Random

trait Evolution[Chromosome]:
  def mutate(chromosome: Chromosome)(using random: Random): Chromosome
  def breed(first: Chromosome, second: Chromosome)(using
      random: Random
  ): Chromosome
end Evolution

object Evolution:

  def of[Chromosome](
      initial: Chromosome,
      maximumNumberOfRetries: Int
  )(using
      evolution: Evolution[Chromosome],
      ascendingFitnessOrder: Order[Chromosome]
  ): Chromosome =
    given Random = new Random(initial.hashCode())

    val descendingFitnessOrdering = ascendingFitnessOrder.toOrdering.reverse

    def ensureMoreThanOneChromosomeInPopulation(
        population: IndexedSeq[Chromosome]
    ): IndexedSeq[Chromosome] =
      require(population.nonEmpty)
      if population.size > 1 then population
      else
        val sole = population.head
        Vector(sole, evolution.mutate(sole)).sorted(descendingFitnessOrdering)
      end if
    end ensureMoreThanOneChromosomeInPopulation

    @tailrec
    def lifecycle(
        population: IndexedSeq[Chromosome],
        numberOfRetries: Int
    ): Chromosome =
      require(1 < population.size)

      val fittestSoFar = population.head

      val populationSize = population.size

      println((populationSize, numberOfRetries))

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
                  ascendingFitnessOrder.gt(offspring, fittestSoFar)
                offspring -> improvement
              }
            )
          )
          .distinct
          .sorted(descendingFitnessOrdering)
          .take(200) // <<---- NASTY HACK!

      val noImprovement = survivingOffspring.headOption.fold(true)(
        ascendingFitnessOrder.gteqv(fittestSoFar, _)
      )

      if noImprovement && maximumNumberOfRetries == numberOfRetries
      then fittestSoFar
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
      Vector(initial)
    )

    lifecycle(
      initialPopulation,
      numberOfRetries = 0
    )
  end of
end Evolution
