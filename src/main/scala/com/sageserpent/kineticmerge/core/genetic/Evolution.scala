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
      testBudget: Int
  )(using
      evolution: Evolution[Chromosome],
      ascendingFitnessOrder: Order[Chromosome]
  ): Chromosome =
    given Random = new Random(initial.hashCode())

    val descendingFitnessOrdering = ascendingFitnessOrder.toOrdering.reverse

    // PLAN: evolve a population that is a sorted set of chromosomes in
    // descending order of fitness. Each cycle improves the population by
    // breeding the fittest individuals and their mutations to make a stream of
    // offspring, expanding the offspring population until one of them beats the
    // fittest chromosome from the previous cycle.

    // This terminates when the fittest chromosome mutates to worse chromosomes
    // each time over the test budget.

    // TWEAKS:
    // 1. Use a diagonal strategy to breed chromosomes.
    // 2. Prevent the population from collapsing by adding mutants.
    // 3. Ratchet the population size.

    def ensureMoreThanOneChromosomeInPopulation(
        population: Seq[Chromosome]
    ): Seq[Chromosome] =
      require(population.nonEmpty)
      if population.size > 1 then population
      else
        val sole = population.head
        Seq(sole, evolution.mutate(sole))
      end if
    end ensureMoreThanOneChromosomeInPopulation

    @tailrec
    def lifecycle(population: Seq[Chromosome]): Chromosome =
      require(1 < population.size)

      val fittestSoFar = population.head

      val fittestCohort =
        population.takeWhile(ascendingFitnessOrder.eqv(_, fittestSoFar))

      val fittestCannotBeBeatenInTestBudget =
        Iterator
          .fill(testBudget)(fittestSoFar)
          .map(evolution.mutate)
          .filterNot(fittestCohort.contains)
          .forall(ascendingFitnessOrder.lt(_, fittestSoFar))

      if fittestCannotBeBeatenInTestBudget then fittestSoFar
      else
        val offspringIterator = LazyList
          .from(population)
          .flatMap(first =>
            LazyList
              .from(population)
              .flatMap(second =>
                if first != second then
                  val nonMutant = evolution.breed(first, second)
                  Seq(nonMutant, evolution.mutate(nonMutant))
                else Seq.empty
              )
          )
          .iterator
          .distinct

        val survivingOffspring =
          Seq
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
            .sorted(descendingFitnessOrdering)

        if survivingOffspring.isEmpty then
          // Keep trying...
          lifecycle(population)
        else
          // The new generation takes over...
          lifecycle(ensureMoreThanOneChromosomeInPopulation(survivingOffspring))
        end if
      end if
    end lifecycle

    lifecycle(ensureMoreThanOneChromosomeInPopulation(Seq(initial)))
  end of
end Evolution
