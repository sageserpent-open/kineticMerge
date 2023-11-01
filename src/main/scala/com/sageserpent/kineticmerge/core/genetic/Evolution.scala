package com.sageserpent.kineticmerge.core.genetic

import cats.Order
import cats.implicits.catsKernelStdOrderForSortedSet
import com.sageserpent.kineticmerge.core.genetic.Evolution.Population

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
      population: Population[Chromosome],
      maximumPopulationSize: Int
  )(using evolution: Evolution[Chromosome]): Population[Chromosome] =
    given ordering: Ordering[Chromosome] = population.chromosomes.ordering
    given Order[Chromosome]              = Order.fromOrdering(ordering)

    given Random = new Random(population.hashCode())

    @tailrec
    def of(originals: Seq[Chromosome]): Seq[Chromosome] =

      val mutants = originals.map(evolution.mutate)

      val assimilated = originals ++ mutants

      val offspring = for
        first  <- assimilated
        second <- assimilated
        if first != second
      yield evolution.breed(first, second)

      val evolved =
        (offspring ++ assimilated).distinct
          .sorted(ordering.reverse)
          .take(maximumPopulationSize)

      println(evolved)

      Order[Seq[Chromosome]].compare(originals, evolved) match
        case value if 0 > value => of(evolved)
        case value if 0 < value => of(originals)
        case 0                  => originals
      end match
    end of

    Population(
      SortedSet(of(population.chromosomes.toSeq)*)(
        population.chromosomes.ordering
      )
    )
  end of

  case class Population[Chromosome](
      chromosomes: SortedSet[Chromosome]
  ):
    require(chromosomes.nonEmpty)

    def fittest: Chromosome = chromosomes.last
  end Population

  object Population:
    def singleton[Chromosome](chromosome: Chromosome)(using
        order: Order[Chromosome]
    ) = Population(SortedSet(chromosome)(order.toOrdering))
    end singleton
  end Population
end Evolution
