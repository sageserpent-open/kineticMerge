package com.sageserpent.kineticmerge.core.genetic

import cats.Order
import com.sageserpent.kineticmerge.core.genetic.Evolution.Population

import scala.collection.immutable.SortedSet

trait Evolution[Chromosome]:
  def mutate(chromosome: Chromosome): Chromosome
  def breed(first: Chromosome, second: Chromosome): Chromosome
end Evolution

object Evolution:
  def of[Chromosome: Evolution](
      population: Population[Chromosome],
      maximumPopulationSize: Int
  ): Population[Chromosome] = population

  case class Population[Chromosome] private (
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
