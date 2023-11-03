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
      order: Order[Chromosome]
  ): Chromosome =
    given Random = new Random(initial.hashCode())

    ???
  end of
end Evolution
