package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.{DynamicTests, Syntax}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MoveDestinationsTest.newUniqueFakeLocation
import org.junit.jupiter.api.{Test, TestFactory}

import java.util.UUID

object MoveDestinationsTest:
  def newUniqueFakeLocation(): UUID = UUID.randomUUID()
end MoveDestinationsTest

class MoveDestinationsTest:
  @TestFactory
  def aSingleMoveOnOneSideIsNeitherAmbiguousNorDivergent: DynamicTests =
    // NOTE: need to define at least one source, because the invariant for
    // `MoveDestinations` forbids a degenerate instance from modelling a move to
    // just one side.
    val suts = Trials.api
      .choose(
        MoveDestinations(
          left = Set(newUniqueFakeLocation()),
          right = Set.empty,
          coincident = Set.empty
        ),
        MoveDestinations(
          left = Set.empty,
          right = Set(newUniqueFakeLocation()),
          coincident = Set.empty
        )
      )

    suts
      .withLimit(10)
      .dynamicTests { sut =>
        assert(!sut.isAmbiguous)
        assert(!sut.isDivergent)

        println(sut.description)
      }
  end aSingleMoveOnOneSideIsNeitherAmbiguousNorDivergent

  @TestFactory
  def multipleMovesOnOneSideAreAmbiguousButNotDivergent: DynamicTests =
    // NOTE: need to define at least one source, because the invariant for
    // `MoveDestinations` forbids a degenerate instance from modelling moves to
    // just one side.
    val suts = Trials.api.choose(
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set.empty
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set.empty
      )
    )

    suts.withLimit(10).dynamicTests { sut =>
      assert(sut.isAmbiguous)
      assert(!sut.isDivergent)

      println(sut.description)
    }
  end multipleMovesOnOneSideAreAmbiguousButNotDivergent

  @Test
  def aSingleCoincidentMoveIsNeitherAmbiguousNorDivergent(): Unit =
    val sut =
      MoveDestinations(
        left = Set.empty,
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      )
    assert(!sut.isAmbiguous)
    assert(!sut.isDivergent)

    println(sut.description)
  end aSingleCoincidentMoveIsNeitherAmbiguousNorDivergent

  @Test
  def multipleCoincidentMovesAreAmbiguousButNotDivergent(): Unit =
    val sut =
      MoveDestinations(
        left = Set.empty,
        right = Set.empty,
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      )
    assert(sut.isAmbiguous)
    assert(!sut.isDivergent)

    println(sut.description)
  end multipleCoincidentMovesAreAmbiguousButNotDivergent

  @TestFactory
  def mixturesOfMovesOnOneSideAndCoincidentMovesAreAmbiguousButNotDivergent
      : DynamicTests =
    val suts = Trials.api.choose(
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation()),
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      )
    )

    suts.withLimit(10).dynamicTests { sut =>
      assert(sut.isAmbiguous)
      assert(!sut.isDivergent)

      println(sut.description)
    }
  end mixturesOfMovesOnOneSideAndCoincidentMovesAreAmbiguousButNotDivergent

  @Test
  def aSingleLeftMoveWithASingleRightMoveIsDivergentButNotAmbiguous(): Unit =
    val sut =
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set.empty
      )
    assert(!sut.isAmbiguous)
    assert(sut.isDivergent)

    println(sut.description)
  end aSingleLeftMoveWithASingleRightMoveIsDivergentButNotAmbiguous

  @TestFactory
  def anythingElseInvolvingLeftMovesAndRightMovesIsBothAmbiguousAndDivergent
      : DynamicTests =
    val suts = Trials.api.choose(
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set.empty
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set.empty
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set.empty
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation() -> newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(
          newUniqueFakeLocation() -> newUniqueFakeLocation(),
          newUniqueFakeLocation() -> newUniqueFakeLocation()
        )
      )
    )

    suts.withLimit(10).dynamicTests { sut =>
      assert(sut.isAmbiguous)
      assert(sut.isDivergent)

      println(sut.description)
    }
  end anythingElseInvolvingLeftMovesAndRightMovesIsBothAmbiguousAndDivergent
end MoveDestinationsTest
