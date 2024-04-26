package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.Syntax
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MoveDestinationsTest.newUniqueFakeLocation
import org.junit.jupiter.api.{Test, TestFactory}

import java.util.UUID

object MoveDestinationsTest:
  def newUniqueFakeLocation(): UUID = UUID.randomUUID()
end MoveDestinationsTest

class MoveDestinationsTest:
  @TestFactory
  def aSingleMoveOnOneSideIsNeitherAmbiguousNorDivergent =
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
      .withLimit(10)
      .dynamicTests { sut =>
        assert(!sut.isAmbiguous)
        assert(!sut.isDivergent)
      }
  end aSingleMoveOnOneSideIsNeitherAmbiguousNorDivergent

  @TestFactory
  def multipleMovesOnOneSideAreAmbiguousButNotDivergent =
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
    }
  end multipleMovesOnOneSideAreAmbiguousButNotDivergent

  @Test
  def aSingleCoincidentMoveIsNeitherAmbiguousNorDivergent =
    val sut =
      MoveDestinations(
        left = Set.empty,
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation())
      )
    assert(!sut.isAmbiguous)
    assert(!sut.isDivergent)
  end aSingleCoincidentMoveIsNeitherAmbiguousNorDivergent

  @Test
  def multipleCoincidentMovesAreAmbiguousButNotDivergent =
    val sut =
      MoveDestinations(
        left = Set.empty,
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      )
    assert(sut.isAmbiguous)
    assert(!sut.isDivergent)
  end multipleCoincidentMovesAreAmbiguousButNotDivergent

  @TestFactory
  def mixturesOfMovesOnOneSideAndCoincidentMovesAreAmbiguousButNotDivergent =
    val suts = Trials.api.choose(
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set.empty,
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set.empty,
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      )
    )

    suts.withLimit(10).dynamicTests { sut =>
      assert(sut.isAmbiguous)
      assert(!sut.isDivergent)
    }
  end mixturesOfMovesOnOneSideAndCoincidentMovesAreAmbiguousButNotDivergent

  @Test
  def aSingleLeftMoveWithASingleRightMoveIsDivergentButNotAmbiguous =
    val sut =
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set.empty
      )
    assert(!sut.isAmbiguous)
    assert(sut.isDivergent)
  end aSingleLeftMoveWithASingleRightMoveIsDivergentButNotAmbiguous

  @TestFactory
  def anythingElseInvolvingLeftMovesAndRightMovesIsBothAmbiguousAndDivergent =
    val suts = Trials.api.choose(
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
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
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set.empty
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation())
      ),
      MoveDestinations(
        left = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        right = Set(newUniqueFakeLocation(), newUniqueFakeLocation()),
        coincident = Set(newUniqueFakeLocation(), newUniqueFakeLocation())
      )
    )

    suts.withLimit(10).dynamicTests { sut =>
      assert(sut.isAmbiguous)
      assert(sut.isDivergent)
    }
  end anythingElseInvolvingLeftMovesAndRightMovesIsBothAmbiguousAndDivergent
end MoveDestinationsTest
