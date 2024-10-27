package benchmarks

import cats.Eval
import cats.data.{IndexedStateT, State, StateT}
import cats.syntax.all.catsSyntaxTuple2Semigroupal
import org.openjdk.jmh.annotations.Benchmark

import scala.annotation.tailrec
import scala.collection.mutable

class FibonacciBenchmarks:
  import FibonacciBenchmarks.*
  
  def fibonacciNumberViaNaiveApproach(index: Int): BigInt =
    require(0 <= index)
    if 1 >= index then index
    else
      fibonacciNumberViaNaiveApproach(
        index - 1
      ) + fibonacciNumberViaNaiveApproach(index - 2)
    end if
  end fibonacciNumberViaNaiveApproach

  @Benchmark
  def naive(): Unit = inputs map fibonacciNumberViaNaiveApproach

  @Benchmark
  def imperativeCache(): Unit =
    inputs map fibonacciNumberViaImperativeMemoizationApproach

  def fibonacciNumberViaImperativeMemoizationApproach(index: Int): BigInt =
    val partialResultsCache = mutable.Map.empty[Int, BigInt]

    def _fibonacciNumber(index: Int): BigInt =
      require(0 <= index)
      if 1 >= index then index
      else
        memoizedFibonacciNumber(index - 1) + memoizedFibonacciNumber(index - 2)
      end if
    end _fibonacciNumber

    def memoizedFibonacciNumber(index: Int): BigInt =
      partialResultsCache.getOrElseUpdate(index, _fibonacciNumber(index))
    end memoizedFibonacciNumber

    memoizedFibonacciNumber(index)
  end fibonacciNumberViaImperativeMemoizationApproach

  @Benchmark
  def fpPlod(): Unit = inputs map fibonacciNumberViaFpPlod

  def fibonacciNumberViaFpPlod(index: Int): BigInt =
    type PartialResultsCache = Map[Int, BigInt]

    def _fibonacciNumber(partialResultsCache: PartialResultsCache)(
        index: Int
    ): (PartialResultsCache, BigInt) =
      require(0 <= index)
      if 1 >= index then partialResultsCache -> index
      else
        val (onceUpdatedCache, previous) =
          memoizedFibonacciNumber(partialResultsCache)(index - 1)
        val (twiceUpdatedCache, nextOneFurtherBack) =
          memoizedFibonacciNumber(onceUpdatedCache)(index - 2)
        twiceUpdatedCache -> (previous + nextOneFurtherBack)
      end if
    end _fibonacciNumber

    def memoizedFibonacciNumber(partialResultsCache: PartialResultsCache)(
        index: Int
    ): (PartialResultsCache, BigInt) =
      val cachedResult = partialResultsCache.get(index)

      cachedResult.fold(ifEmpty =
        val (onceUpdatedCache, computedResult) =
          _fibonacciNumber(partialResultsCache)(index)

        (onceUpdatedCache + (index -> computedResult)) -> computedResult
      )(partialResultsCache -> _)
    end memoizedFibonacciNumber

    val (_, finalResult) =
      memoizedFibonacciNumber(Map.empty)(index)

    finalResult
  end fibonacciNumberViaFpPlod

  @Benchmark
  def stateTransformer(): Unit = inputs map fibonacciNumberViaStateTransformer

  def fibonacciNumberViaStateTransformer(index: Int): BigInt =
    type PartialResultsCacheState[X] = State[Map[Int, BigInt], X]

    def _fibonacciNumber(index: Int): PartialResultsCacheState[BigInt] =
      require(0 <= index)
      if 1 >= index then State.pure(index)
      else
        (memoizedFibonacciNumber(index - 1), memoizedFibonacciNumber(index - 2))
          .mapN(_ + _)
      end if
    end _fibonacciNumber

    def memoizedFibonacciNumber(index: Int): PartialResultsCacheState[BigInt] =
      StateT.get.flatMap { partialResultsCache =>
        val cachedResult = partialResultsCache.get(index)

        cachedResult.fold(ifEmpty = for
          computedResult <- _fibonacciNumber(index)
          _ <- State.modify[Map[Int, BigInt]](
            _ + (index -> computedResult)
          )
        yield computedResult)(StateT.pure)
      }

    val (_, finalResult) =
      memoizedFibonacciNumber(index).run(Map.empty).value

    finalResult
  end fibonacciNumberViaStateTransformer

  @Benchmark
  def justEval(): Unit = inputs map fibonacciNumberViaJustEval

  def fibonacciNumberViaJustEval(index: Int): BigInt =
    val partialResultsCache = mutable.Map.empty[Int, BigInt]

    def _fibonacciNumber(index: Int): Eval[BigInt] =
      require(0 <= index)
      if 1 >= index then Eval.now(index)
      else
        (memoizedFibonacciNumber(index - 1), memoizedFibonacciNumber(index - 2))
          .mapN(_ + _)
      end if
    end _fibonacciNumber

    def memoizedFibonacciNumber(index: Int): Eval[BigInt] =
      Eval
        .defer {
          val cachedResult = partialResultsCache.get(index)

          cachedResult.fold(ifEmpty =
            for computedResult <- _fibonacciNumber(index)
            yield
              partialResultsCache.put(index, computedResult)
              computedResult
          )(Eval.now)
        }
    end memoizedFibonacciNumber

    memoizedFibonacciNumber(index).value
  end fibonacciNumberViaJustEval

  @Benchmark
  def noForking(): Unit = inputs map fibonacciNumberViaNoForkingApproach

  def fibonacciNumberViaNoForkingApproach(index: Int): BigInt =
    case class ResultPair(
        alignedWithIndex: BigInt,
        alignedWithNextLowestIndex: BigInt
    )

    def _fibonacciNumber(index: Int): ResultPair =
      require(0 <= index)
      if 1 >= index then
        ResultPair(
          alignedWithIndex = index,
          alignedWithNextLowestIndex = index - 1
        )
      else
        val ResultPair(alignedWithIndex, alignedWithNextLowestIndex) =
          _fibonacciNumber(index - 1)

        ResultPair(
          alignedWithIndex = alignedWithIndex + alignedWithNextLowestIndex,
          alignedWithNextLowestIndex = alignedWithIndex
        )
      end if
    end _fibonacciNumber

    _fibonacciNumber(index).alignedWithIndex
  end fibonacciNumberViaNoForkingApproach

  @Benchmark
  def tailRecursion(): Unit = inputs map fibonacciNumberViaTailRecursion

  def fibonacciNumberViaTailRecursion(index: Int): BigInt =
    case class ResultPair(
        alignedWithIndex: BigInt,
        alignedWithNextLowestIndex: BigInt
    )

    @tailrec
    def _fibonacciNumber(
        reversedSenseIndex: Int,
        partialResult: ResultPair
    ): ResultPair =
      require(0 < reversedSenseIndex)
      if index == reversedSenseIndex then partialResult
      else
        val ResultPair(alignedWithIndex, alignedWithNextLowestIndex) =
          partialResult

        _fibonacciNumber(
          reversedSenseIndex = 1 + reversedSenseIndex,
          partialResult = ResultPair(
            alignedWithIndex = alignedWithIndex + alignedWithNextLowestIndex,
            alignedWithNextLowestIndex = alignedWithIndex
          )
        )
      end if
    end _fibonacciNumber

    if 0 == index then 0
    else
      _fibonacciNumber(
        reversedSenseIndex = 1,
        ResultPair(alignedWithIndex = 1, alignedWithNextLowestIndex = 0)
      ).alignedWithIndex
    end if
  end fibonacciNumberViaTailRecursion
end FibonacciBenchmarks

object FibonacciBenchmarks:
  val inputs =
    // If you're feeling brave, uncomment the `:+ 46`. If you're more daring,
    // change the 46 to 5000. Don't bother doing the latter until you've
    // commented
    // out the first example, though.
    (0 to 20) :+ 46
end FibonacciBenchmarks
