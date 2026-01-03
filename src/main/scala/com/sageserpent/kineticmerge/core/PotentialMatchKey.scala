package com.sageserpent.kineticmerge.core

import cats.{Eq, Order}
import com.google.common.hash.{Funnel, HashFunction}

// NOTE: this is subtle - this type is used as an ordered key to find
// matches across sides; fingerprints can and do collide, so we need the
// content as a tiebreaker. However, we don't want to have to freight the
// content around for keys that will never match across sides - there are a
// lot of keys involved in finding matches at low window sizes, and large
// window sizes imply large content sizes.
//
// The solution is to rely on lazy evaluation semantics for ordering of
// pairs, and to evaluate the content of the section when it's really needed
// to break a tie on fingerprints. However, this means that when there are
// multiple matches whose keys collide, then only one key can represent the
// matches in a `SortedMultiDict` - so we expect to see keys whose section
// is unrelated to some of the matches it is associated with, but is a
// legitimate key for them nonetheless.
case class PotentialMatchKey[Element: Eq: Order: Funnel](
    fingerprint: BigInt,
    impliedContent: Section[Element]
)(using hashFunction: HashFunction):
  import PotentialMatchKey.tiebreakContentSamplingLimit

  // NOTE: instances of `PotentialMatchKey` are intended to be put into sets
  // using hashing, so we may as well get on with it and compute the
  // inevitable hash code.
  private val cachedHashCode: Int =
    val hasher = hashFunction.newHasher()

    hasher.putBytes(fingerprint.toByteArray)

    impliedContent.content
      .take(tiebreakContentSamplingLimit)
      .foreach(hasher.putObject(_, summon[Funnel[Element]]))

    hasher.hash().asInt()
  end cachedHashCode

  override def equals(another: Any): Boolean =
    another.asInstanceOf[Matchable] match
      case PotentialMatchKey[Element](
            anotherFingerprint,
            anotherImpliedContent
          ) =>
        fingerprint == anotherFingerprint && PotentialMatchKey.impliedContentEquality
          .eqv(
            impliedContent,
            anotherImpliedContent
          )
      case _ => false

  override def hashCode(): Int = cachedHashCode
end PotentialMatchKey

object PotentialMatchKey:
  private val tiebreakContentSamplingLimit = 5

  private def impliedContentEquality[Element: Eq]: Eq[Section[Element]] =
    Eq.by[Section[Element], Seq[Element]](
      _.content.take(tiebreakContentSamplingLimit)
    )
end PotentialMatchKey
