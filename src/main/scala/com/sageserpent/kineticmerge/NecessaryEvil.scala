package com.sageserpent.kineticmerge

import scala.collection.BuildFrom

extension [Key, Value](thisMap: Map[Key, Value])
  // Adapted from
  // https://github.com/scala/scala-collection-contrib/blob/7dbb2494ddb4e1e4ded532df25ea31a65206c3cc/src/main/scala/scala/collection/decorators/MapDecorator.scala#L60
  // as a temporary workaround.
  def mergeByKeyWith[MergedValue](
      other: Map[Key, Value]
  )(mergeValues: PartialFunction[(Option[Value], Option[Value]), MergedValue])(
      implicit
      bf: BuildFrom[
        Map[Key, Value],
        (Key, MergedValue),
        Map[Key, MergedValue]
      ]
  ): Map[Key, MergedValue] =
    import scala.collection.mutable

    val b         = bf.newBuilder(thisMap)
    val traversed = mutable.Set.empty[Key]
    for
      (k, v) <- thisMap
      x = other
        .get(k)
        .fold[MergedValue](mergeValues(Some(v), None)) { w =>
          traversed += k; mergeValues(Some(v), Some(w))
        }
    do b += k -> x
    end for
    for
      (k, w) <- other if !traversed(k)
      x = mergeValues(None, Some(w))
    do b += k -> x
    end for
    b.result()
  end mergeByKeyWith

  def mergeByKey(
      other: Map[Key, Value]
  )(implicit
      bf: BuildFrom[
        Map[Key, Value],
        (Key, (Option[Value], Option[Value])),
        Map[Key, (Option[Value], Option[Value])]
      ]
  ): Map[Key, (Option[Value], Option[Value])] = mergeByKeyWith(other)(identity)
end extension
