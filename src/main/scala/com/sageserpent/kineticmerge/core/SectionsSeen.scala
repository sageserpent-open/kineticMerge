package com.sageserpent.kineticmerge.core

/** A persistent interval tree implemented as an augmented treap.
  *
  * The treap is based on the randomized binary search tree by Cecilia R. Aragon
  * and Raimund Seidel (https://faculty.washington.edu/aragon/pubs/rst89.pdf).
  *
  * The interval tree augmentation is based on the approach described in
  * "Introduction to Algorithms" by Cormen, Leiserson, Rivest, and Stein (CLRS).
  */
trait SectionsSeen[Element] extends Iterable[Section[Element]]:
  def filterIncludes(interval: (Int, Int)): Iterable[Section[Element]]
  def filterOverlaps(interval: (Int, Int)): Iterable[Section[Element]]
  def +(section: Section[Element]): SectionsSeen[Element]
  def -(section: Section[Element]): SectionsSeen[Element]
end SectionsSeen

object SectionsSeen:
  def empty[Element]: SectionsSeen[Element] =
    Empty.asInstanceOf[SectionsSeen[Element]]

  private case class Treap[Element](
      section: Section[Element],
      priority: Int,
      maxOnePastEndOffset: Int,
      left: Treap[Element] | Empty.type,
      right: Treap[Element] | Empty.type,
      override val size: Int
  ) extends SectionsSeen[Element]:
    override def isEmpty: Boolean = false

    override def iterator: Iterator[Section[Element]] =
      (left match
        case l: Treap[Element] => l.iterator
        case Empty             => Iterator.empty
      ) ++ Iterator(section) ++ (right match
        case r: Treap[Element] => r.iterator
        case Empty             => Iterator.empty)

    override def filterIncludes(
        interval: (Int, Int)
    ): Iterable[Section[Element]] =
      val (start, onePastEnd) = interval
      val results = scala.collection.mutable.ListBuffer.empty[Section[Element]]

      def search(node: Treap[Element] | Empty.type): Unit = node match
        case Empty             =>
        case t: Treap[Element] =>
          if t.maxOnePastEndOffset >= onePastEnd then
            search(t.left)

            if t.section.startOffset <= start then
              if t.section.onePastEndOffset >= onePastEnd
              then results += t.section
              end if
              search(t.right)
            end if

      search(this)
      results
    end filterIncludes

    override def filterOverlaps(
        interval: (Int, Int)
    ): Iterable[Section[Element]] =
      val (start, onePastEnd) = interval
      val results = scala.collection.mutable.ListBuffer.empty[Section[Element]]

      def search(node: Treap[Element] | Empty.type): Unit = node match
        case Empty             =>
        case t: Treap[Element] =>
          if t.maxOnePastEndOffset > start then
            search(t.left)

            if t.section.startOffset < onePastEnd then
              if t.section.onePastEndOffset > start
              then results += t.section
              end if
              search(t.right)
            end if

      search(this)
      results
    end filterOverlaps

    override def +(section: Section[Element]): SectionsSeen[Element] =
      // Use the section's hash code as a deterministic priority to ensure
      // reproducibility.
      val priority = section.hashCode()
      def add(node: Treap[Element] | Empty.type): Treap[Element] = node match
        case Empty =>
          Treap(section, priority, section.onePastEndOffset, Empty, Empty, 1)
        case t: Treap[Element] =>
          if priority > t.priority then
            val (l, r) = split(t, section.startOffset)
            Treap(
              section,
              priority,
              section.onePastEndOffset,
              l,
              r,
              1
            ).recompute
          else if section.startOffset < t.section.startOffset then
            t.copy(left = add(t.left)).recompute
          else t.copy(right = add(t.right)).recompute

      add(this)
    end +

    override def -(section: Section[Element]): SectionsSeen[Element] =
      def remove(
          node: Treap[Element] | Empty.type
      ): Treap[Element] | Empty.type = node match
        case Empty             => Empty
        case t: Treap[Element] =>
          if t.section == section then merge(t.left, t.right)
          else if section.startOffset < t.section.startOffset then
            val newLeft = remove(t.left)
            if newLeft eq t.left then t else t.copy(left = newLeft).recompute
          else
            val newRight = remove(t.right)
            if newRight eq t.right then t
            else t.copy(right = newRight).recompute
            end if

      remove(this).asInstanceOf[SectionsSeen[Element]]
    end -

    private def split(
        node: Treap[Element] | Empty.type,
        pivotStartOffset: Int
    ): (Treap[Element] | Empty.type, Treap[Element] | Empty.type) = node match
      case Empty             => (Empty, Empty)
      case t: Treap[Element] =>
        if t.section.startOffset < pivotStartOffset then
          val (l, r) = split(t.right, pivotStartOffset)
          (t.copy(right = l).recompute, r)
        else
          val (l, r) = split(t.left, pivotStartOffset)
          (l, t.copy(left = r).recompute)

    private def merge(
        l: Treap[Element] | Empty.type,
        r: Treap[Element] | Empty.type
    ): Treap[Element] | Empty.type = (l, r) match
      case (Empty, _)                               => r
      case (_, Empty)                               => l
      case (lt: Treap[Element], rt: Treap[Element]) =>
        if lt.priority > rt.priority then
          lt.copy(right = merge(lt.right, rt)).recompute
        else rt.copy(left = merge(lt, rt.left)).recompute

    private def recompute: Treap[Element] =
      val leftMax = left match
        case lt: Treap[Element] => lt.maxOnePastEndOffset
        case Empty              => 0
      val rightMax = right match
        case rt: Treap[Element] => rt.maxOnePastEndOffset
        case Empty              => 0
      val leftSize = left match
        case lt: Treap[Element] => lt.size
        case Empty              => 0
      val rightSize = right match
        case rt: Treap[Element] => rt.size
        case Empty              => 0
      copy(
        maxOnePastEndOffset = section.onePastEndOffset max leftMax max rightMax,
        size = 1 + leftSize + rightSize
      )
    end recompute
  end Treap

  private object Empty extends SectionsSeen[Any]:
    override def filterIncludes(interval: (Int, Int)): Iterable[Section[Any]] =
      Iterable.empty
    override def filterOverlaps(interval: (Int, Int)): Iterable[Section[Any]] =
      Iterable.empty
    override def +(section: Section[Any]): SectionsSeen[Any] = Treap(
      section,
      section.hashCode(),
      section.onePastEndOffset,
      Empty,
      Empty,
      1
    )
    override def -(section: Section[Any]): SectionsSeen[Any] = this
    override def iterator: Iterator[Section[Any]]            = Iterator.empty
    override def isEmpty: Boolean                            = true
    override def size: Int                                   = 0
  end Empty

end SectionsSeen
