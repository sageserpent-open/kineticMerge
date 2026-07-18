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
      multiplicity: Int,
      override val size: Int
  ) extends SectionsSeen[Element]:
    require(0 < multiplicity)

    override val hashCode: Int =
      val leftHash  = left.hashCode
      val rightHash = right.hashCode

      (
        section,
        priority,
        maxOnePastEndOffset,
        leftHash,
        rightHash,
        multiplicity,
        size
      )
        .hashCode()
    end hashCode

    override def isEmpty: Boolean = false

    override def iterator: Iterator[Section[Element]] =
      (left match
        case l: Treap[Element] => l.iterator
        case Empty             => Iterator.empty
      ) ++ Iterator.fill(multiplicity)(section) ++ (right match
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
      // Use both the section's hash code and the current treap's hash code to
      // calculate a deterministic priority.
      val priority = (section, this).hashCode()
      def add(node: Treap[Element] | Empty.type): Treap[Element] = node match
        case Empty =>
          Treap(
            section = section,
            priority = priority,
            maxOnePastEndOffset = section.onePastEndOffset,
            left = Empty,
            right = Empty,
            multiplicity = 1,
            size = 1
          )
        case t: Treap[Element] =>
          if t.section == section then
            t.copy(multiplicity = 1 + t.multiplicity, size = 1 + t.size)
          else if priority > t.priority then
            val (l, r) = split(t, section.startOffset)
            Treap(
              section = section,
              priority = priority,
              maxOnePastEndOffset = section.onePastEndOffset,
              left = l,
              right = r,
              multiplicity = 1,
              size = 1
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
          if t.section == section then
            if t.sectionIsUnique then merge(t.left, t.right)
            else t.copy(multiplicity = t.multiplicity - 1, size = t.size - 1)
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

    private def sectionIsUnique: Boolean = 1 == multiplicity

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
        size = multiplicity + leftSize + rightSize
      )
    end recompute
  end Treap

  private object Empty extends SectionsSeen[Any]:
    override val hashCode: Int = 57

    override def filterIncludes(interval: (Int, Int)): Iterable[Section[Any]] =
      Iterable.empty
    override def filterOverlaps(interval: (Int, Int)): Iterable[Section[Any]] =
      Iterable.empty
    override def +(section: Section[Any]): SectionsSeen[Any] =
      val priority = section.hashCode()
      Treap(
        section = section,
        priority = priority,
        maxOnePastEndOffset = section.onePastEndOffset,
        left = Empty,
        right = Empty,
        multiplicity = 1,
        size = 1
      )
    end +
    override def -(section: Section[Any]): SectionsSeen[Any] = this
    override def iterator: Iterator[Section[Any]]            = Iterator.empty
    override def isEmpty: Boolean                            = true
    override def size: Int                                   = 0
  end Empty

end SectionsSeen
