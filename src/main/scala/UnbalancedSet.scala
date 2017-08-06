import scala.annotation.tailrec

trait UnbalancedSet[T] extends Set[T, Tree[T]] {

  implicit val ev :Ordered[T]

  override def member(e: T, t: Tree[T]): Boolean =
    t match {
      case Empty => false
      case Node(a, y, b) =>
        if (ev.lt(e,y)) {
          member(e,a)
        } else if (ev.lt(y,e)) {
          member(e,b)
        } else
          true
    }

  final override def insert(e: T, s: Tree[T]): Tree[T] =
    s match {
      case Empty => Node(Empty, e, Empty)
      case Node(a, y, b) =>
        if (ev.lt(e,y)) {
         Node(insert(e, a), y, b)
        } else if (ev.lt(y,e)) {
          Node(a,y,insert(e, b))
        } else s
    }

  /**
    * Inserting an existing element into a binary searh tree copies the entire search path even though
    * the copied nodes are indistinguishable from the originals.
    * Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion
    * rather than one handler per iteration.
    */

  /**
    * In the worst case, member performs approximately 2d comparisons,
    * where d is the depth of the tree. Rewrite member to take no more than d+1 comparisons by
    * keeping track of a candidate element that might be equal to the query element (< return false or <= return true)
    * Then checking equality only when you hit the bottom of the tree.
    */
  @tailrec
  final def memberVersion2(e: T, t: Tree[T], candidate: Option[T] = None): Boolean =
    t match {
      case Empty => if (candidate.isDefined) candidate.get == e else false
      case Node(a, y, b) =>
        if (ev.lt(e,y)) {
          memberVersion2(e, a, candidate)
        } else {
          memberVersion2(e, b, Some(y))
        }
    }
}

