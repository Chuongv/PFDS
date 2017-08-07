import scala.annotation.tailrec

class UnbalancedSet[T]()(implicit ev: Ordering[T]) extends Set[T, Tree[T]] {

  override def empty: Tree[T] = Empty

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

  /**
    * Inserting an existing element into a binary search tree copies the entire search path even though
    * the copied nodes are indistinguishable from the originals.
    * Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion
    * rather than one handler per iteration.
    */
  final def insert2(e: T, s: Tree[T]): Tree[T] =
    s match {
      case Empty => Node(Empty, e, Empty)
      case Node(a, y, b) =>
        if (ev.lt(e,y)) {
          val inserted = insert2(e,a)
          Node(inserted,y,b)
        } else if (ev.lt(y,e)) {
          val inserted = insert2(e,b)
          Node(a,y,inserted)
        } else throw new Exception("Element already exists")
    }

  /**
    * Combine the ideas of the previous two exercises to obtain a version of insert
    * that performs no unnecessary copying and uses no more than d+1 comparisons.
    */
  final def insert3(e: T, s: Tree[T], candidate: Option[T] = None): Tree[T] =
    s match {
      case Empty => if(candidate.isDefined && candidate.get == e) throw new Exception("Already exists")
        else
          Node(Empty, e, Empty)
      case Node(a, y, b) =>
        if (ev.lt(e,y)) {
          val inserted = insert3(e,a,candidate)
          Node(inserted,y,b)
        } else {
          val inserted = insert3(e,b,Some(y))
          Node(a,y,inserted)
        }
    }

  /**
    * Write a function complete that returns a complete binary tree
    * Making sure to reuse the same subtrees if the given nodes are identical
    * In this scenario, we consider depth of zero to be an empty tree
    * This function should run in O(depth) time
    */
  final def complete(x: T, depth: Int): Tree[T] = {
    depth match {
      case base if base <= 0 => Empty
      case d =>
        val subtree = complete(x, depth - 1)
        Node(subtree, x, subtree)
    }
  }

  /**
    * Extend the function to create balanced trees of arbitrary size. Trees will be not
    * be complete but will be balanced as much as possible.
    */
  final def create(x:T, amount: Int): Tree[T] = {
   amount match {
     case base if base <= 0 => Empty
     case m if m % 2 != 0 =>
       val subtree = create(x, (amount - 1) / 2)
       Node(subtree, x, subtree)
     case m =>
       val leftSubtree = create(x, (amount - 1) / 2)
       val rightSubtree = create(x, (amount - 1) / 2 - 1)
       Node(leftSubtree, x, rightSubtree)
   }
  }
}

