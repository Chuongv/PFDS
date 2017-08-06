trait UnbalancedSet[T <:Ordered[T]] extends Set[T, Tree[T]] {

  override def member(e: T, t: Tree[T]): Boolean =
    t match {
      case Empty => false
      case Node(a, y, b) =>
        if (e lt y) {
          member(e,a)
        } else if (y lt e) {
          member(e,b)
        } else
          true
    }

  override def insert(e: T, s: Tree[T]): Tree[T] =
    s match {
      case Empty => Node(Empty, e, Empty)
      case Node(a, y, b) =>
        if (e lt y) {
         Node(insert(e, a), y, b)
        } else if (y lt e) {
          Node(a,y,insert(e, b))
        } else s
    }

}

