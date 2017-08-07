class FiniteMap[K,V]()(implicit ev: Ordering[K]) extends UnbalancedSet[(K,V)] {
  def lookup(k: K, tree: Tree[(K,V)]): V =
    tree match {
      case Empty => throw new Exception("No key found")
      case Node(l,n,r) =>
        if (ev.lt(k, n._1))
          lookup(k, l)
        else if (ev.lt(n._1,k))
          lookup(k,r)
        else
          n._2
    }

  def bind(element: (K,V), tree: Tree[(K,V)]): Tree[(K,V)] = insert(element, tree)
}
