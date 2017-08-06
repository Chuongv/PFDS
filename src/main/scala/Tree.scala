
sealed trait Tree[T]
case object Empty extends Tree[Nothing]
final case class Node[T](left: Tree[T], obj: T, right: Tree[T]) extends Tree[T]
