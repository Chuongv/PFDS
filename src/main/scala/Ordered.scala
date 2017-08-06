trait Ordered[T] {
  def lt(a: T, b: T) : Boolean
  def leq(a: T, b: T) : Boolean
}
