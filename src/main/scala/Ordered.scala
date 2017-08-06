trait Ordered[T] {
  def eq(b: T) : Boolean
  def lt(b: T) : Boolean
  def leq(b: T) : Boolean
}
