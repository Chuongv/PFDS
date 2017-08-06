import org.scalatest.{FlatSpec, Matchers}

class UnbalancedSetTest extends FlatSpec with Matchers with UnbalancedSet[Int] {

  behavior of "Test of Tree"

  implicit object ev extends Ordered[Int] {
    def lt(a: Int, b: Int) : Boolean = a < b
    def leq(a: Int, b: Int) : Boolean = a <= b
  }

  it should "Verify that 5 is in the tree" in {
    val root: Tree[Int] = Node(Empty, 5, Empty)

    member(5, root) shouldBe true
    member(8, root) shouldBe false
  }

}

