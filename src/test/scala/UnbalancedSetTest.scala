import org.scalatest.{FlatSpec, Matchers}

class UnbalancedSetTest extends FlatSpec with Matchers {

  behavior of "Test of Tree"

  implicit object ev extends Ordered[Int] {
    def lt(a: Int, b: Int) : Boolean = a < b
    def leq(a: Int, b: Int) : Boolean = a <= b
  }

  it should "Verify that 5 is in the tree" in {
    val set = new UnbalancedSet[Int]()
    val t1 = set.insert(4, set.empty)
    val t2 = set.insert(5, t1)
    val t3 = set.insert(6, t2)

    set.member(4,t1) shouldBe true
    set.member(4,t2) shouldBe true
    set.member(4,t3) shouldBe true
    set.member(5,t3) shouldBe true
    set.member(8,t3) shouldBe false
  }

}

