/**
  * Leftists heaps are heap-ordered binary trees that
  * satisfy the leftist property: the rank of any left child is at least as large as the rank
  * of its right sibling.
  */

sealed trait Heap[+A]
final case class HeapNode[A](rank: Int, elem: A, left: Heap[A], right: Heap[A])(implicit ev: Ordering[A]) extends Heap[A]
case object HeapEmpty extends Heap[Nothing]

object Heap {

  def empty[A]: Heap[A] = HeapEmpty
  def isEmpty[A](a: Heap[A]): Boolean = {
    a match {
      case HeapEmpty => true
      case _ => false
    }
  }

  def insert[A](a: A, h: Heap[A])(implicit ev: Ordering[A]): Heap[A] =
    merge(h, HeapNode(1, a, HeapEmpty, HeapEmpty))

  def merge[A](a: Heap[A], b: Heap[A])(implicit ev: Ordering[A]): Heap[A] = {

    def rank(a: Heap[A]): Int = {
      a match {
        case HeapEmpty => 0
        case HeapNode(r, _, _, _) => r
      }
    }

    def makeT(elem: A, a: Heap[A], b: Heap[A]): Heap[A] = {
      if (rank(a) >= rank(b)) {
        HeapNode(rank(b) + 1, elem, a, b)
      } else {
        HeapNode(rank(a) + 1, elem, b, a)
      }
    }

    (a, b) match {
      case (HeapEmpty, HeapEmpty) => HeapEmpty
      case (HeapEmpty, r) => r
      case (l, HeapEmpty) => l
      case (HeapNode(lr, el, llc, lrc), HeapNode(rr, er, rlc, rrc)) => {
        if (ev.lt(el,er)) {
          makeT(el, llc, merge(lrc, b))
        } else {
          makeT(er, rlc, merge(rrc, a))
        }
      }
    }
  }


  def findMin[A](a: Heap[A]): A = a match {
    case HeapEmpty => throw new Exception ("Heap is empty")
    case HeapNode(_, e, _, _) => e
  }

  def deleteMin[A](a: Heap[A])(implicit ev: Ordering[A]): Heap[A] = {
    a match {
      case HeapEmpty => throw new Exception("Heap is empty")
      case HeapNode(_, _, HeapEmpty, HeapEmpty) => HeapEmpty
      case HeapNode(_, _, l, HeapEmpty) => l
      case HeapNode(_, _, HeapEmpty, r) => r
      case HeapNode(_, _, l, r) => merge(l, r)
    }
  }

}
