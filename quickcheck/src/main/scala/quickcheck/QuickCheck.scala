package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(elem, heap)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should" +
           "get the smallest of the two elements back.") = forAll { (a: Int, b: Int) => 
             val min = if (a < b) a else b
             findMin(insert(a,insert(b, empty))) == min
  }
  
  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }
  
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima." +
           "(Hint: recursion and helper functions are your friends.)") = forAll { (h: H) =>    
    def minList(heap: H, list: List[Int]): List[Int] = {
      if (!isEmpty(heap)) {
        minList(deleteMin(heap), findMin(heap) :: list)
      }
      else list.reverse
    }
    val xs = minList(h, Nil)
    xs == xs.sorted
  }
  
  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll { (h: H, i: H) =>
    val m = findMin(h)
    val n = findMin(i)
    val mx = if (m < n) m else n
    findMin(meld(h, i)) == mx
  }
  
  property("DeleteMin on melded Heap") = forAll { (a: Int, b: Int) => 
    val h = insert(a, empty)
    val hx = insert(b, h)
    val hDel = deleteMin(hx)
    
    findMin(hDel) == List(a,b).max
  }
  
  property("insert and remove list to Heap.") = forAll { (h1: H, h2: H) => 
    def toList(h: H): List[A] = h match {
      case h if isEmpty(h) => Nil
      case h => findMin(h) :: toList(deleteMin(h))
    }
      
    val melded = meld(h1,h2)
    val list = toList(h1) ::: toList(h2)
    toList(melded).sorted == list.sorted
  }
  
}
