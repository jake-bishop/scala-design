package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- Arbitrary.arbitrary[A]
    heap <- oneOf(const[H](empty), genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint 0: insert an element into an empty heap, heap min should equal element") = forAll { a: A =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  property("hint 1: inserting two elements findMin returns min of two elements") = forAll { (a1: A, a2: A) =>
    val heap = insert(a2, insert(a1, empty))
    val min = findMin(heap)
    (a1 == min && a1 <= a2) || (a2 == min && a2 <= a1)
  }

  property("hint 2: add an item to an empty heap, delete the min, heap is empty") = forAll { (a: A) =>
    val heap = insert(a, empty)
    isEmpty(deleteMin(heap))
  }

  property("hint 3: heap is in ascending order") = forAll { (h: H) =>
    def sortIter(lastVal: A, heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val min = findMin(heap)
        lastVal <= min && sortIter(min, deleteMin(heap))
      }
    }
    if (isEmpty(h)) true
    else sortIter(findMin(h), deleteMin(h))
  }

  property("hint 4: min of meld of two lists is the min of either list") = forAll { (h1: H, h2: H) =>
    val heap = meld(h1, h2)
    val min = findMin(heap)
    (min == findMin(h1)) || (min == findMin(h2))
  }

  // solve bogus #3
  property("link of two nodes will return one element as the leftmost child of the other element") = forAll { (a1: A, a2: A) =>
    val heap = insert(a2, insert(a1, empty))
    val min1 = findMin(heap)
    val min2 = findMin(deleteMin(heap))
    (min1 == a1 && min2 == a2) || (min1 == a2 && min2 == a1)
  }

  // solve bogus #4
  property("deleteMin should always remove min regardless of rank/sibling order") = forAll { (a1: A, a2: A, h: H) =>
    def heapEqualIter(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && heapEqualIter(deleteMin(h1), deleteMin(h2))
      }
    }
    // we need to generate two heaps with equivalent contents, but different internal structures:
    val insertedHeap = insert(a2, insert(a1, h))
    val meldedHeap = meld(insert(a2, insert(a1, empty)), h)
    heapEqualIter(insertedHeap, meldedHeap)
  }
}
