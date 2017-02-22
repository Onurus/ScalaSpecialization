package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =  for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get
  // the smallest of the two elements back.

  property("emptyMin2Insert") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  //If you insert an element into an empty heap,
  // then delete the minimum,
  // the resulting heap should be empty.

  property("emptyInsertDelete") = forAll { a:Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  //Given any heap, you should get a sorted
  // sequence of elements when continually
  // finding and deleting minima

  property("sortedSequence") =forAll{(h:H)=>
    def checker(h2:H):Boolean={
      val a = findMin(h2)
      val h3 = deleteMin(h2)
      if(isEmpty(h3)){
        true
      }else{
        (a<=findMin(h3)) && checker(h3)
      }
    }
      checker(h)
  }


  // Finding a minimum of
  // the melding of any two heaps
  // should return a minimum of one or the other.
  property("melding") = forAll { (a:Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)
    val merged = meld(h, i)

    findMin(merged) == min(a,b)
  }

  property("melding heaps") = forAll { (h:H, i:H) =>
    val minH = findMin(h)
    val minI = findMin(i)
    val merged = meld(h, i)
    findMin(merged) == min(minH , minI)
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insDelMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meldMin") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val m = meld(h1, h2)
    val minMeld = findMin(m)
    minMeld == min1 || minMeld == min2
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("remMin") = forAll { h: H =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val xs = remMin(h, Nil)
    xs == xs.sorted
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }
}
