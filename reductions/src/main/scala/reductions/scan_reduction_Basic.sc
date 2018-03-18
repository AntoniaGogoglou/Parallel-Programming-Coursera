import org.scalameter._
import common._

object ArrayScan { // Parallel scan of an array

  /*
   fold left array segment from left to right-1, sequentially.
   Used in the base case for upsweep.
   This is the same operation we would use in the base case of parallel fold.
   */
  def foldASegSeq[A,B](inp: Array[A],
                       left: Int, right: Int,
                       b0: B, // initial element
                       f: (B,A) => B): B = {
    var b= b0
    var i= left
    while (i < right) {
      b= f(b, inp(i))
      i= i+1
    }
    b
  }

  // Binary trees whose nodes store elements of type A
  sealed abstract class FoldTree[A] {
    val res: A // whether it is leaf or internal node, res stores the result
  }
  case class Leaf[A](from: Int, to: Int, resLeaf: A) extends FoldTree[A] {
    val res= resLeaf
  }
  case class Node[A](l: FoldTree[A], r: FoldTree[A], resNode: A) extends FoldTree[A] {
    val res= resNode
  }

  /*
   fold array segment in parallel and record the intermediate computation results in a Tree[A].
   In the context of scan, this phase is called upsweep.
   For an intuition, picture the array to reduce on the bottom, and the root of the tree at the top.
   Once the 'parallel' tasks are initiated, the results are combined in the 'up' direction, from array
   to the result of the fold.
   */
  def upsweep[A](inp: Array[A],
                 left: Int, right: Int,
                 a0: A,
                 f: (A,A) => A): FoldTree[A] = {
    // requires f to be associative
    val threshold=5
    if (right - left < threshold)
      Leaf(left, right, foldASegSeq(inp, left + 1, right, inp(left), f))
    else {
      val mid = left + (right - left)/2
      val (t1,t2) = parallel(upsweep(inp, left, mid, a0, f),
        upsweep(inp, mid, right, a0, f))
      Node(t1, t2, f(t1.res,t2.res))
    }
  }

  /*
   Scan array segment inp(left) to inp(right-1),
   storing results into out(left+1) to out(right).
   At the end, out(i+1) stores fold of elements:
   [a0, in(left),... in(i)] for i from left to right-1.
   In particular, out(left+1) stores f(a0,inp(left))
   and out(right) stores fold of [a0, in[(left),... inp(right-1)].
   The value a0 is not directly stored into out anywhere.
   This is used below cutoff in downsweep for scanAPar,
   and also to implement scanASeq as a comparison point.
   */
  def scanASegSeq1[A](inp: Array[A],
                      left: Int, right: Int,
                      a0: A,
                      f: (A,A) => A,
                      out: Array[A]) = {
    if (left < right) {
      var i= left
      var a= a0
      while (i < right) {
        a= f(a,inp(i))
        out(i+1)=a
        i= i+1
      }
    }
  }

  def downsweep[A](inp: Array[A],
                   a0: A,
                   f: (A,A) => A,
                   t: FoldTree[A],
                   out: Array[A]): Unit = {
    t match {
      case Leaf(from, to, res) =>
        scanASegSeq1(inp, from, to, a0, f, out)
      case Node(l, r, res) => {
        val (_,_) = parallel(
          downsweep(inp, a0, f, l, out),
          downsweep(inp, f(a0,l.res), f, r, out))
      }
    }
  }

  def scanASegPar[A](inp: Array[A],
                     from: Int, to: Int,
                     a0: A,
                     f: (A,A) => A,
                     out: Array[A]) = {
    val t = upsweep(inp, from, to, a0, f)
    downsweep(inp, a0, f, t, out)
  }

  def scanAPar[A](inp: Array[A],
                  a0: A,
                  f: (A,A) => A,
                  out: Array[A]) = {
    out(0)= a0
    scanASegPar(inp, 0, inp.length, a0, f, out)
  }

  def scanASeq[A](inp: Array[A],
                  a0: A,
                  f: (A,A) => A,
                  out: Array[A]) = {
    out(0) = a0
    scanASegSeq1(inp, 0, inp.length, a0, f, out)
  }