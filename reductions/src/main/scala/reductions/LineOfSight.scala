package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    output(0)=0
    var i=1
    while (i<input.length){
      val angle_i=input(i)/i
      //if current angle is bigger than the previous entry in output, it becomes the running max up until i!
      output(i)=max(angle_i,output(i-1))
      i=i+1
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    //every Node will have the max value of its left and right subtrees
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var running_max=0.0f
    var i=from
    while (i<until){
      val angle_i=input(i)/i
      //if current angle is bigger than the running max, then it becomes the running max up until i!
      running_max=max(angle_i,running_max)
      i=i+1
    }
    running_max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if ((end-from)<threshold){
      //define a Leaf that contains the max value of the sequential upsweep in the range from-end
      Leaf(from, end, upsweepSequential(input, from, end))
    }else{
      val mid=from+(end-from)/2
      //the recursive call is made until we reach a Leaf and then the subtrees are aggregated to make our Node
      val (leftSubtree,rightSubtree)=parallel(upsweep(input,from,mid,threshold),upsweep(input,mid+1,end,threshold))
      Node(leftSubtree,rightSubtree)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  //different from the LineOfSight function because it needs to work for arbitrary range of input
  //basically does the same as upsweepSequential but with a given startingAngle and outputs an array not the maxAngle at this point
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    var running_max=startingAngle
    var i=from
    while (i<until){
      val angle_i=input(i)/i
      //if current angle is bigger than the running_max (that started from the startingAngle),
      // it becomes the running max up until i and written in the output
      running_max=max(angle_i,running_max)
      output(i)=running_max
      i=i+1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  //the 'downsweepSequential' will do the actual writing in the output array, but the parallel 'downsweep' will break the
  //the tree down from the given node (tree:Tree) and call 'downsweepSequential' when the Leaf of each subtree is reached
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match{
      case Leaf(from, until, maxPrevious) =>{ downsweepSequential(input, output,
        startingAngle, from, until) }
        //SOS: startingAngle is the max one so far for the left, but as we go to the right we need the max between left max and
        //startingAngle
      case Node(left, right) => {parallel(downsweep(input, output,startingAngle,left),
        downsweep(input,output,max(left.maxPrevious, startingAngle),right))}
    }

  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    //call upsweep for the whole input array to create all the tree-type nodes
    val reductionTree = upsweep(input, 0, input.length - 1, threshold)
    //println(reductionTree)
    downsweep(input, output, 0, reductionTree)
    val lastInputIdx: Int = input.length - 1
    output(lastInputIdx) = max(output(lastInputIdx - 1), input(lastInputIdx) / lastInputIdx)

    //val result_tree=upsweep(input, 1, input.length, threshold)
    //println(result_tree)
    //downsweep(input, output, 1, result_tree)
  }
}
