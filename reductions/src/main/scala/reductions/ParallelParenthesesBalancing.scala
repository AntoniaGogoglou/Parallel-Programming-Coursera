package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def checkOpenClose(chars: Array[Char], openCount: Int): Boolean = {
      if (chars.isEmpty) openCount == 0
      else if (openCount < 0) false
      else if (chars.head == '(') checkOpenClose(chars.tail, openCount + 1)
      else if (chars.head == ')') checkOpenClose(chars.tail, openCount - 1)
      else checkOpenClose(chars.tail, openCount)

    }
    checkOpenClose(chars,0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    //this is a traditional implementation of traverse (sequential) that returns the remaining open parentheses (right ones
    //that haven't been closed) and closed ones (left ones that haven't been matched with right ones) in range idx-until
    def traverse(idx: Int, until: Int, open: Int, close: Int) = {
      //this re-assignments are necessary because Int are immutable in Scala
      var i=idx
      var open_tmp=open
      var closed_tmp=close
      while (i<until){
        if (chars(i)=='('){
          open_tmp=open_tmp+1
          i=i+1
        }
        else if (chars(i)==')'){
          if (open_tmp<=0){
            closed_tmp=closed_tmp+1
            i=i+1
          }
          else {
            open_tmp=open_tmp+1
            i=i+1
          }
        }
        else i=i+1
      }
      (open_tmp,closed_tmp)
    }

    //this is a more functional implementation of traverse recursively
    def traverse_imperative(idx: Int, until: Int, closed: Int, open: Int): (Int, Int) = {
      if (idx == until)
        (closed, open)
      else chars(idx) match {
        case '(' => traverse_imperative(idx + 1, until, closed, open + 1)
        case ')' =>
          if (open > 0) traverse_imperative(idx + 1, until, closed, open - 1)
          else traverse_imperative(idx + 1, until, closed + 1, open)
        case _ => traverse_imperative(idx + 1, until, closed, open)
      }
    }

    def combine(open_left: Int, closed_left: Int, open_right: Int, closed_right: Int): (Int, Int)={
      var open_total=(open_left-closed_right)+open_right
      var closed_total=(open_left-closed_right)+closed_left
      (open_total,closed_total)
    }

    //reduce will call traverse for range from-until if the length of the sequence <threshold
    //else it will call itself recursively and in parallel for the two halves and then combine the results
    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until-from)<threshold) {
        //val (open_total,closed_total)=traverse(from,until,0,0)
        traverse(from,until,0,0)
      }else {
        val mid = (until - from) / 2
        val ((open_left, closed_left), (open_right, closed_right)) = parallel(reduce(from, mid), reduce(mid + 1, until))
        combine(open_left, closed_left, open_right, closed_right)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
