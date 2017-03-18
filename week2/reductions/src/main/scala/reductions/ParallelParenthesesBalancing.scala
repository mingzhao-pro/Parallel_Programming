package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.collection.mutable

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
    val length = 10000
    val chars = new Array[Char](length)
    val threshold = 10
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
    def loop(count: Int, rest: List[Char]): Int = {
      if(rest.isEmpty || count < 0) count
      else {
        val a = rest.head
        if (a == ')')  loop(count - 1, rest.tail)
        else if (a == '(') loop(count + 1, rest.tail)
        else loop(count, rest.tail)
      }
    }

    loop(0, chars.toList) == 0
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, open: Int, close: Int): Int = {

      def loop(count: Int, rest: List[Char]): Int = {
        if(rest.isEmpty || count < 0) count
        else {
          val a = rest.head
          if (a == ')')  loop(count - 1, rest.tail)
          else if (a == '(') loop(count + 1, rest.tail)
          else loop(count, rest.tail)
        }
      }

      loop(0, chars.toList)
    }

    def reduce(from: Int, until: Int): Int = {
      if(until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val (a, b) = parallel(reduce(from, mid), reduce(mid, until))
        if(a < 0) a else a + b
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
