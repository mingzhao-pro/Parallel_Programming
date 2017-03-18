package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val input = Array(0.0f, 7.0f, 10.0f, 33.0f, 48.0f)
    val threshold = 2
    val res = upsweep(input, 1, 5, threshold)
    assert(res == Node(Leaf(1,3,7.0f),Leaf(3,5,12.0f)))
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](6)
    val input = Array(0.0f, 9.0f, 18.0f, 27.0f, 36.0f, 45.0f)
    val t = upsweep(input, 0, input.length, 3)
    downsweep(input, output, 0, t)
    assert(output.toList == List(0.0, 9.0, 9.0, 9.0, 9.0, 9.0))
  }

  test("downsweepSequential ") {
    val output = new Array[Float](6)
    val input = Array(0.0f, 9.0f, 18.0f, 27.0f, 36.0f, 45.0f)
    val t = upsweep(input, 0, input.length, 1)
    downsweep(input, output, 0, t)
    assert(output.toList == List(0.0, 9.0, 9.0, 9.0, 9.0, 9.0))
  }

  test("downsweepSequential should correctly ") {
    val output = new Array[Float](5)
    val input = Array(0.0f, 7.0f, 5.0f, 33.0f, 48.0f)
//    val t = upsweep(input, 0, input.length, 1)

    downsweep(input, output, 8, new Leaf(0, 5, 12))
    assert(output.toList == List(0.0, 8.0, 8.0, 11.0, 12.0))
  }

}

