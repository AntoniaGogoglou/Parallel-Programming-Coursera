package kmeans

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

class KMeans {
  //SOOOOS By making all the methods working for GenSeq, GenMap, klp we make them suitable both for par and sequential use
  //all we have to do is make the point and means parallel sequences and then call the kmeans() function on them!

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    var MapOfPointsToMeans = points.groupBy(findClosest(_,means))
    //if some of the means don't have any points assigned they should still appear
    for (x<-means){
     if (MapOfPointsToMeans.contains(x)==false) {
       MapOfPointsToMeans+=(x, List())
      }
    }
    MapOfPointsToMeans
    //val MapOfPointsToMeans = points.groupBy(findClosest(_, means))
    //means.map((point: Point) => (point, MapOfPointsToMeans.getOrElse(point, GenSeq.empty))).toMap
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.length == 0) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    //this way with map we are sure that each point in the new 'oldMeans' every Point i corresponds to the ith element in 'oldMeans'
    oldMeans.map(PointOfMeans=>findAverage(PointOfMeans,classified(PointOfMeans)))

  }

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    //create sequence of tuples containint (oldMeans, corresponding newMeans)
    var tuplesOfOldAndNewMeans=oldMeans zip newMeans
    //each point in tuplesOfOldAndNewMeans is (oldMeansPoint, newMeansPoint). Point._1=oldMeansPoint and point._2=newMeansPoint
    //the result of the map will be a Sequence of length=oldMeans.length and each element will the squareDistance between old and new for that mean
    var SequenceOfSquareDistances=tuplesOfOldAndNewMeans.map(point=>point._1.squareDistance(point._2))
    //if all square distances are less than eta, return true
    SequenceOfSquareDistances.forall(x => x < eta)
  }

  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val classified = classify(points, means)
    val updated = update(classified, means)
    if (!(converged(eta)(means,updated))) {
      kMeans(points ,updated , eta)
    }
    else means

    // your implementation need to be tail recursive
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    println(k)
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
