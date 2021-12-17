import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._


def rangeContainsPoint(xRange: (Int, Int), yRange: (Int, Int), point: (Int, Int)): Boolean = 
  (xRange._1 to xRange._2).contains(point._1) && (yRange._1 to yRange._2).contains(point._2)

def findNewVelocity(velocity: (Int, Int)): (Int, Int) =  
  ((if (velocity._1 < 0) velocity._1 + 1 else if (velocity._1 > 0) velocity._1 - 1 else 0), velocity._2 - 1)

def findElevationOfVelocity(xRange: (Int, Int), yRange: (Int, Int), velocity: (Int, Int)): Int = {
  (1 to 300).foldLeft((0, (0, 0), velocity))((info, iter) => {
    val highest = info._1
    val pos = info._2
    val vel = info._3
    val newPos = (pos._1 + vel._1, pos._2 + vel._2)
    if (rangeContainsPoint(xRange, yRange, newPos))
      return math.max(highest, newPos._2)
    if ((vel._2 < 0 && newPos._2 < yRange._1) || (vel._1 > 0 && newPos._1 > xRange._2))
      return -1
    if (vel._1 == 0 && newPos._1 < xRange._1)
      return -1
    (math.max(highest, newPos._2), newPos, findNewVelocity(vel))
  })
  -1
}

def findHighestElevationPoint(xRange: (Int, Int), yRange: (Int, Int)): (Int, Int) = {
  val highestElevMappings = (0 to xRange._2).map(x => (yRange._1 to 135).map(y => {
    val highestElev = findElevationOfVelocity(xRange, yRange, (x, y))
    highestElev
  }))

  val counter = highestElevMappings.foldLeft(0)((acc, row) => acc + row.foldLeft(0)((rowAcc, value) => rowAcc + (if (value != -1) 1 else 0)))
  val highestTotalElev = highestElevMappings.foldLeft(0)((acc, row) => math.max(acc, row.foldLeft(0)((rowAcc, value) => math.max(rowAcc, value))))

  (highestTotalElev, counter)
}

val filename = "input.txt"
val line = Source.fromFile(filename).getLines.toList(0)

val xRangeList = line.substring(line.indexOf("x") + 2, line.indexOf(",")).split("[..]").toList.filter(_.size>0).map(_.toInt)
val yRangeList = line.substring(line.indexOf("y") + 2).split("[..]").toList.filter(_.size>0).map(_.toInt)

val xRange = (xRangeList(0), xRangeList(1))
val yRange  = (yRangeList(0), yRangeList(1))

println(xRange + " " + yRange)

val answer = findHighestElevationPoint(xRange, yRange)
println(answer)
