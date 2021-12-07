import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.SortedMap

def getMovePrice(spawns: List[Long], destination: Long, currentSpot: Long): Long = {
  val diff = math.abs(destination - currentSpot)
  diff * (diff + 1) / 2
}

def getTotalMovePrice(spawns: List[Long], destination: Long): Long = {
  val movePrice = spawns.foldLeft(0L)((acc, currentSpot) => acc + getMovePrice(spawns, destination, currentSpot))
  movePrice
}

def binarySearchMovePrice(spawns: List[Long], low: Long, high: Long): Long = {
  if (low == high)
    return getTotalMovePrice(spawns, low)
  val midPoint = ((low + high)/2).toLong
  val midPointValue = getTotalMovePrice(spawns, midPoint)
  if (getTotalMovePrice(spawns, midPoint - 1) < midPointValue)
    binarySearchMovePrice(spawns, low, midPoint)
  else
    math.min(midPointValue, binarySearchMovePrice(spawns, midPoint + 1, high))
}

val filename = "input.txt"
val spawns = Source.fromFile(filename).getLines.toList(0).split(",").toList.map(_.toLong).sorted
println(spawns)
println(binarySearchMovePrice(spawns, spawns(0), spawns(spawns.size - 1)))
