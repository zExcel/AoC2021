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

def getMinimumMovePrice(spawns: List[Long]): Long = {
  (0L until spawns(spawns.size - 1)).foldLeft(10000000000L)((acc, destination) =>
      math.min(acc, getTotalMovePrice(spawns, destination)))
}

val filename = "input.txt"
val spawns = Source.fromFile(filename).getLines.toList(0).split(",").toList.map(_.toLong).sorted
println(spawns)
println(getMinimumMovePrice(spawns))
