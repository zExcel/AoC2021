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

def getMinimumMovePrice(spawns: List[Long]): Long = {
  (0L until spawns(spawns.size - 1)).foldLeft(10000000000L)((acc, destination) =>
      math.min(acc, getTotalMovePrice(spawns, destination)))
}

val filename = "input.txt"
val spawns = Source.fromFile(filename).getLines().toList(0).split(",").toList.map(_.toLong).sorted

val t1 = System.nanoTime
val slowSolution = getMinimumMovePrice(spawns)
val t2 = System.nanoTime
val solution = binarySearchMovePrice(spawns, spawns(0), spawns(spawns.size - 1))
val t3 = System.nanoTime
val slowTime  = (t2 - t1)/1e9d
val fastTime = (t3 - t2)/1e9d

assert(slowSolution == solution)
println("The solution is: " + solution)
println("It took: " + slowTime + "s to find the solution the slow way")
println("It took: " + fastTime + "s to find the solution the fast way")
println("Sped up code is: " + slowTime / fastTime + " times faster")
