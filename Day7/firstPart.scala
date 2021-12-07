import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.SortedMap

def getNewDiff(spawns: List[Int], currentDiff: Int, index: Int): Int = {
  if (index == spawns.size - 1)
    currentDiff
  else
    currentDiff + (2 * index + 1 - spawns.size + 2) * (spawns(index + 1) - spawns(index)) + spawns(index) - spawns(index + 1) 
}

def smallestDiff(spawns: List[Int], currentDiff: Int, index: Int): Int = {
  println(currentDiff + " " + index)
  if (index == spawns.size)
    currentDiff
  else
    math.min(currentDiff, smallestDiff(spawns, getNewDiff(spawns, currentDiff, index), index + 1))
}

val filename = "input.txt"
val spawns = Source.fromFile(filename).getLines.toList(0).split(",").toList.map(_.toInt).sorted
println(spawns)
val sum = spawns.foldLeft(0)(_ + _)
val initialDiff = spawns.drop(1).foldLeft(0)((acc, value) => acc + value - spawns(0))
println(smallestDiff(spawns, initialDiff, 0))
