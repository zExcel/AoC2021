import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

val vectors: List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))

def getPosition(grid: List[List[Int]], row: Int, col: Int): Int = { 
  if (row < 0 || row >= grid.size || col < 0 || col >= grid(0).size)
    10
  else
    grid(row)(col)
}

def smallerThanSurrounding(grid: List[List[Int]], basin: Set[(Int, Int)], row: Int, col: Int): Int = { 
  if (row < 0 || row >= grid.size || col < 0 || col >= grid(0).size || grid(row)(col) == 9)
    return 0
  
  if (vectors.foldLeft(true)((acc, entry) => acc && (grid(row)(col) < getPosition(grid, row + entry._1, col + entry._2) || basin.contains((row + entry._1, col + entry._2)))))
    grid(row)(col) + 1
  else
    0
}

def neverGoesDown(grid: List[List[Int]], basin: Set[(Int, Int)], row: Int, col: Int): Boolean = {

  if (row < 0 || row >= grid.size || col < 0 || col >= grid(0).size || grid(row)(col) == 9)
    return false

  val value = grid(row)(col)
  val queue: Queue[(Int, Int)] = Queue((row, col))
  val visited: Set[(Int, Int)] = basin.clone

  while (!queue.isEmpty) {
    breakable {
      val entry = queue.dequeue()
      if (visited.contains(entry))
        break
      val position = getPosition(grid, entry._1, entry._2)
      visited += ((entry._1, entry._2))
      if (position < 9 && position >= value) {
        vectors.foreach(neighbors => (queue.enqueue((neighbors._1 + entry._1, neighbors._2 + entry._2))))
      } 
      else if (position < value)
        return false
    }
  }
  true
}

def floodFill(grid: List[List[Int]], row: Int, col: Int): Int = {
  if (!neverGoesDown(grid, Set[(Int, Int)](), row, col))
    return 1

  val queue: Queue[(Int, Int)] = Queue((row, col))
  val basin: Set[(Int, Int)] = Set()
  while (!queue.isEmpty) {
    breakable {
      val entry = queue.dequeue()
      if (basin.contains(entry))
        break
      if (smallerThanSurrounding(grid, basin, entry._1, entry._2) > 0 || neverGoesDown(grid, basin, entry._1, entry._2)) {
        basin += ((entry._1, entry._2))
        vectors.foreach(neighbors => (queue.enqueue((neighbors._1 + entry._1, neighbors._2 + entry._2))))

      }
    }
  }

  basin.size
}

def firstPartSolver(grid: List[List[Int]]): Int = { 
  val pairs = for(x <- (0 until grid.size); y <- (0 until grid(0).size)) yield (x, y)
  pairs.foldLeft(0)((acc, entry) => acc + smallerThanSurrounding(grid, Set[(Int,Int)](), entry._1, entry._2))

}

def findBoardValue(grid: List[List[Int]]): Int = {
  val pairs = for(x <- (0 until grid.size); y <- (0 until grid(0).size)) yield (x, y)
  val basinSizes = pairs.foldLeft(List[Int]())((acc, entry) => floodFill(grid, entry._1, entry._2) :: acc).sorted.reverse
  basinSizes(0) * basinSizes(1) * basinSizes(2)

}


val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList.map(_.split("").toList.map(_.toInt))


println("First part solution: " + firstPartSolver(lines))
println("Second part solution: " + findBoardValue(lines))
