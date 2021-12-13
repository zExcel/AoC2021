import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

val vectors = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1))

def increaseSpot(grid: List[List[Int]], flashedSpots: List[List[Boolean]], row: Int, col: Int): List[List[Int]] = {
  if (row < 0 || row >= grid.size || col < 0 || col >= grid(0).size || flashedSpots(row)(col))
    return grid

  val newGrid = grid.updated(row, grid(row).updated(col, grid(row)(col) + 1))
  newGrid
}

def flashSpot(grid: List[List[Int]], flashedSpots: List[List[Boolean]], row: Int, col:  Int): List[List[Int]] = {
  if (flashedSpots(row)(col))
    grid
  else {
    val tempFlashedSpots = flashedSpots.updated(row, flashedSpots(row).updated(col, true))
    val newGrid = grid.updated(row, grid(row).updated(col, 0))
    vectors.foldLeft(newGrid)((acc, vector) => 
        increaseSpot(acc, tempFlashedSpots, row + vector._1, col + vector._2))
  }
}

def findSpotToFlash(grid: List[List[Int]], flashedSpots: List[List[Boolean]]): (Int, Int) = {
  for (row <- (0 until grid.size); col <- (0 until grid(0).size)) {
    if (grid(row)(col) >= 10 && flashedSpots(row)(col) == false)
      return (row, col)
  }
  (-1, -1)
}

def progressGrid(grid: List[List[Int]], flashedSpots: List[List[Boolean]], row: Int, col: Int, flashes: Int): (List[List[Int]], List[List[Boolean]], Int) = {
  //println(row + " " + col)
  //grid.map(println)
  //println()
  if (row >= grid(0).size) {
    val spotToFlash = findSpotToFlash(grid, flashedSpots)
    if (spotToFlash == (-1, -1))
      (grid, flashedSpots, flashes)
    else {
      val flashedGrid = flashSpot(grid, flashedSpots, spotToFlash._1, spotToFlash._2)
      val updatedFlashed = flashedSpots.updated(spotToFlash._1, flashedSpots(spotToFlash._1).updated(spotToFlash._2, true))
      progressGrid(flashedGrid, updatedFlashed, row, col, flashes + 1)
    }
  } else {
    val updatedGrid = grid.updated(row, grid(row).updated(col, grid(row)(col) + 1))
    progressGrid(updatedGrid, flashedSpots, row + (col + 1) / grid(0).size, (col + 1) % grid(0).size, flashes)
  }
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList.map(_.split("").toList.map(_.toInt))
val flashedSpots = List.fill(lines.size)(List.fill(lines(0).size)(false))

val solution = (1 to 10000).foldLeft((lines, flashedSpots, 0))((acc, step) => {
    val temp = progressGrid(acc._1, flashedSpots, 0, 0, acc._3)
    if (temp._3 - acc._3 == lines.size * lines(0).size)
      println(step)
    temp
  }
)


println(solution._3)

