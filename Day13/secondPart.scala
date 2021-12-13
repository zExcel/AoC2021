import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

class Grid(var marked: Set[(Int, Int)]) {
  
  def foldX(xValue: Int): Unit = {
    this.marked = for (value <- marked) yield {
      val diff = math.abs(xValue - value._1)
      if (value._1 < xValue)
        (value._1, value._2)
      else
        (xValue - diff, value._2)
    }
  }

  def foldY(yValue: Int): Unit = {
    this.marked = for (value <- marked) yield {
      val diff = math.abs(yValue - value._2)
      if (value._2 < yValue)
        (value._1, value._2)
      else
        (value._1, yValue - diff)
    }
  }

  def applyInstruction(instruction: String): Unit = {
    if (instruction.contains('x'))
      this.foldX(instruction.split("=").toList(1).toInt)
    else
      this.foldY(instruction.split("=").toList(1).toInt)
  }
  
  def printGrid(): Unit = {
    val minX = marked.foldLeft(100000)((acc, value) =>
        math.min(acc, value._1))
    val largestX = marked.foldLeft(-100000)((acc, value) =>
        math.max(acc, value._1))
    val minY = marked.foldLeft(100000)((acc, value) =>
        math.min(acc, value._2))
    val largestY = marked.foldLeft(-10000)((acc, value) =>
        math.max(acc, value._2))

    val tempList = List.tabulate(largestY - minY + 1, largestX - minX + 1)((row, col) => {
      if (marked.contains((col, row)))
        '#'
      else
        '.'
    })
    tempList.map(println)
  }
}


val filename = "input.txt"
val coords: List[List[Int]] = Source.fromFile(filename).getLines.toList.filter(line =>
    line.size > 0 && line(0) != 'f').map(_.split(",").toList.map(_.toInt))
val coordsSet: Set[(Int, Int)] = coords.map(coord =>
    (coord(0), coord(1))).toSet

val instructions = Source.fromFile(filename).getLines.toList.drop(coords.size + 1)

val grid: Grid = new Grid(coordsSet)

instructions.map(instruction =>
    grid.applyInstruction(instruction))

grid.printGrid
println(grid.marked.size)
