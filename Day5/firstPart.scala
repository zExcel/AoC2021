import scala.io.Source
import scala.collection.mutable.ListBuffer



def addCounterFromLine(ventLine: List[(Int, Int)], grid: List[List[Int]]): List[List[Int]] = {
  if (ventLine(0)._1 == ventLine(1)._1)
    (math.min(ventLine(0)._2, ventLine(1)._2) to math.max(ventLine(0)._2, ventLine(1)._2)).foldLeft(grid)((accGrid, row) => accGrid.updated(row, accGrid(row).updated(ventLine(0)._1, accGrid(row)(ventLine(0)._1) + 1)))
  else if (ventLine(0)._2 == ventLine(1)._2)
    (math.min(ventLine(0)._1, ventLine(1)._1) to math.max(ventLine(0)._1, ventLine(1)._1)).foldLeft(grid)((accGrid, col) => accGrid.updated(ventLine(0)._2, accGrid(ventLine(0)._2).updated(col, accGrid(ventLine(0)._2)(col) + 1)))
  else {
    val rowRange = ventLine(0)._2 to ventLine(1)._2 by (if (ventLine(0)._2 > ventLine(1)._2) -1 else 1)
    val colRange = ventLine(0)._1 to ventLine(1)._1 by (if (ventLine(0)._1 > ventLine(1)._1) -1 else 1)
    (0 until rowRange.size).foldLeft(grid)((accGrid, index) => 
        accGrid.updated(rowRange(index), accGrid(rowRange(index)).updated(colRange(index), accGrid(rowRange(index))(colRange(index)) + 1)))
  }
}

def fillGrid(ventLines: List[List[(Int, Int)]], grid: List[List[Int]]): List[List[Int]] =
  if (ventLines.size == 0)
    grid
  else
    fillGrid(ventLines.drop(1), addCounterFromLine(ventLines(0), grid))

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList
val ventLines = lines.map(line => line.split(" -> ").toList
  .map(pair => pair.split(',').toList.map(number => number.toInt) match {
    case List(a, b) => (a, b)}))
val maxRows = ventLines.foldLeft(0)((acc, ventList) => 
    math.max(math.max(acc, ventList(0)._2), ventList(1)._2))
val maxCols = ventLines.foldLeft(0)((acc, ventList) => 
    math.max(math.max(acc, ventList(0)._1), ventList(1)._1))

val grid = List.fill(maxRows + 1)(List.fill(maxCols + 1)(0))

val dangerousPoints = fillGrid(ventLines, grid).foldLeft(0)((acc, row) => 
    acc + row.filter(_ > 1).size)

println(dangerousPoints)

