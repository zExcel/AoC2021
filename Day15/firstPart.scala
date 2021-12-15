import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

val filename = "input.txt"
def vectors: List[(Int, Int)] = List((-1, 0), (0, -1), (1, 0), (0, 1))

val lowestRisks: Map[(Int, Int), Int] = Map()
val maxHolder: Int = 10000000

def getGridRisk(lowestRisks: ListBuffer[ListBuffer[Int]], row: Int, col: Int): Int = {
  if ((row, col) == (0, 0))
    return 0
  if (row < 0 || row >= lowestRisks.size || col  < 0  || col >= lowestRisks(0).size)
    maxHolder
  else
    lowestRisks(row)(col)
}

def findLowestRiskPath(riskGrid: List[List[Int]]): Int = {
  val lowestRisks = ListBuffer.tabulate(riskGrid.size, riskGrid(0).size)((r, c) => maxHolder)
  for (times <- (1 to 5)) {
    
    (for (row <- (0 until riskGrid.size); col <- (0 until riskGrid(0).size)) yield (row, col)).map(pos => {
      val r = pos._1
      val c = pos._2
      if (pos == (0, 0))
        lowestRisks(r)(c) = 0
      else
        lowestRisks(r)(c) = List(getGridRisk(lowestRisks, r - 1, c), 
          getGridRisk(lowestRisks, r, c - 1), 
          getGridRisk(lowestRisks, r + 1, c),
          getGridRisk(lowestRisks, r, c + 1)).min + riskGrid(r)(c)
    })
  }
  if (filename == "worstCaseInput.txt")
    lowestRisks.map(println)
  lowestRisks(lowestRisks.size - 1)(lowestRisks(0).size - 1)
}

val lines = Source.fromFile(filename).getLines.toList.map(_.split("").map(_.toInt).toList)

println(findLowestRiskPath(lines))
