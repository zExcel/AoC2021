import scala.io.Source
import scala.collection.mutable.ListBuffer

def checkRows(board: List[List[Int]]) : Boolean = 
  if (board.size == 0)
    false
  else 
    if (board(0).fold(0)((a, b) => a + b) == board(0).size)
      true
    else
      checkRows(board.drop(1))

def checkColumns(board: List[List[Int]], column: Int) : Boolean = 
  if (column == board(0).size)
    false
  else
    if (board.foldLeft(0)((acc, currRow) => acc + currRow(column)) == board.size)
      true
    else
      checkColumns(board, column + 1)

def getBoardValue(board: List[List[String]], calledBoard: List[List[Int]]) : Int = {
  calledBoard.zipWithIndex.foldLeft(0)((total, rowEntry) => rowEntry match {
    case (row, rIndex) => 
      total + row.zipWithIndex.foldLeft(0)((rowTotal, entry) => entry match {
      case (column, cIndex) =>
      rowTotal + (if (calledBoard(rIndex)(cIndex) == 0) board(rIndex)(cIndex).toInt else 0)
    })
  })
}

def boardIsComplete(board: List[List[Int]]) : Boolean = checkRows(board) || checkColumns(board, 0)

def findMoveEntry(move: String, board: List[List[String]]) : (Int, Int) = {
  board.zipWithIndex.foreach { case (row, rowIndex) =>
    if (row.indexOf(move) != -1)
      return (rowIndex, row.indexOf(move))
  }
  (-1, -1)
}

def updateCalledBoard(position: (Int, Int), calledBoard: List[List[Int]]) = {
  try {
    calledBoard.updated(position._1, calledBoard(position._1).updated(position._2, 1))
  } catch {
    case _ : Throwable => calledBoard
  }
}

def movesRequired(moveList: List[String], board: List[List[String]], moveCounter: Int, calledBoard: List[List[Int]]) : (Int, Int) = {
  if (moveCounter == moveList.size)
    (10000000, -1)
  else if (boardIsComplete(calledBoard))
    (moveCounter, getBoardValue(board, calledBoard))
  else
    movesRequired(moveList, board, moveCounter + 1, updateCalledBoard(findMoveEntry(moveList(moveCounter), board), calledBoard))
}

def scoreFirstCompleteBoard(moveList: List[String], boards: List[List[List[String]]]): Int = {
  boards.foldLeft((100000, 0))((acc, board) => 
        movesRequired(moveList, board, 0, List.fill(board.size)(List.fill(board(0).size)(0))) match {
          case (moves, score) => (if (moves < acc._1) (moves, score) else acc)
        }
      ) match {
        case (moves, score) => moveList(moves - 1).toInt * score
      }
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList.filter(_.length > 5)
val input = lines(0).split(",").toList
val boards = lines.drop(1).map(_.replaceAll("[ ]+", " ").trim.split(" ").toList).grouped(5).toList
val rows = boards(0).size
val columns = boards(0)(0).size

println(scoreFirstCompleteBoard(input, boards))
