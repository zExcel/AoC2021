import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

val openers = Set('(', '[', '{', '<')
val closingMap = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
val chunkPairs = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
val charValues = Map('(' -> 1L, '[' -> 2L, '{' -> 3L, '<' -> 4L)

def findIncompleteChunk(chunk: String, stack: String): String = {
  if (chunk.size == 0)
    return stack
  if (openers.contains(chunk.head)) {
    findIncompleteChunk(chunk.tail, chunk.head.toString + stack)
  } else if (chunkPairs(chunk.head) != stack.head) {
    ""
  } else if (stack.size == 0) {
    ""
  } else {
    findIncompleteChunk(chunk.tail, stack.tail)
  }
}

def findIncompleteValue(line: String): Long = {
  findIncompleteChunk(line, "").foldLeft(0L)((acc, char) => 
      5  * acc + charValues(char))
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList

val lineValues = lines.map(line => findIncompleteValue(line)).filter(_ > 0L).sorted

println(lineValues(lineValues.size / 2))
