import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

val openers = Set('(', '[', '{', '<')
val chunkPairs = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
val pairValues = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

@tailrec
def findFirstCorrupted(chunk: String, stack: String): Int = {
  if (chunk.size == 0) {
    return 0
  }
  if (openers.contains(chunk.head)) {
    findFirstCorrupted(chunk.tail, chunk.head.toString + stack)
  } else if (chunkPairs(chunk.head) != stack.head) {
    pairValues(chunk.head)
  } else {
    findFirstCorrupted(chunk.tail, stack.tail)
  }
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList

val answer = lines.foldLeft(0)((acc, line) => 
    acc + findFirstCorrupted(line, ""))



println(answer)
