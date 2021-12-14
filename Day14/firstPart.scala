import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

def progressString(mappings: Map[String, String], currentString: String): String = {
  (for(x <- 0 until currentString.size - 1) yield x).foldLeft("")((newString, index) => {
    newString + currentString(index).toString + mappings.getOrElse(currentString(index).toString + currentString(index + 1).toString, "")
  }) + currentString(currentString.size - 1).toString
}

def getCounts(string: String): Map[String, Long] = {
  val pairMap = (for (i <- 0 until string.size - 1) yield i).foldLeft(Map[String, Long]())((map, index) => {
    val pair = string(index).toString + string(index + 1).toString
    map + (pair -> (map.getOrElse(pair, 0L) + 1L))
  })
  pairMap
}

def getCharCounts(string: String): Map[Char, Long] = {
  val counts = (for (i <- 'A' to 'Z') yield i).foldLeft(Map[Char,Long]())((map, char) => {
    map + (char -> string.count(_ == char))
  }).filter(_._2 > 0)
  counts
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList
val starting = lines.head
val pairs = lines.drop(2).map(_.split(" -> ").toList).map(list => (list(0), list(1)))
val mappings: Map[String, String] = pairs.toMap

val finalString = (for (i <- 1 to 10) yield i).foldLeft(starting)((newString, _) => {
  val string = progressString(mappings, newString)
  string
}).sorted

val counts = (for (i <- 'A' to 'Z') yield i).foldLeft(List(0))((list, char) => {
  list ++ List(finalString.count(_ == char))
}).filter(_ > 0).sorted

println("Answer: " + (counts(counts.size - 1) - counts(0)))

