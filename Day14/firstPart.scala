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

