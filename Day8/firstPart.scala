import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.SortedMap

def countUniqueSegments(values: List[String]): Int = {
  val uniqueSegments = values.foldLeft(0)((acc, value) => 
      if (List(2,3,4,7).contains(value.size))
        acc + 1
      else
        acc)
  println(values + " " + uniqueSegments)
  uniqueSegments
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList.map(_.split("[|]").toList.map(_.trim))
println(lines)

val answer = lines.foldLeft(0)((acc, line) => 
    acc + countUniqueSegments(line(1).split(" ").toList))
println(answer)
