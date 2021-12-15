import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

class Graph(val maxRepeats: Int = 2) {
  val map: Map[String, List[String]] = Map()
  val maxRepeatsIdentifier = "maxRepeatsIdentifier"

  def addConnection(first: String, second: String): Unit = {
    if (map.contains(first))
      map += (first -> (second :: map(first)))
    else
      map += (first -> List(second))
    
    if (map.contains(second))
      map += (second -> (first :: map(second)))
    else
      map += (second -> List(first))
  }

  def findPaths(): Long = {
    
    val queue: Queue[(String, scala.collection.immutable.Map[String, Int])] = Queue(("start", scala.collection.immutable.Map[String, Int]()))
    var counter = 0L
    while (!queue.isEmpty) {
      breakable {
        val current = queue.dequeue
        if (current._1 == "end"){
          counter += 1L
          break
        }
        if (current._1 == "start" && current._2.contains("start"))
          break
        
        val isLowerCase = ('a' to 'z').contains(current._1(0))
        if (isLowerCase && ((current._2.getOrElse(current._1, 0) == 1 && current._2.contains(maxRepeatsIdentifier)) || current._2.getOrElse(current._1, 0) == maxRepeats))
          break

        val newMap: scala.collection.immutable.Map[String, Int] = {
          if (!isLowerCase)
            current._2
          else {
            val newOccurrences = current._2.getOrElse(current._1, 0) + 1
            if (newOccurrences >= 2)
              current._2 + (current._1 -> newOccurrences) + (maxRepeatsIdentifier -> 1)
            else
              current._2 + (current._1 -> newOccurrences)
          }
        }
        map(current._1).foreach(node => queue.enqueue((node, newMap)))
      }    
    }
    counter
  }
    
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList.map(_.split("-").toList)
val graph =  new Graph()
lines.foreach(line => graph.addConnection(line(0), line(1)))

println(graph.findPaths)
