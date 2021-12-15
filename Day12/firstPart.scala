import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._

class Graph {
  val map: Map[String, List[String]] = Map()

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
    
    var counter: Long = 0
    val queue: Queue[(String, scala.collection.immutable.Set[String])] = Queue(("start", scala.collection.immutable.Set[String]()))

    while (!queue.isEmpty) {
      breakable {
        val current = queue.dequeue
        if (current._1 == "end"){
          counter += 1
          break
        }

        if (('a' to 'z').contains(current._1(0)) && current._2.contains(current._1))
          break

        val newSet = current._2 + current._1
        map(current._1).foreach(node => queue.enqueue((node, newSet)))
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
