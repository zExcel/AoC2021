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

def addToMap(currentPairs: Map[String, Long], newMap: Map[String, Long], mappings: Map[String, String], pair: String): Map[String, Long] = {
  if (!mappings.contains(pair))
    return newMap + (pair -> (newMap.getOrElse(pair, 0L) + currentPairs(pair)))
  val mapping = mappings(pair)
  val firstPair = pair(0).toString + mapping
  val secondPair = mapping + pair(1).toString
  if (firstPair == secondPair)
    newMap + (firstPair -> (newMap.getOrElse(firstPair, 0L) + currentPairs(pair) * 2))
  else
    newMap + (firstPair -> (newMap.getOrElse(firstPair, 0L) + currentPairs(pair)), secondPair -> (newMap.getOrElse(secondPair, 0L) + currentPairs(pair)))
}

def progressStringMap(mappings: Map[String, String], currentPairs: Map[String, Long]): Map[String, Long] = {
  currentPairs.foldLeft(Map[String, Long]())((newMap, pair) => {
    addToMap(currentPairs, newMap, mappings, pair._1)
  })
}

def countDoubles(char: Char, prevMap: Map[String, Long]): Long = {
  prevMap.getOrElse(char.toString + char.toString, 0L)
}

def subtractFromCounts(mappings: Map[String, String], counts: Map[Char, Long], previous: ListBuffer[Map[String, Long]]): Map[Char, Long] = {
  (for (i <- 'A' to 'Z') yield i).foldLeft(counts)((map, char) => {
    val amountToSubtract = mappings.foldLeft(0L)((amount, pair) => {
      if (pair._2.contains(char)) {
        val change = previous.foldLeft(0L)((previousAcc, prevMap) => previousAcc + prevMap.getOrElse(pair._1, 0L)) 
        amount + change
      }
      else
        amount
    }) + countDoubles(char, previous(0))
    map + (char -> (counts.getOrElse(char, 0L) - amountToSubtract))
  }).filter(_._2 > 0)
}

def getPairCounts(string: String): Map[String, Long] = {
  val pairMap = (for (i <- 0 until string.size - 1) yield i).foldLeft(Map[String, Long]())((map, index) => {
    val pair = string(index).toString + string(index + 1).toString
    map + (pair -> (map.getOrElse(pair, 0L) + 1L))
  })
  pairMap
}

def getCounts(answer: Map[String, Long], mappings: Map[String, String], allPairCounts: ListBuffer[Map[String, Long]]): Map[Char, Long] = {
  val counts = (for (i <- 'A' to 'Z') yield i).foldLeft(Map[Char, Long]())((map, char) => {
    val charCount = map + (char -> (answer.foldLeft(0L)((count, pair) => {
      if (pair._1 == (char.toString + char.toString))
        count + 2 * pair._2
      else
        count + (if (pair._1.contains(char)) 1 else 0) * pair._2
    })))
    charCount
  }).filter(_._2 > 0L)
  subtractFromCounts(mappings, counts, allPairCounts)
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList
val starting = lines.head
val pairs = lines.drop(2).map(_.split(" -> ").toList).map(list => (list(0), list(1)))
val mappings: Map[String, String] = pairs.toMap

val startingMap: Map[String, Long] = getPairCounts(starting)
val allPairCounts: ListBuffer[Map[String, Long]] = ListBuffer(startingMap)

val penult = (for (i <- 1 to 39) yield i).foldLeft(startingMap)((map, _) => {
  val progressedMap = progressStringMap(mappings, map)
  allPairCounts += progressedMap
  progressedMap
})

val answer = progressStringMap(mappings, penult)

val finalCounts = getCounts(answer, mappings, allPairCounts)

println("Answer: " + (finalCounts.valuesIterator.max - finalCounts.valuesIterator.min))
