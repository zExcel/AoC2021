import scala.io.Source
import scala.collection.mutable.ListBuffer

def findRating(list: List[String], sourceCreator: (List[String], Int, Int, Int) => String, sourceString: String, index: Int) : String = 
  if (list.size == 1)
    list(0)
  else {
    val newList = list.filter(string => string(index) == sourceString(0))
    if (newList.size == 1)
      return newList(0)
    val newSource = sourceCreator(newList, index + 1, newList.size, 0)
    findRating(newList, sourceCreator, newSource, index + 1)
  }

def findMostCommon(list: List[String], index: Int, cutoff: Int, oneCounter: Int) : String = 
  if (list.size == 0)
    if (oneCounter >= cutoff/2.0) "1" else "0"
  else {
    findMostCommon(list.drop(1), index, cutoff, oneCounter + (if (list(0)(index) == '1') 1 else 0))
  }

def findLeastCommon(list: List[String], index: Int, cutoff: Int, oneCounter: Int) : String  = 
  if (list.size == 0)
    if (oneCounter >= cutoff/2.0) "0" else "1"
  else
    findLeastCommon(list.drop(1), index, cutoff, oneCounter + (if (list(0)(index) == '1') 1 else 0))

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList
var counters = Vector.fill(lines(0).length)(0)
lines.map(line => {
  line.zipWithIndex.foreach{
    case (value, index) => counters = counters.updated(index, counters(index) + value.asDigit) 
  }
})
val mostCommonString = counters.foldLeft("")((workValue, currentValue) => 
    workValue + (if(currentValue >= lines.size/2) "1" else "0"))

val leastCommonString = counters.foldLeft("")((workValue, currentValue) => 
    workValue + (if(currentValue >= lines.size/2) "0" else "1"))

println(mostCommonString + " " + leastCommonString)

var oxygenList = lines
var scrubberList = lines

val oxygenRating = Integer.parseInt(findRating(lines, findMostCommon, mostCommonString(0).toString, 0), 2)

val COTwoRating = Integer.parseInt(findRating(lines, findLeastCommon, leastCommonString(0).toString, 0), 2)

println(oxygenRating + " " + COTwoRating)
println(oxygenRating *  COTwoRating)
