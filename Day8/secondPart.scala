import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.SortedMap

def inCommonCharacters(first: String, second: String) : Int = 
  first.size - first.filter(char => !(second.contains(char))).size

def determineMiddle(candidates: List[String]) : String = 
    if (inCommonCharacters(candidates(0), candidates(1)) == inCommonCharacters(candidates(0), candidates(2)))
      candidates(0)
    else if (inCommonCharacters(candidates(1), candidates(0)) == inCommonCharacters(candidates(1), candidates(2)))
      candidates(1)
    else
      candidates(2)

def determineNine(input: List[String], four: String) : String = {
  val commonChars = input(0).toSet.filter(char => (input(1).contains(char) && input(2).contains(char)))
  if (input(0).filter(char => !(commonChars.contains(char))).foldLeft(true)((acc, char) => acc && (four.contains(char))))
    input(0)
  else if (input(1).filter(char => !(commonChars.contains(char))).foldLeft(true)((acc, char) => acc && (four.contains(char))))
    input(1)
  else
    input(2)
}

def determineZero(input: List[String], one: String) : String = { 
  val commonChars = input(0).toSet.filter(char => (input(1).contains(char)))
  if (one.contains(input(0).filter(char => !(commonChars.contains(char)))(0)))
    input(0)
  else
    input(1)
}

def decodeInput(input: List[String], secretCode: List[String]) : Int = {
  val one = input(0)
  val seven = input(1)
  val four = input(2)
  val eight = input(9)

  val nineCands = input.filter(_.size == 6)
  val nine = determineNine(nineCands, four)

  val zeroCands = nineCands.filter(_ != nine)
  val zero = determineZero(zeroCands, one)
  val six = zeroCands.filter(_ != zero)(0)

  val threeCands = input.filter(_.size == 5)
  val three = determineMiddle(threeCands)

  val fiveCands = threeCands.filter(_ != three)
  val five = {
    if (four.contains(fiveCands(0).filter(char => {!(three.toSet.contains(char))})(0)))
      fiveCands(0)
    else
      fiveCands(1)
  }
  val two = fiveCands.filter(_ != five)(0)
  
  val digits: List[String] = List(zero, one, two, three, four, five, six, seven, eight, nine).map(_.sorted)

  secretCode.foldLeft(0)((acc, code) => 10 * acc + digits.indexOf(code.sorted))
}

def decodeInputs(input: List[List[String]], secretCodes: List[List[String]]): Int = {
  input.zipWithIndex.map(entry => decodeInput(entry._1, secretCodes(entry._2))).foldLeft(0)(_ + _)
}

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList.map(_.split("[|]").toList.map(_.trim))
val inputCodes = lines.map(line => line(0).split(" ").toList.sortBy(_.size))
val secretCodes = lines.map(line => line(1).split(" ").toList)
println(decodeInputs(inputCodes, secretCodes)) 
