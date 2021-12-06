import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "inputDay1.txt"
val numbers = ListBuffer[Int]()
for (line <- Source.fromFile(filename).getLines) {
    val currentValue = line.toInt
    numbers += currentValue
}

var counter = 0
for (i <- 0 until numbers.size - 3) {
  if (numbers(i) + numbers(i + 1) + numbers(i + 2) < numbers(i + 1) + numbers(i + 2) + numbers(i + 3)) {
    counter = counter + 1
  }
}

println(counter)
