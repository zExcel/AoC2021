import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "input.txt"
var horizontal = 0
var aim = 0
var depth = 0
for (line <- Source.fromFile(filename).getLines) {
  val direction = line.split(" ")(0)
  val amount = line.split(" ")(1).toInt
  direction match {
    case "forward" => horizontal += amount; depth += aim * amount
    case "down" => aim += amount
    case "up" => aim -= amount
  }
}

println(horizontal * depth)

