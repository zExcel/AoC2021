import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "input.txt"
val lines = Source.fromFile(filename).getLines.toList
var counters = Vector.fill(lines(0).length)(0)
lines.map(line => {
  line.zipWithIndex.foreach{
    case (value, index) => counters = counters.updated(index, counters(index) + value.asDigit) 
  }
})
println(lines.size)
val finalValue = counters.foldLeft(0)((workValue, currentValue) => 
    workValue * 2 + (if(currentValue > lines.size/2) 1 else 0))

println(finalValue  * (scala.math.pow(2,counters.size) - 1 - finalValue))
