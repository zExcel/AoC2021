import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.SortedMap

def progressDay(spawnCounts: SortedMap[Int, Int]): SortedMap[Int, Int] =
  spawnCounts.foldRight(SortedMap[Int,Int]())((count, newSpawnCounts) =>
      if (count._1 == 0)
        newSpawnCounts + (6 -> (spawnCounts.get(7).getOrElse(0) + count._2), 8 -> count._2)
      else
        newSpawnCounts + ((count._1 - 1) -> count._2))

def progressMultipleDays(spawnCounts: SortedMap[Int, Int], days: Int): SortedMap[Int, Int] = {
  if (days == 0)
    spawnCounts
  else
    progressMultipleDays(progressDay(spawnCounts), days - 1)
}

val filename = "input.txt"
val spawns = Source.fromFile(filename).getLines.toList(0).split(",").toList.map(_.toInt)
println(spawns)


val counts = spawns.foldLeft(SortedMap[Int,Int]())((map, timer) =>
    map + (timer -> (1 + map.get(timer).getOrElse(0))))

val progressCount = progressMultipleDays(counts, 80)
println(progressCount)
println(progressCount.foldLeft(0)((acc, count) => acc + count._2))
