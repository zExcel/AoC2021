import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.SortedMap

def progressDay(spawnCounts: SortedMap[Long, Long]): SortedMap[Long, Long] =
  spawnCounts.foldRight(SortedMap[Long,Long]())((count, newSpawnCounts) =>
      if (count._1 == 0L)
        newSpawnCounts + (6L -> (spawnCounts.get(7).getOrElse(0L) + count._2), 8L -> count._2)
      else
        newSpawnCounts + ((count._1 - 1L) -> count._2))

def progressMultipleDays(spawnCounts: SortedMap[Long, Long], days: Int): SortedMap[Long, Long] = {
  if (days == 0)
    spawnCounts
  else
    progressMultipleDays(progressDay(spawnCounts), days - 1)
}

val filename = "input.txt"
val spawns = Source.fromFile(filename).getLines.toList(0).split(",").toList.map(_.toLong)
println(spawns)


val counts = spawns.foldLeft(SortedMap[Long,Long]())((map, timer) =>
    map + (timer -> (1L + map.get(timer).getOrElse(0L))))

val progressCount = progressMultipleDays(counts, 256)
println(progressCount)
println(progressCount.foldLeft(0L)((acc, count) => acc + count._2))
