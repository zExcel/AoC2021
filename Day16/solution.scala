import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import util.control.Breaks._
import java.math.BigInteger

val hexToBinMap: Map[String, String] = Map () + 
  ("0" -> "0000",
  "1" -> "0001",
  "2" -> "0010",
  "3" -> "0011",
  "4" -> "0100",
  "5" -> "0101",
  "6" -> "0110",
  "7" -> "0111",
  "8" -> "1000",
  "9" -> "1001",
  "A" -> "1010",
  "B" -> "1011",
  "C" -> "1100",
  "D" -> "1101",
  "E" -> "1110",
  "F" -> "1111")

val binToHexMap: Map[String, String] = for ((k, v) <- hexToBinMap) yield (v, k)
var versionSum: Int = 0

def convertHexToBinary(hex: String): String = 
  hex.foldLeft("")((bin, char) => bin + hexToBinMap(char.toString))


// (value, lastIndex)
def parseLiteralValues(packet: String): (String, Int) = {
  if (packet(0) == '0')
    (packet.substring(1,5), 5)
  else {
    val value = packet.substring(1,5)
    val restInfo = parseLiteralValues(packet.substring(5))
    
    (value + restInfo._1, restInfo._2 + 5)
  }
}

def parseLengthPackets(packet: String): (List[BigInteger], Int) = {
  if (packet.size == 0)
    return (List[BigInteger](), 0)

  val packetInfo = parsePacket(packet)
  val subPacketInfo = parseLengthPackets(packet.substring(packetInfo._2))
  (packetInfo._1 :: subPacketInfo._1, packetInfo._2 + subPacketInfo._2)
}

def parseSubPackets(packet: String, packetsLeft: Int): (List[BigInteger], Int) = {
  if (packetsLeft == 0)
    return (List[BigInteger](), 0)
  val packetInfo = parsePacket(packet)
  val nextPacketInfo = parseSubPackets(packet.substring(packetInfo._2), packetsLeft - 1)
  (packetInfo._1 :: nextPacketInfo._1, packetInfo._2 + nextPacketInfo._2)
}

def getPacketValue(subPacketValues: List[BigInteger], typeId: Int): BigInteger = {
  if (typeId == 0)
    subPacketValues.foldLeft(BigInteger.ZERO)((acc, value) => acc.add(value))
  else if (typeId == 1)
    subPacketValues.foldLeft(BigInteger.ONE)((acc, value) => acc.multiply(value))
  else if (typeId == 2)
    subPacketValues.tail.foldLeft(subPacketValues.head)((acc, value) => acc.min(value))
  else if (typeId == 3)
    subPacketValues.tail.foldLeft(subPacketValues.head)((acc, value) => acc.max(value))
  else if (typeId == 5)
    (if (subPacketValues(0).compareTo(subPacketValues(1)) == 1) BigInteger.ONE else BigInteger.ZERO)
  else if (typeId == 6)
    (if (subPacketValues(0).compareTo(subPacketValues(1)) == -1) BigInteger.ONE else BigInteger.ZERO)
  else
    (if (subPacketValues(0).compareTo(subPacketValues(1)) == 0) BigInteger.ONE else BigInteger.ZERO)
}

// (value, indexVisitedLast)
def parsePacket(packet: String): (BigInteger, Int) = {
  if (packet.size == 0)
    return (BigInteger.ZERO, 0)

  val version = Integer.parseInt(packet.substring(0, 3), 2)
  versionSum += version
  val typeId = Integer.parseInt(packet.substring(3, 6), 2)
  
  if (typeId == 4) {
    val packetInfo: (String, Int) = parseLiteralValues(packet.substring(6))
    (new BigInteger(packetInfo._1, 2), packetInfo._2 + 6)
  } else {
    val lengthId = Integer.parseInt(packet.substring(6, 7))
    if (lengthId == 0) {
      val length = Integer.parseInt(packet.substring(7, 22), 2)
      val packetInfo = parseLengthPackets(packet.substring(22, 22 + length))
      (getPacketValue(packetInfo._1, typeId), 22 + length)
    } else {
      val subPackets = Integer.parseInt(packet.substring(7, 18), 2)
      val packetInfo = parseSubPackets(packet.substring(18), subPackets)
      (getPacketValue(packetInfo._1, typeId), packetInfo._2 + 18)
    }
  }
}

val filename = "input.txt"
val hex = Source.fromFile(filename).getLines.toList(0)
val bin = convertHexToBinary(hex)


val partTwoAnswer = parsePacket(bin)._1

println(s"Part one answer: $versionSum")

println(s"Part two answer: $partTwoAnswer")

