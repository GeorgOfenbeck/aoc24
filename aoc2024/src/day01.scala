import os._
import scala.collection.mutable.ArrayBuffer

object Day1 {
  @main
  def day01_02(): Unit = {
    val filePath = os.resource / "day1.txt"

    def addMap(map: Map[Long, Long], key: Long, value: Long): Map[Long, Long] = {
      map + (key -> (map.getOrElse(key, 0L) + value))
    }
    var lmap = Map.empty[Long, Long]
    var rmap = Map.empty[Long, Long]
    os.read.lines(filePath).map { line =>
      if (!line.isEmpty()) {
        val numbers = line.trim.split("\\s+").map(_.toLong)
        if (numbers.size == 2) {
            lmap = addMap(lmap, numbers(0), 1)
            rmap = addMap(rmap, numbers(1), 1)
        }
      }
    }
    var sum = 0L
    for ((k, v) <- lmap) {
        val right = rmap.getOrElse(k, 0L)
        sum = sum + k * v * right
    }
    println(sum)
  }



  def day01_01(): Unit = {
    val filePath = os.resource / "day1.txt"

    import scala.collection.mutable.PriorityQueue
    import scala.math.Ordering

    val minHeapLeft  = PriorityQueue.empty[Long](Ordering[Long].reverse)
    val minHeapRight = PriorityQueue.empty[Long](Ordering[Long].reverse)
    val left         = ArrayBuffer.empty[Long]
    val right        = ArrayBuffer.empty[Long]
    var count        = 0
    os.read.lines(filePath).map { line =>
      if (!line.isEmpty()) {
        val numbers = line.trim.split("\\s+").map(_.toLong)
        if (numbers.size == 2) {
          minHeapLeft.enqueue(numbers(0))
          minHeapRight.enqueue(numbers(1))
          left.append(numbers(0))
          right.append(numbers(1))

        }
      }
    }

    left.sortInPlace()
    right.sortInPlace()

    var sum = 0L
    for (i <- 0 until left.size) {
      val l = left(i)
      val r = right(i)

      val diff = Math.abs(l - r)
      sum += diff
      // os.write.append(os.pwd / "output.txt", s"$i l: $l ${minHeapLeft.dequeue()}, r: $r ${minHeapRight.dequeue()}, diff: $diff, sum: $sum \n")
    }
//    println(sum)

  }

}
