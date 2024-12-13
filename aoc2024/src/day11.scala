import os._

import scala.collection.{mutable => mu}

object day11 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day11_sample.txt"
    } else {
      os.resource / "day11.txt"
    }
  }

  val numbers: Vector[Long] = os.read.lines(filePath)(0).split(" ").map(y => y.toLong).toVector

  case class Stone(number: Long, evolution: Long)
  case class Result(sum: Long, remainingStones: List[Stone])
  case class Distance2Split(distance: Long, branches: List[Long])

  @main
  def day11_02(): Unit = {
    // println(numbers)
    // var list = List(1L) //0L +: numbers
    // var list = List(16192, 1, 18216, 12144, 2, 0, 2, 4, 16192, 1, 18216, 12144, 32, 77, 26, 8, 4, 0, 4, 8, 20, 24, 4, 0, 4, 8, 8, 0, 9, 6, 16192, 1, 18216, 12144, 2, 0, 2, 4, 16192, 1, 18216, 12144, 32, 77, 26, 8, 32, 77, 26, 8, 2, 0, 2, 4, 36, 86, 91, 84, 24, 57, 94, 56).map(x=>x.toLong)
    // var list = List(77L)
    val list  = numbers
    var depth = 25

    var mylist = list

    val cache = mu.Map.empty[Long, Distance2Split]
    val sizes = list.map(start => getSize(start, cache, depth))

    if (printstuff) {
      println(mylist)
      for (i <- 0 until depth) {
        mylist = mylist.flatMap(x => rules(x))
        println(mylist)
      }
      println(mylist.size)
      println("===")
      // println(rules(1036288))
      println(s"list size: ${list.size}")

      println(cache.map(x => x._1).toVector.sorted)
      val sizes2 = list.map(x => rules(x)).map(x => x.size)
      println(".......")
      sizes
        .zip(sizes2)
        .zip(list)
        .foreach(x => {
          val ((a, b), c) = x
          if (a != b.toLong) {
            println(c)
          }

        })
      println(".......")
      println(sizes)
    }
    val distances = cache.map(x => x._2.distance)
    println(distances.max)
    println(sizes.sum)
  }

  def getSize(start: Long, cache: mu.Map[Long, Distance2Split], depth: Int): Long = {
    val nmap = evolveTillSplit(cache, start)
    val dist = nmap(start)
    if (dist.distance > depth) {
      return 1
    } else {
      val pair     = dist.branches
      val subsizes = pair.toList.map(newstart => getSize(newstart, nmap, (depth - dist.distance).toInt))
      subsizes.sum
    }

  }

  def evolveTillSplit(paths: mu.Map[Long, Distance2Split], startPoint: Long): mu.Map[Long, Distance2Split] = {
    paths.get(startPoint) match
      case None => {
        var trail = List(startPoint)
        var cur   = startPoint
        var next  = rules(cur)

        while (next.size == 1 && !paths.contains(next.head)) {
          cur = next.head
          trail = cur +: trail
          next = rules(cur)
        }
        if (next.size > 1) {
          // we are at a split
          val split = next
          trail
            .zipWithIndex
            .map((point, idx) => {
              paths.addOne(point, Distance2Split(idx + 1, split))
            })
          paths
        } else {
          println(".")
          val hit = paths.get(next.head).get
          trail
            .zipWithIndex
            .map((point, idx) => {
              paths.addOne(point, Distance2Split(idx + hit.distance + 1, hit.branches))
            })
          paths
        }
      }
      case Some(value) =>
        paths
  }

  def stoneCountAfterBlinks(blinks: Int, stones: List[Long]): Long = {
    val initial = stones.groupMapReduce(identity)(_ => 1L)(_ + _)
    Iterator.iterate(initial)(blink).drop(blinks).next.values.sum
  }

  def blink(stonesMap: Map[Long, Long]): Map[Long, Long] = {
    stonesMap.toList.flatMap((stone, count) => blink(stone).map(_ -> count)).groupMapReduce(_._1)(_._2)(_ + _)
  }

  def blink(stone: Long): Iterator[Long] = {
    val stoneString = stone.toString
    val digits      = stoneString.size
    if stone == 0 then
      Iterator(1)
    else if digits % 2 == 0 then
      Iterator(stoneString.take(digits / 2).toLong, stoneString.drop(digits / 2).toLong)
    else {
      Iterator(stone * 2024)
    }
  }

  @main
  def day11_01(): Unit = {
    println(numbers)
    var list              = numbers
    var totalnums         = list.size
    var unique: Set[Long] = numbers.toSet
    for (i <- 0 until 25) {
      list = list.flatMap(x => rules(x))
      totalnums = totalnums + list.size
      unique = unique ++ list.toSet
      // println(s"$i: $list")
    }
    // println(rules(1036288))
    println(s"totalnum $totalnums, unique: ${unique.size} - avg reuse = ${totalnums / unique.size}")
    println(stoneCountAfterBlinks(6, List(125L, 17L)))
    println(list.size)
    println(stoneCountAfterBlinks(75, numbers.toList))
  }

  def rules(stone: Long): List[Long] = {
    if (stone == 0) {
      return List(1)
    } else {
      val nrdigits = getnumDigits(stone)
      if (nrdigits % 2 == 0) {
        split(nrdigits, stone)
      } else {
        List(stone * 2024)
      }
    }
  }

  def split(nrdigits: Int, stone: Long): List[Long] = {
    var x = 1
    for (i <- 0 until nrdigits / 2) {
      x = x * 10
    }
    val right = stone % x
    val left  = stone / x
    List(left, right)
  }

  def getnumDigits(stone: Long): Int = {
    var nrdigits = 1

    if (stone < 10) {
      return 1
    }

    var cur = stone
    while (cur >= 10) {
      cur = cur / 10
      nrdigits = nrdigits + 1
    }
    nrdigits
  }
}
