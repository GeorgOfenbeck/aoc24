import os._
import scala.util.boundary, boundary.break

import scala.collection.immutable.{TreeSet => TS}

object day09 {

  
  val test = false
  val printstuff = test
  val filePath = {
    if (test) {
      os.resource / "day9_sample.txt"
    } else {
      os.resource / "day9.txt"
    }
  }

  val input: String = os.read.lines(filePath)(0)

  case class AFile(id: Option[Int], times: Int)

  object Space {
    given gapOrder: Ordering[Space] with
      def compare(x: Space, y: Space): Int = x.index.compare(y.index)
  }

  case class DominatGap(gap: Space, dominated: TS[DominatGap])
  object DominatGap {
    given Ordering[DominatGap] with
      def compare(x: DominatGap, y: DominatGap): Int = x.gap.times.compare(y.gap.times)
  }

  sealed trait BFile {
    def times: Int
    def index: Int
  }
  case class File(id: Int, times: Int, index: Int) extends BFile
  case class Space(times: Int, index: Int)         extends BFile

  // Tree -> odered by GapSize => smallest Idx

  @main
  def day09_02(): Unit = {
    val decomp = decompress3(input = input)
    val onlyGaps = {
      decomp
        .foldLeft(List.empty[Space])((acc, ele) => {
          ele match
            case File(id, times, index) =>
              acc
            case s: Space =>
              s +: acc
        })
        .reverse
    }

    val minVec = Vector(
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
      scala.collection.mutable.TreeSet.empty[Space],
    )

    onlyGaps.map { space =>
      {
        if (space.times > 0) {
          minVec(space.times - 1).addOne(space)
        }
      }
    }

    val decev = decomp.reverse.toArray

    val check = decev.map(x => x.times.toLong).sum

    for (i <- 0 until decev.size) {
      val ele = decev(i)
      ele match
        case f: File => {
          val fit = findSmallestFit(f, minVec)
          fit.map { space =>
            {
              decev(i) = f.copy(index = space.index)
              val newSpace = Space(times = space.times - f.times, index = space.index + f.times)
              assert(newSpace.times + f.times == space.times)
              update(minVec, space, newSpace)
            }
          }   
        }
        case Space(times, index) =>
      
      assert(check == decev.map(x => x.times.toLong).sum)
    }

    val nmap = {
      decev.foldLeft(Map.empty[Int, File]) { (acc, ele) =>
        {
          ele.match
            case f: File =>
              acc + (f.index -> f)
            case Space(times, index) =>
              acc

        }
      }
    }
    var i          = 0
    var sum2       = 0L
    var max        = decev.map(x => x.index).max + 10
    while (i < max) {
      val x = nmap.get(i)
      x.match
        case None => {
          if (printstuff) {
            print(".")
          }
          i = i + 1
        }
        case Some(value) => {
          for (j <- 0 until value.times) {
            sum2 = sum2 + value.id * i.toLong
            if (printstuff) {
              print(s"[${value.id}]")
            }
            i = i + 1
          }
        }
    }
    println
    println(sum2)
    println("----")

    val maxid = {
      decomp
        .map(x => {
          x.match
            case File(id, times, index) =>
              id
            case Space(times, index) =>
              0
        })
        .max
    }

    println(maxid)
  }

  def update(minVec: Vector[scala.collection.mutable.TreeSet[Space]], from: Space, to: Space) = {
    val before = minVec(from.times -1 ).size
    minVec(from.times - 1).remove(from)
    val after = minVec(from.times - 1).size
    assert(after + 1 == before) 
    if (to.times >= 1) {
      minVec(to.times - 1).addOne(to)
    }
  }

  def findSmallestFit(b: File, minVec: Vector[scala.collection.mutable.TreeSet[Space]]): Option[Space] = {
    val options = minVec.filter(tree => !tree.isEmpty).map(_.min).filter(s => s.times >= b.times && s.index < b.index)
    options.sorted.headOption
  }

  @main
  def day09_02_old(): Unit = {
    val decomp = decompress3(input = input)
    // printA(decomp)
    val onlyGaps = {
      decomp
        .foldLeft(List.empty[Space])((acc, ele) => {
          ele match
            case File(id, times, index) =>
              acc
            case s: Space =>
              s +: acc
        })
        .reverse
    }
    val gaptree = gapList(onlyGaps.toVector)
    // printGapTree(gaptree, 0)
    // val vec: Vector[Space] = treeToList(gaptree)
    // printGapTree(gapList(vec), 0)
    /*
    updateGap(1, gaptree).map { (updated, space) =>

      println(s"space: ${space.index}")
      printGapTree(updated, 0)
    }*/

    val devec = decomp.reverse.toArray

    var worktree = gaptree

    for (i <- 0 until devec.size) {
      val ele = devec(i)
      ele match
        case File(id, times, index) => {
          updateGap(times, worktree).map { (newtree, space) =>
            if (space.index < index) {
              // println(s"$index -> ${space.index}")
              devec(i) = File(id, times, space.index)
              worktree = newtree
            }
          }
        }
        case Space(times, index) =>
    }

    val nmap = {
      devec.foldLeft(Map.empty[Int, File]) { (acc, ele) =>
        {
          ele.match
            case f: File =>
              acc + (f.index -> f)
            case Space(times, index) =>
              acc

        }
      }
    }
    var i    = 0
    var sum2 = 0L
    var max  = devec.map(x => x.index).max + 10
    while (i < max) {
      val x = nmap.get(i)
      x.match
        case None => {
//          print(".")
          i = i + 1
        }
        case Some(value) => {
          for (j <- 0 until value.times) {
            sum2 = sum2 + value.id * i
            //            print(value.id)
            i = i + 1
          }
        }
    }
    println
    println(sum2)
    println("----")
    // println(sum)
    // val res: Vector[AFile] = compress2(decomp)
    // printA(res.toList)
    //  val sum = checksum(res)
    // println(sum)

    // 6211348208140
  }

  def treeToList(dg: TS[DominatGap]): Vector[Space] = {
    dg.foldLeft(Vector.empty[Space]) { (acc, ele) =>
      acc ++ (ele.gap +: treeToList(ele.dominated))
    }
  }

  def updateGap(req: Int, dg: TS[DominatGap]): Option[(TS[DominatGap], Space)] = {
    val fit = dg.minAfter(DominatGap(Space(req, 0), TS.empty))
    fit match
      case None =>
        None
      case Some(value) => {
        val newgapSize = value.gap.times - req
        val prev       = dg.maxBefore(value)
        prev match
          case None => {
            // we are the first in the tree
            if (newgapSize == 0) {
              Some((dg - value) ++ value.dominated, value.gap)
            } else {
              val newSpace = Space(newgapSize, value.gap.index + req)
              val vec      = newSpace +: treeToList(value.dominated)
              Some((dg - value) ++ gapList(vec), value.gap)
            }
          }
          case Some(prevvalue) => {
            val prevlist = prevvalue.gap +: treeToList(prevvalue.dominated)

            if (newgapSize == 0) {
              Some((dg - value) ++ gapList(prevlist ++ treeToList(value.dominated)), value.gap)
            } else {
              val newSpace = Space(newgapSize, value.gap.index + req)
              val vec      = (prevlist :+ newSpace) ++ treeToList(value.dominated)
              Some((dg - value) ++ gapList(vec), value.gap)
            }
          }
      }
  }

  def gapList(inlist: Vector[Space]): TS[DominatGap] = {

    if (inlist.isEmpty) {
      return TS.empty
    }

    var tree: TS[DominatGap]   = TS.empty[DominatGap]
    var biggestSofar           = 0
    var dominated: List[Space] = List.empty

    var cur: Option[DominatGap] = None
    for (i <- 0 until inlist.size) {
      val ele = inlist(i)
      if (ele.times > biggestSofar) { // commit last with list
        biggestSofar = ele.times
        cur match
          case None => {
            cur = Some(DominatGap(Space(ele.times, ele.index), TS.empty))
          }
          case Some(value) => {
            val dlist = gapList(dominated.reverse.toVector)
            // val dlist = TS.empty[DominatGap]
            val commit: DominatGap = value.copy(dominated = dlist)
            tree = tree + commit
            dominated = List.empty
            cur = Some(DominatGap(Space(ele.times, ele.index), TS.empty))
          }
      } else {
        dominated = ele +: dominated
      }
    }
    cur.map(value => {

      val dlist = gapList(dominated.reverse.toVector)
      // val dlist = TS.empty[DominatGap]
      val commit: DominatGap = value.copy(dominated = dlist)
      tree = tree + commit
    })
    tree

  }

  def printGapTree(gapTree: TS[DominatGap], ind: Int): Unit = {
    gapTree.foreach(ele => {
      for (i <- 0 until ind) {
        print(" ")
      }
      println(ele.gap.times)
      printGapTree(ele.dominated, ind + 1)
    })
  }

  def printA(l: List[BFile]): Unit = {
    for (i <- l) {
      i match
        case File(id, times, index) =>
          for (j <- 0 until times) {
            print(id)
          }
        case Space(times, index) =>
          for (j <- 0 until times) {
            print('.')
          }
    }
    println
    val pos = {
      l.foldLeft(Set.empty[Int]) { (acc, ele) =>
        acc + ele.index
      }
    }
    val max = l.map(_.index).max
    for (i <- 0 to max) {
      if (pos.contains(i)) {
        print('X')
      } else {
        print('.')
      }

    }
    println
  }

  def compress3(inputReversed: Vector[AFile], done: Vector[AFile]): Vector[AFile] = {
    val withDropped = inputReversed.dropWhile(p => p.id.isEmpty)
    val last        = withDropped.head
    val toFitSize   = last.times
    ???

  }

  def compress2(input: List[AFile]): Vector[AFile] = {
    val arr = input.toArray

    var j = arr.size - 1
    var i = 0

    while (j > 0) {
      val right = arr(j)
      if (right.id.isEmpty) {
        j = j - 1
      } else {
        i = 0
        var break = false
        while (i < j && !break) {
          val left = arr(i)
          if (left.id.isEmpty && left.times >= right.times) {
            arr(i) = right
            arr(j) = left
            break = true
          }
          i = i + 1
        }
        j = j - 1
      }
    }
    arr.toVector
  }

  def decompress3(input: String): List[BFile] = {
    var files = true
    var id    = 0
    var sb    = List.empty[BFile]
    var pos   = 0
    for (i <- 0 until input.length()) {
      val digit = input(i).asDigit
      if (files) {
        sb = File(id, digit, pos) +: sb
        id = id + 1
        files = false
        pos = pos + digit
      } else {
        sb = Space(digit, pos) +: sb
        files = true
        pos = pos + digit
      }
    }
    sb.reverse
  }
  def decompress2(input: String): List[AFile] = {
    var files = true
    var id    = 0
    var sb    = List.empty[AFile]
    for (i <- 0 until input.length()) {
      val digit = input(i).asDigit
      if (files) {
        sb = AFile(Some(id), digit) +: sb
        id = id + 1
        files = false
      } else {
        sb = AFile(None, digit) +: sb
        files = true
      }
    }
    sb.reverse
  }
  @main
  def day09_01(): Unit = {
    val decomp = decompress(input = input)
//    println(decomp)

    val res: List[Int] = compress(decomp)
    // println(res)
    val sum = checksum(res)
    println(sum)

    // 6211348208140
  }

  def decompress(input: String): List[Option[Int]] = {

    var files = true
    var id    = 0
    var sb    = List.empty[Option[Int]]
    for (i <- 0 until input.length()) {
      val digit = input(i).asDigit
      if (files) {
        for (block <- 0 until digit) {
          sb = Some(id) +: sb
        }
        id = id + 1
        files = false
      } else {
        for (block <- 0 until digit) {
          sb = None +: sb
        }
        files = true
      }
    }
    sb.reverse
  }

  def compress(input: List[Option[Int]]): List[Int] = {
    val arr: Array[Option[Int]] = input.toArray

    var i = 0
    var j = arr.size - 1

    while (i < j) {
      if (arr(j).isEmpty) {
        j = j - 1
      } else {
        if (arr(i).isDefined) {
          i = i + 1
        } else {
          val c = arr(i)
          arr(i) = arr(j)
          arr(j) = c
        }
      }
    }
    arr.flatten.toList
  }

  def checksum(input: List[Int]): Long = {
    input
      .zipWithIndex
      .foldLeft(0L) { (acc, ele) =>
        {
          val (idx, num) = ele
          acc + idx * num
        }
      }
  }
}

/*
0099811188827773336446555566..............
0099811188827773336446555566..............


022111222......
022111222......
 */
