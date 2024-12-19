import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import scala.annotation.tailrec

import mu.PriorityQueue

object day18 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day18_sample.txt"
    } else {
      os.resource / "day18.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  val nums = {
    inputStrings.filter(line => line.contains(",")).map(x => x.split(",").map(y => y.toInt)).map(_.toVector).toVector
  }

  sealed trait MapObject {
    def pos: Pos
  }
  case class Pos(x: Int, y: Int)

  case class Wall(val pos: Pos)  extends MapObject
  case class Start(val pos: Pos) extends MapObject

  case class End(val pos: Pos) extends MapObject

  def printMap(matrix: Iterable[Iterable[Option[MapObject]]]): Unit = {
    matrix.foreach { row =>
      row.foreach { ele =>
        ele match {
          case Some(value) => {
            value match {
              case Wall(_) =>
                print("#")
              case End(_) =>
                print("E")
              case Start(_) =>
                print("S")
            }
          }
          case None =>
            print(".")
        }
      }
      println
    }
  }

  def createMapAfterNFall(memsize: Int, falls: Int, falling: Vector[Vector[Int]]): Vector[Vector[Option[MapObject]]] = {
    val arr = Array.ofDim[Option[MapObject]](memsize + 2, memsize + 2)
    for (i <- 0 until arr.size) {
      for (j <- 0 until arr(i).size) {
        arr(i)(j) = None
      }
    }

    for (i <- 0 until arr.size) {
      arr(i)(0) = Some(Wall(Pos(0, i)))
      arr(0)(i) = Some(Wall(Pos(i, 0)))
      arr(arr.size - 1)(i) = Some(Wall(Pos(i, arr.size - 1)))
      arr(i)(arr(i).size - 1) = Some(Wall(Pos(arr(i).size, i)))
    }

    for (i <- 0 until falls) {
      val x = falling(i)(0)
      val y = falling(i)(1)
      arr(1 + y)(x + 1) = Some(Wall(Pos(x, y)))
    }
    arr(1)(1) = Some(Start(Pos(1, 1)))
    arr(memsize)(memsize) = Some(End(Pos(memsize, memsize)))
    arr.map(_.toVector).toVector
  }

  @inline
  def visitNeigh(
      cost: Int,
      pos: Pos,
      vertical: Boolean,
      matrix: Vector[Vector[Option[MapObject]]],
      visited: Map[Pos, List[Pos]],
      queue: PriorityQueue[(Int, (Pos, Boolean, Pos))],
  ): Unit = {

    val up    = pos.copy(y = pos.y - 1)
    val down  = pos.copy(y = pos.y + 1)
    val right = pos.copy(x = pos.x + 1)
    val left  = pos.copy(x = pos.x - 1)

    enq(up, matrix, queue, cost, pos)
    enq(down, matrix, queue, cost, pos)
    enq(right, matrix, queue, cost, pos)
    enq(left, matrix, queue, cost, pos)
  }

  @tailrec
  def rec(
      matrix: Vector[Vector[Option[MapObject]]],
      visited: Map[Pos, List[Pos]],
      costMap: Map[Pos, Int],
      queue: PriorityQueue[(Int, (Pos, Boolean, Pos))],
  ): Int = {
    if (queue.isEmpty) {
      return 0
    }
    val (cost, (pos, vertical, prevPos)) = queue.dequeue()
    val cur                              = matrix(pos.y)(pos.x)

    if (visited.contains(pos)) {
      val c = costMap(pos)
      if (c == cost) {
        val nvisited = visited + (pos -> (prevPos +: visited.getOrElse(pos, List.empty)))
        return rec(matrix, nvisited, costMap, queue)
      } else {
        return rec(matrix, visited, costMap, queue)
      }
    }
    cur match
      case Some(value) => {
        value match
          case Wall(pos) => {
            assert(false)
            0
          }
          case Start(pos) => {
            val curPos   = Pos(pos.x, pos.y)
            val nvisited = visited + (curPos -> (prevPos +: visited.getOrElse(curPos, List.empty)))
            val nCost    = costMap + (curPos -> cost)
            visitNeigh(cost, curPos, vertical, matrix, nvisited, queue)
            return rec(matrix, nvisited, nCost, queue)
          }
          case End(pos) => {
            cost
          }

      }
      case None => {
        val curPos   = Pos(pos.x, pos.y)
        val nvisited = visited + (curPos -> (prevPos +: visited.getOrElse(curPos, List.empty)))
        val nCost    = costMap + (curPos -> cost)
        visitNeigh(cost, pos, vertical, matrix, nvisited, queue)
        return rec(matrix, nvisited, nCost, queue)
      }
  }

  def count(cur: Pos, visited: Map[Pos, List[Pos]], countset: Set[Pos]): Set[Pos] = {
    val next = visited.get(cur)
    next match
      case None => {
        assert(false)
        Set.empty
      }
      case Some(value) => {
        if (value.head == cur) {
          countset
        } else {
          countset ++ value.map(prev => count(prev, visited, Set(Pos(cur.x, cur.y)))).reduce((a, b) => a ++ b)
        }
      }

  }

  @inline
  def enq(
      pos: Pos,
      matrix: Vector[Vector[Option[MapObject]]],
      queue: PriorityQueue[(Int, (Pos, Boolean, Pos))],
      curCost: Int,
      prevPos: Pos,
  ) = {
    val ele = matrix(pos.y)(pos.x)
    ele match
      case None =>
        queue.addOne(((curCost + 1), (pos, true, prevPos)))
      case Some(value) =>
        value match
          case Wall(pos)  =>
          case Start(pos) =>
          case e: End =>
            queue.addOne(((curCost + 1), (pos, true, prevPos)))
  }

  @main
  def day18_01(): Unit = {
    val afterN = {
      if (test) {
        createMapAfterNFall(7, 12, nums)
      } else {
        createMapAfterNFall(71, 1024, nums)
      }
    }
    printMap(afterN)

    implicit val ordering: Ordering[(Int, (Pos, Boolean, Pos))] = Ordering.by[(Int, (Pos, Boolean, Pos)), Int](-_._1)

    val minHeap = PriorityQueue.empty[(Int, (Pos, Boolean, Pos))]
    val estart  = Pos(1, 1)
    val res     = rec(afterN, Map.empty, Map.empty, minHeap.addOne(0, (estart, false, estart)))

  }

  @main
  def day18_02(): Unit = {
    var works = true
    for (i <- 1024 until nums.size) {
      val afterN = {
        if (test) {
          createMapAfterNFall(7, 12, nums)
        } else {
          createMapAfterNFall(71, i, nums)
        }
      }
      implicit val ordering: Ordering[(Int, (Pos, Boolean, Pos))] = Ordering.by[(Int, (Pos, Boolean, Pos)), Int](-_._1)

      val minHeap = PriorityQueue.empty[(Int, (Pos, Boolean, Pos))]
      val estart  = Pos(1, 1)
      val res     = rec(afterN, Map.empty, Map.empty, minHeap.addOne(0, (estart, false, estart)))
      if(works == true && res == 0){
        println(s" $i => ${nums(i)}")
        works = false
      }
    }
  }
}
