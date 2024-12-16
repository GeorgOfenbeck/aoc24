import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import mu.PriorityQueue
import scala.annotation.tailrec

object day16 {

  val test       = true
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day16_sample2.txt"
    } else {
      os.resource / "day16.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  sealed trait MapObject {
    def pos: Pos
  }
  case class Pos(x: Int, y: Int)

  case class Wall(val pos: Pos)  extends MapObject
  case class Start(val pos: Pos) extends MapObject

  case class End(val pos: Pos) extends MapObject

  def readInput(): (Start, Vector[Vector[Option[MapObject]]]) = {
    val matrixPart = inputStrings
    var start      = Start(Pos(0, 0))
    val matrix: Vector[Vector[Option[MapObject]]] = {
      matrixPart
        .zipWithIndex
        .map { case (line, y) =>
          line
            .zipWithIndex
            .map { case (char, x) =>
              char match {
                case '#' =>
                  Some(Wall(Pos(x, y)))
                case 'E' => {
                  Some(End(Pos(x, y)))
                }
                case 'S' => {
                  start = Start(Pos(x, y))
                  Some(Start(Pos(x, y)))
                }
                case '.' =>
                  None
              }
            }
            .toVector
        }
        .toVector
    }
    (start, matrix)
  }

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

  def visitNeigh(
      cost: Int,
      pos: Pos,
      vertical: Boolean,
      matrix: Vector[Vector[Option[MapObject]]],
      visited: Map[Pos, List[Pos]],
      queue: PriorityQueue[(Int, (Pos, Boolean, Pos))],
  ): Int = {

    val up    = pos.copy(y = pos.y - 1)
    val down  = pos.copy(y = pos.y + 1)
    val right = pos.copy(x = pos.x + 1)
    val left  = pos.copy(x = pos.x - 1)

    enq(up, matrix, queue, true, !vertical, cost, pos)
    enq(down, matrix, queue, true, !vertical, cost, pos)
    enq(right, matrix, queue, false, vertical, cost, pos)
    enq(left, matrix, queue, false, vertical, cost, pos)
    1
    // return rec(matrix, visited, queue)
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
      if(pos ==Pos(15,6))
       println("cross")
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
            val nvisited = visited + (pos -> (prevPos +: visited.getOrElse(pos, List.empty)))
            val nCost    = costMap + (pos -> cost)
            visitNeigh(cost, pos, vertical, matrix, nvisited, queue)
            return rec(matrix, nvisited, nCost, queue)
          }
          case End(pos) => {

            println("==========================")
            val x = matrix.map(_.toArray).toArray

            var prev: Pos = prevPos
            var foc: Pos  = visited.getOrElse(prev, List(prev)).head
            // println(visited)
            while (prev != foc) {
              x(foc.y)(foc.x) = Some(Start(pos))
              prev = foc
              foc = visited.getOrElse(prev, List(prev)).head
            }
            val v = x.map(_.toVector).toVector
            printMap(v)
            println(cost)
            println("==========================")
            cost
          }

      }
      case None => {

        val nvisited = visited + (pos -> (prevPos +: visited.getOrElse(pos, List.empty)))
        val nCost    = costMap + (pos -> cost)
        visitNeigh(cost, pos, vertical, matrix, nvisited, queue)
        return rec(matrix, nvisited, nCost, queue)
      }
  }

  @inline
  def enq(
      pos: Pos,
      matrix: Vector[Vector[Option[MapObject]]],
      queue: PriorityQueue[(Int, (Pos, Boolean, Pos))],
      isVertical: Boolean,
      isTurn: Boolean,
      curCost: Int,
      prevPos: Pos,
  ) = {
    val ele = matrix(pos.y)(pos.x)
    ele match
      case None =>
        if (isTurn) {
          queue.addOne(((curCost + 1000 + 1), (pos, isVertical, prevPos)))
        } else {
          queue.addOne(((curCost + 1), (pos, isVertical, prevPos)))
        }
      case Some(value) =>
        value match
          case Wall(pos)  =>
          case Start(pos) =>
          case End(pos) =>
            if (isTurn) {
              queue.addOne(((curCost + 1000 + 1), (pos, isVertical, prevPos)))
            } else {
              queue.addOne(((curCost + 1), (pos, isVertical, prevPos)))
            }

  }

  @main
  def day16_01(): Unit = {
    implicit val ordering: Ordering[(Int, (Pos, Boolean, Pos))] = Ordering.by[(Int, (Pos, Boolean, Pos)), Int](-_._1)
    val (start, matrix)                                         = readInput()
    val minHeap                                                 = PriorityQueue.empty[(Int, (Pos, Boolean, Pos))]
    printMap(matrix)
    val res = rec(matrix, Map.empty, Map.empty, minHeap.addOne(0, (start.pos, false, start.pos)))
    println(res)
  }

}
