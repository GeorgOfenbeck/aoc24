import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import mu.PriorityQueue
import scala.annotation.tailrec

object day16 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day16_sample.txt"
    } else {
      os.resource / "day16.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  sealed trait MapObject {
    def pos: Pos
  }
  case class Pos(x: Int, y: Int)
  case class EPos(x: Int, y: Int, vertical: Boolean)

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
      pos: EPos,
      vertical: Boolean,
      matrix: Vector[Vector[Option[MapObject]]],
      visited: Map[EPos, List[EPos]],
      queue: PriorityQueue[(Int, (EPos, Boolean, EPos))],
  ): Int = {

    val up    = pos.copy(y = pos.y - 1, vertical = true)
    val down  = pos.copy(y = pos.y + 1, vertical = true)
    val right = pos.copy(x = pos.x + 1, vertical = false)
    val left  = pos.copy(x = pos.x - 1, vertical = false)

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
      visited: Map[EPos, List[EPos]],
      costMap: Map[EPos, Int],
      queue: PriorityQueue[(Int, (EPos, Boolean, EPos))],
  ): Int = {
    if (queue.isEmpty) {
      return 0
    }
    val (cost, (pos, vertical, prevPos)) = queue.dequeue()
    val cur                              = matrix(pos.y)(pos.x)

    if (pos == EPos(15, 6, true)) {
      // println("cross")
    }

    if (visited.contains(pos)) {
      val c = costMap(pos)
      if (c == cost) {
        val nvisited = visited + (pos -> (prevPos +: visited.getOrElse(pos, List.empty)))
        return rec(matrix, nvisited, costMap, queue)
      } else {
        return rec(matrix, visited, costMap, queue)
      }
    }
    /*
    println("==========================")
    val x = matrix.map(_.toArray).toArray
    x(pos.y)(pos.x) = Some(Start(Pos(pos.y, pos.x)))
    val v = x.map(_.toVector).toVector
    printMap(v)
    println(cost)
    println("==========================")

     */
    cur match
      case Some(value) => {
        value match
          case Wall(pos) => {
            assert(false)
            0
          }
          case Start(pos) => {
            val curPos   = EPos(pos.x, pos.y, vertical)
            val nvisited = visited + (curPos -> (prevPos +: visited.getOrElse(curPos, List.empty)))
            val nCost    = costMap + (curPos -> cost)
            visitNeigh(cost, curPos, vertical, matrix, nvisited, queue)
            return rec(matrix, nvisited, nCost, queue)
          }
          case End(pos) => {

            println("==========================")
            val x = matrix.map(_.toArray).toArray

            var prev: EPos = prevPos
            var foc: EPos  = visited.getOrElse(prev, List(prev)).head
            // println(visited)
            while (prev != foc) {
              x(foc.y)(foc.x) = Some(Start(pos))
              prev = foc
              val ll = visited.getOrElse(prev, List(prev))
              if (ll.size > 1) {
                x(foc.y)(foc.x) = Some(End(pos))
              }
              foc = ll.head
            }
            val v = x.map(_.toVector).toVector
            printMap(v)
            println(cost)
            val cset = count(prevPos,visited,Set.empty)
            println(cset.size+2)
            println("==========================")
            cost
          }

      }
      case None => {

        val curPos   = EPos(pos.x, pos.y, vertical)
        val nvisited = visited + (curPos -> (prevPos +: visited.getOrElse(curPos, List.empty)))
        val nCost    = costMap + (curPos -> cost)
        visitNeigh(cost, pos, vertical, matrix, nvisited, queue)
        return rec(matrix, nvisited, nCost, queue)
      }
  }

  def count(cur: EPos, 
      visited: Map[EPos, List[EPos]],
      countset: Set[Pos]
  ): Set[Pos] = {
    val next = visited.get(cur)
    next match
        case None => {
            assert(false)
            Set.empty 
        }
        case Some(value) =>{
            if(value.head == cur)
                countset
            else{
               countset ++ value.map( prev => count(prev, visited,  Set(Pos(cur.x, cur.y)))).reduce( (a,b)=> a ++ b) 
            }
        }
     
  }

  @inline
  def enq(
      pos: EPos,
      matrix: Vector[Vector[Option[MapObject]]],
      queue: PriorityQueue[(Int, (EPos, Boolean, EPos))],
      isVertical: Boolean,
      isTurn: Boolean,
      curCost: Int,
      prevPos: EPos,
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
          case e: End =>
            if (isTurn) {
              queue.addOne(((curCost + 1000 + 1), (pos, isVertical, prevPos)))
            } else {
              queue.addOne(((curCost + 1), (pos, isVertical, prevPos)))
            }

  }

  @main
  def day16_01(): Unit = {
    implicit val ordering: Ordering[(Int, (EPos, Boolean, EPos))] = Ordering
      .by[(Int, (EPos, Boolean, EPos)), Int](-_._1)
    val (start, matrix) = readInput()
    val minHeap         = PriorityQueue.empty[(Int, (EPos, Boolean, EPos))]
    printMap(matrix)
    val estart = EPos(start.pos.x, start.pos.y, false)
    val res    = rec(matrix, Map.empty, Map.empty, minHeap.addOne(0, (estart, false, estart)))
    println(res)
  }

}
