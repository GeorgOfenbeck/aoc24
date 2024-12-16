import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import day12.Pos

object day15 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day15_sample.txt"
    } else {
      os.resource / "day15.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  sealed trait MapObject {
    def pos: Pos
  }

  case class Wall(val pos: Pos)     extends MapObject
  case class Box(val pos: Pos)      extends MapObject
  case class BoxLeft(val pos: Pos)  extends MapObject
  case class BoxRight(val pos: Pos) extends MapObject

  case class Pos(x: Int, y: Int)

  case class Robot(val pos: Pos, moves: Vector[Char]) extends MapObject

  @main
  def day15_02(): Unit = {
    def totalMapValue(matrix: Vector[Vector[Option[MapObject]]]): Int = {
      val a = matrix.map(_.toArray).toArray
      val res = matrix.map(rows => rows.map(e => gpsValue(e, a)).reduce(_ + _)).reduce(_ + _)
      res
    }

    def gpsValue(obo: Option[MapObject], matrix: Array[Array[Option[MapObject]]]): Int = {
      obo match {
        case None =>
          0
        case Some(value) =>
          value match {
            case b: BoxLeft => {
              val (left, right) = getFullBox(matrix,Left(b))
              /*val distleft = left.pos.x
              val distright = matrix(left.pos.y).size - right.pos.x
              val disttop = 
              0*/
              b.pos.x + b.pos.y * 100
            }
            case ob: Box => {
              ob.pos.x + ob.pos.y * 100
            }
            case _ =>
              0
          }
      }
    }

    def printMap(matrix: Vector[Vector[Option[MapObject]]]): Unit = {
      matrix.foreach { row =>
        row.foreach { ele =>
          ele match {
            case Some(value) => {
              value match {
                case Wall(_) =>
                  print("#")
                case Box(_) =>
                  print("O")
                case Robot(_, _) =>
                  print("@")
                case BoxLeft(pos) =>
                  print("[")
                case BoxRight(pos) =>
                  print("]")
              }
            }
            case None =>
              print(".")
          }
        }
        println
      }
    }

    def readInput(): (Vector[Vector[Option[MapObject]]], Robot) = {
      val (matrixPart, movesPart) = inputStrings.span(_.nonEmpty)
      val moves                   = movesPart.drop(1).mkString.trim.toVector

      var robot: Robot = null
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
                  case 'O' =>
                    Some(Box(Pos(x, y)))
                  case '@' => {
                    robot = Robot(Pos(x, y), moves)
                    Some(Robot(Pos(x, y), moves))
                  }
                  case '.' =>
                    None
                }
              }
              .toVector
          }
          .toVector
      }
      (matrix, robot)
    }

    def dP1(x: Pos): Pos = Pos(x.x * 2, x.y)
    def dP2(x: Pos): Pos = Pos(x.x * 2 + 1, x.y)
    def scaleInput(matrix: Vector[Vector[Option[MapObject]]]): Vector[Vector[Option[MapObject]]] = {
      matrix.map { row =>
        {
          val newrow = {
            row.foldLeft(List.empty[Option[MapObject]])((acc, ele) => {
              ele match
                case None =>
                  None :: None :: acc
                case Some(value) =>
                  value match
                    case Wall(pos) =>
                      Some(Wall(dP2(pos))) :: Some(Wall(dP1(pos))) :: acc
                    case Box(pos) =>
                      Some(BoxRight(dP2(pos))) :: Some(BoxLeft(dP1(pos))) :: acc

                    case BoxLeft(pos) => {
                      assert(false)
                      acc
                    }
                    case BoxRight(pos) => {

                      assert(false)
                      acc
                    }
                    case Robot(pos, moves) =>
                      None :: Some(Robot(dP1(pos), moves)) :: acc
            })
          }
          newrow.reverse.toVector
        }
      }
    }

    def char2Dir(c: Char): Pos => Pos = {

      val up: Pos => Pos    = (p: Pos) => Pos(p.x, p.y - 1)
      val down: Pos => Pos  = (p: Pos) => Pos(p.x, p.y + 1)
      val left: Pos => Pos  = (p: Pos) => Pos(p.x - 1, p.y)
      val right: Pos => Pos = (p: Pos) => Pos(p.x + 1, p.y)
      c match
        case '^' =>
          up
        case 'v' =>
          down
        case '<' =>
          left
        case '>' =>
          right
        case _ => {
          assert(false)
          ???
        }
    }

    def isMoveable(matrix: Array[Array[Option[MapObject]]], pos: Pos): Boolean = {
      matrix(pos.y)(pos.x) match
        case None =>
          false
        case Some(value) =>
          value match
            case Wall(pos) =>
              false
            case Box(pos) =>
              true
            case Robot(pos, moves) =>
              false
            case BoxLeft(pos) =>
              true
            case BoxRight(pos) =>
              true

    }

    def isV(dir: Pos => Pos): Boolean = {
      dir(Pos(1, 1)).x == 1
    }

    def getFullBox(matrix: Array[Array[Option[MapObject]]], box: Either[BoxLeft, BoxRight]): (BoxLeft, BoxRight) = {
      val fullbox: (BoxLeft, BoxRight) = {
        box match
          case Left(value) => {
            val op = matrix(value.pos.y)(value.pos.x + 1)
            val right: BoxRight = {
              op.get match
                case b: BoxRight =>
                  b
                case _ => {
                  assert(false)
                  BoxRight(Pos(0, 0))
                }
            }
            (value, right)
          }
          case Right(value) => {
            val op = matrix(value.pos.y)(value.pos.x - 1)
            val left: BoxLeft = {
              op.get match {
                case b: BoxLeft =>
                  b
                case _ => {
                  assert(false)
                  BoxLeft(Pos(0, 0))
                }
              }
            }
            (left, value)
          }
      }
      fullbox
    }

    def canbeMovedV(
        matrix: Array[Array[Option[MapObject]]],
        pos: Pos,
        dir: Pos => Pos,
        isVertical: Boolean,
    ): Boolean = {
      val cur = matrix(pos.y)(pos.x)
      cur match
        case None =>
          true
        case Some(value) =>
          value match
            case Wall(pos) =>
              false
            case Box(pos) =>
              canbeMovedV(matrix, dir(pos), dir, isVertical)
            case b: BoxLeft => {
              if (isVertical) {
                val (left, right) = getFullBox(matrix, Left(b))
                canbeMovedV(matrix, dir(left.pos), dir, isVertical) &&
                canbeMovedV(matrix, dir(right.pos), dir, isVertical)
              } else {
                canbeMovedV(matrix, dir(pos), dir, isVertical)
              }
            }
            case b: BoxRight => {
              if (isVertical) {
                val (left, right) = getFullBox(matrix, Right(b))
                canbeMovedV(matrix, dir(left.pos), dir, isVertical) &&
                canbeMovedV(matrix, dir(right.pos), dir, isVertical)
              } else {
                canbeMovedV(matrix, dir(pos), dir, isVertical)
              }
            }
            case Robot(pos, moves) =>
              false
    }

    def moveV(matrix: Array[Array[Option[MapObject]]], pos: Pos, dir: Pos => Pos, isVertical: Boolean): Unit = {
      val cur = matrix(pos.y)(pos.x)
      cur match
        case None =>
        case Some(value) =>
          value match
            case Wall(pos) =>
            case Box(pos) => {
              moveV(matrix, dir(pos), dir, isVertical)
              val npos = dir(pos)
              matrix(npos.y)(npos.x) = Some(Box(npos))
              matrix(pos.y)(pos.x) = None
            }
            case b: BoxLeft => {
              if (isVertical) {
                val (left, right) = getFullBox(matrix, Left(b))
                moveV(matrix, dir(left.pos), dir, isVertical)
                moveV(matrix, dir(right.pos), dir, isVertical)

                val nlpos = dir(left.pos)
                matrix(nlpos.y)(nlpos.x) = Some(BoxLeft(nlpos))
                val nrpos = dir(right.pos)
                matrix(nrpos.y)(nrpos.x) = Some(BoxRight(nrpos))
                matrix(left.pos.y)(left.pos.x) = None
                matrix(right.pos.y)(right.pos.x) = None
              } else {
                moveV(matrix, dir(pos), dir, isVertical)
                val npos = dir(pos)
                matrix(npos.y)(npos.x) = Some(BoxLeft(npos))
                matrix(pos.y)(pos.x) = None
              }
            }
            case b: BoxRight => {
              if (isVertical) {
                val (left, right) = getFullBox(matrix, Right(b))
                moveV(matrix, dir(left.pos), dir, isVertical)
                moveV(matrix, dir(right.pos), dir, isVertical)
                val nlpos = dir(left.pos)
                matrix(nlpos.y)(nlpos.x) = Some(BoxLeft(nlpos))
                val nrpos = dir(right.pos)
                matrix(nrpos.y)(nrpos.x) = Some(BoxRight(nrpos))
                matrix(left.pos.y)(left.pos.x) = None
                matrix(right.pos.y)(right.pos.x) = None
              } else {
                moveV(matrix, dir(pos), dir, isVertical)
                val npos = dir(pos)
                matrix(npos.y)(npos.x) = Some(BoxRight(npos))
                matrix(pos.y)(pos.x) = None
              }
            }
            case Robot(pos, moves) =>
    }

    def canbeMoved(matrix: Array[Array[Option[MapObject]]], pos: Pos, dir: Pos => Pos): Boolean = {
      val isVertical = isV(dir)
      canbeMovedV(matrix, pos, dir, isVertical)
    }

    def moveBoxes(matrix: Array[Array[Option[MapObject]]], pos: Pos, dir: Pos => Pos): Unit = {
      val isVertical = isV(dir)
      moveV(matrix, pos, dir, isVertical)
    }

    def moveRobot(robot: Robot, matrix: Array[Array[Option[MapObject]]]): (Robot, Array[Array[Option[MapObject]]]) = {
      if (robot.moves.isEmpty) {
        return (robot, matrix)
      }

      val nextmove = robot.moves.head

      val dir = char2Dir(nextmove)

      val nextpos = dir(robot.pos)

      if (matrix(nextpos.y)(nextpos.x).isEmpty) {
        val newrobot = Robot(dir(robot.pos), robot.moves.tail)
        matrix(nextpos.y)(nextpos.x) = Some(newrobot)
        matrix(robot.pos.y)(robot.pos.x) = None
        (newrobot, matrix)
      } else {
        val colobj = matrix(nextpos.y)(nextpos.x).get

        colobj match
          case Wall(pos) => {
            val newrobot = Robot(robot.pos, robot.moves.tail)
            matrix(robot.pos.y)(robot.pos.x) = Some(newrobot)
            (newrobot, matrix)
          }
          case b: BoxRight => {
            if (canbeMoved(matrix, b.pos, dir)) {
              moveBoxes(matrix, b.pos, dir)
              val newrobot = Robot(b.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = None
              matrix(b.pos.y)(b.pos.x) = Some(newrobot)
              (newrobot, matrix)
            } else {
              val newrobot = Robot(robot.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = Some(newrobot)
              (newrobot, matrix)
            }
          }
          case b: BoxLeft => {
            if (canbeMoved(matrix, b.pos, dir)) {
              moveBoxes(matrix, b.pos, dir)
              val newrobot = Robot(b.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = None
              matrix(b.pos.y)(b.pos.x) = Some(newrobot)
              (newrobot, matrix)
            } else {
              val newrobot = Robot(robot.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = Some(newrobot)
              (newrobot, matrix)
            }
          }
          case b: Box => {
            if (canbeMoved(matrix, b.pos, dir)) {
              moveBoxes(matrix, b.pos, dir)
              val newrobot = Robot(b.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = None
              matrix(b.pos.y)(b.pos.x) = Some(newrobot)
              (newrobot, matrix)
            } else {
              val newrobot = Robot(robot.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = Some(newrobot)
              (newrobot, matrix)
            }
          }
          case Robot(pos, moves) => {
            assert(false)
            (robot, matrix)
          }
      }

    }
    val (matrix, robot) = readInput()

    val scaled = scaleInput(matrix)

    var r = robot.copy(pos = dP1(robot.pos))
    printMap(scaled)

    val a = scaled.map(x => x.toArray).toArray

    while (!r.moves.isEmpty) {
      // printMap(a.map(x => x.toVector).toVector)
      // println(s"move: ${r.moves.head} ${r.moves.size} ${r.pos}")
      val (nr, na) = moveRobot(r, a)
      r = nr
    }

    val v = a.map(_.toVector).toVector
    if (printstuff) {
      printMap(v)
    }
    val res = totalMapValue(v)
    println(res)
  }
  @main
  def day15_01(): Unit = {
    def totalMapValue(matrix: Vector[Vector[Option[MapObject]]]): Int = {
      val res = matrix.map(rows => rows.map(e => gpsValue(e, matrix)).reduce(_ + _)).reduce(_ + _)
      res
    }

    def gpsValue(obo: Option[MapObject], matrix: Vector[Vector[Option[MapObject]]]): Int = {
      obo match {
        case None =>
          0
        case Some(value) =>
          value match {
            case b: BoxLeft => {
              b.pos.x + b.pos.y * 100
            }
            case _ =>
              0
          }
      }
    }

    def printMap(matrix: Iterable[Iterable[Option[MapObject]]]): Unit = {
      matrix.foreach { row =>
        row.foreach { ele =>
          ele match {
            case Some(value) => {
              value match {
                case Wall(_) =>
                  print("#")
                case Box(_) =>
                  print("O")
                case Robot(_, _) =>
                  print("@")
              }
            }
            case None =>
              print(".")
          }
        }
        println
      }
    }

    def readInput(): (Vector[Vector[Option[MapObject]]], Robot) = {
      val (matrixPart, movesPart) = inputStrings.span(_.nonEmpty)
      val moves                   = movesPart.drop(1).mkString.trim.toVector

      var robot: Robot = null
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
                  case 'O' =>
                    Some(Box(Pos(x, y)))
                  case '@' => {
                    robot = Robot(Pos(x, y), moves)
                    Some(Robot(Pos(x, y), moves))
                  }
                  case '.' =>
                    None
                }
              }
              .toVector
          }
          .toVector
      }
      (matrix, robot)
    }

    def char2Dir(c: Char): Pos => Pos = {

      val up: Pos => Pos    = (p: Pos) => Pos(p.x, p.y - 1)
      val down: Pos => Pos  = (p: Pos) => Pos(p.x, p.y + 1)
      val left: Pos => Pos  = (p: Pos) => Pos(p.x - 1, p.y)
      val right: Pos => Pos = (p: Pos) => Pos(p.x + 1, p.y)
      c match
        case '^' =>
          up
        case 'v' =>
          down
        case '<' =>
          left
        case '>' =>
          right
        case _ => {
          assert(false)
          ???
        }
    }

    def isMoveable(matrix: Array[Array[Option[MapObject]]], pos: Pos): Boolean = {
      matrix(pos.y)(pos.x) match
        case None =>
          false
        case Some(value) =>
          value match
            case Wall(pos) =>
              false
            case Box(pos) =>
              true
            case Robot(pos, moves) =>
              false

    }

    def canbeMoved(matrix: Array[Array[Option[MapObject]]], box: Box, dir: Pos => Pos): Boolean = {
      var newpos = dir(box.pos)
      while (isMoveable(matrix, newpos)) {
        newpos = dir(newpos)
      }
      return matrix(newpos.y)(newpos.x) == None
    }

    def moveBoxes(matrix: Array[Array[Option[MapObject]]], box: Box, dir: Pos => Pos): Unit = {
      var newpos = dir(box.pos)
      while (isMoveable(matrix, newpos)) {
        newpos = dir(newpos)
      }

      matrix(newpos.y)(newpos.x) = Some(Box(newpos))
    }

    def moveRobot(robot: Robot, matrix: Array[Array[Option[MapObject]]]): (Robot, Array[Array[Option[MapObject]]]) = {
      if (robot.moves.isEmpty) {
        return (robot, matrix)
      }

      val nextmove = robot.moves.head

      val dir = char2Dir(nextmove)

      val nextpos = dir(robot.pos)

      if (matrix(nextpos.y)(nextpos.x).isEmpty) {
        val newrobot = Robot(dir(robot.pos), robot.moves.tail)
        matrix(nextpos.y)(nextpos.x) = Some(newrobot)
        matrix(robot.pos.y)(robot.pos.x) = None
        (newrobot, matrix)
      } else {
        val colobj = matrix(nextpos.y)(nextpos.x).get

        colobj match
          case Wall(pos) => {
            val newrobot = Robot(robot.pos, robot.moves.tail)
            matrix(robot.pos.y)(robot.pos.x) = Some(newrobot)
            (newrobot, matrix)
          }
          case b: Box => {
            if (canbeMoved(matrix, b, dir)) {
              moveBoxes(matrix, b, dir)
              val newrobot = Robot(b.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = None
              matrix(b.pos.y)(b.pos.x) = Some(newrobot)
              (newrobot, matrix)
            } else {
              val newrobot = Robot(robot.pos, robot.moves.tail)
              matrix(robot.pos.y)(robot.pos.x) = Some(newrobot)
              (newrobot, matrix)
            }
          }
          case Robot(pos, moves) => {
            assert(false)
            (robot, matrix)
          }
      }

    }
    val (matrix, robot) = readInput()

    val a = matrix.map(x => x.toArray).toArray
    var r = robot

    while (!r.moves.isEmpty) {
      // printMap(a.map(x => x.toVector).toVector)
      // println(s"move: ${r.moves.head} ${r.moves.size} ${r.pos}")
      val (nr, na) = moveRobot(r, a)
      r = nr
    }

    val v = a.map(_.toVector).toVector
    if (printstuff) {
      printMap(v)
    }
    val res = totalMapValue(v)
    println(res)
  }
}
