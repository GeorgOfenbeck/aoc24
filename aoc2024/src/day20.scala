import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import scala.annotation.tailrec

import mu.PriorityQueue
import mu.ArraySeq
import scala.annotation.targetName

object day20 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day20_sample.txt"
    } else {
      os.resource / "day20.txt"
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

  case class Path(val pos: Pos, value: Int) extends MapObject

  def printMap2(matrix: mu.ArraySeq[mu.ArraySeq[MapObject]]): Unit = {
    matrix.foreach { row =>
      row.foreach { ele =>
        ele match {
          case Wall(_) =>
            print("##")
          case End(_) =>
            print("EE")
          case Start(_) =>
            print("SS")
          case Path(pos, value) =>
            print(f"$value%02d")
        }
      }
      println
    }
  }

  def printMap(matrix: Seq[Seq[Option[MapObject]]]): Unit = {
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
              case Path(pos, value) =>
                print(value)
            }

          }
          case None =>
            print(".")
        }
      }
      println
    }
  }

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

  def getNext(matrix: Vector[Vector[Option[MapObject]]], up: Pos, cur: Pos, visited: mu.Set[Pos]): Option[Pos] = {
    if (!visited.contains(up)) {
      val cur = matrix(up.y)(up.x)
      cur match
        case None => {
          visited.add(up)
          Some(up)
        }
        case Some(value) =>
          value match
            case End(pos) => {
              visited.add(up)
              Some(up)
            }
            case _ =>
              None
    } else {
      None
    }
  }

  def isEnd(ob: Option[MapObject]): Boolean = {
    ob match
      case None =>
        false
      case Some(value) =>
        value match
          case End(pos) =>
            true
          case _ =>
            false
  }

  def insertPath(start: Start, matrix: Vector[Vector[Option[MapObject]]]): ArraySeq[ArraySeq[MapObject]] = {
    // i want to convert the matrix to to the result type which is a mutable version - in  addition the option is removed -
    // whenever a element is None is replaced with a "Path" MapObject
    val mutableMatrix = ArraySeq.fill[MapObject](matrix.length, matrix.head.length)(Path(Pos(0, 0), 0))
    for (
      y <- matrix.indices;
      x <- matrix(y).indices
    ) {
      matrix(y)(x) match {
        case obj: Some[MapObject] =>
          mutableMatrix(y)(x) = obj.value
        case None =>
          mutableMatrix(y)(x) = Path(Pos(x, y), 0)
      }
    }
    var visited             = mu.Set(start.pos)
    var curpos: Option[Pos] = Some(start.pos)
    mutableMatrix(start.pos.y)(start.pos.x) = Path(start.pos, 0)

    while (curpos.map(pos => !isEnd(matrix(pos.y)(pos.x))).getOrElse(false)) {
      curpos.map { pos =>
        {
          val up    = Pos(pos.x, pos.y - 1)
          val down  = Pos(pos.x, pos.y + 1)
          val left  = Pos(pos.x - 1, pos.y)
          val right = Pos(pos.x + 1, pos.y)
          val nextOption = getNext(matrix, up, pos, visited)
            .orElse(getNext(matrix, down, pos, visited))
            .orElse(getNext(matrix, left, pos, visited))
            .orElse(getNext(matrix, right, pos, visited))
          nextOption.map { next =>
            {
              val prev = mutableMatrix(pos.y)(pos.x)
              prev match
                case Path(pathpos, prevvalue) =>
                  mutableMatrix(next.y)(next.x) = Path(next, prevvalue + 1)
                case _ =>
            }
          }
          curpos = nextOption
        }
      }
    }
    mutableMatrix
  }

  def calcSaving(origin: Path, target: Path, length: Int): Int = {
    target.value - origin.value - length
  }

  def runStencil(
      stencil: Vector[Pos],
      length: Int,
      path: Path,
      rec: mu.Map[Int, Int],
      pathSegments: Map[Pos, Path],
  ): Unit = {

    for (focus <- stencil) {
      pathSegments
        .get(focus)
        .map { target =>
          {
            if (target.value > path.value + 2) {
              val saving = calcSaving(path, target, length)
              rec.addOne((saving, rec.getOrElse(saving, 0) + 1))
            }
          }
        }
    }
  }

  def createStencil(path: Path, length: Int): Vector[Pos] = {
    var l = List.empty[Pos]
    for (i <- 0 to length) {
      for (j <- 0 to length) {
        if (i + j == length) {
          l = {
            Pos(path.pos.x + i, path.pos.y + j) +: Pos(path.pos.x - i, path.pos.y + j) +:
              Pos(path.pos.x + i, path.pos.y - j) +: Pos(path.pos.x - i, path.pos.y - j) +: l
          }

        }
      }
    }
    l.toSet.toVector
  }

  def checkCheat(path: Path, rec: mu.Map[Int, Int], pathSegments: Map[Pos, Path], maxCheat: Int): Unit = {
    for (i <- 1 to maxCheat) {
      val stencil = createStencil(path, i)
      runStencil(stencil, i, path, rec, pathSegments)
    }
  }

  def createMap(matrix: ArraySeq[ArraySeq[MapObject]]): Map[Pos, Path] = {
    var rmap = Map.empty[Pos, Path]
    for (
      y <- matrix.indices;
      x <- matrix(y).indices
    ) {
      matrix(y)(x) match
        case p: Path =>
          rmap = rmap + (p.pos -> p)
        case _ =>
    }
    rmap
  }

  def createCheatMap(matrix: ArraySeq[ArraySeq[MapObject]], maxCheat: Int): mu.Map[Int, Int] = {
    val rmap = createMap(matrix)
    val rec  = mu.Map.empty[Int, Int]
    for ((pos, path) <- rmap) {
      checkCheat(path, rec, rmap, maxCheat)
    }
    rec
  }

  def larger100(cheatMap: mu.Map[Int, Int], gt: Int): Long = {
    val sum = {
      cheatMap
        .map((saved, times) => {
          if (saved >= gt) {
            times.toLong
          } else {
            0L
          }
        })
        .toVector
        .sum
    }
    sum
  }
  @main
  def day20_01(): Unit = {

    val (start, matrix) = readInput()
    // printMap(matrix)
    val withPath = insertPath(start, matrix)
    // printMap2(withPath)

    val cheatMap = createCheatMap(withPath, 2)
    // val printit = cheatMap.map((k,v)=> Vector(k,v)).toVector.sortBy(x=> x(0))
    println(larger100(cheatMap, 100))
  }
  @main
  def day20_02(): Unit = {

    val (start, matrix) = readInput()
    // printMap(matrix)
    val withPath = insertPath(start, matrix)
    // printMap2(withPath)

    val cheatMap = createCheatMap(withPath, 20)

    
     //val printit = cheatMap.map((k,v)=> Vector(k,v)).toVector.sortBy(x=> x(0)).filter(x => x(0) >= 50)
     //println(printit)
    // val printit = cheatMap.map((k,v)=> Vector(k,v)).toVector.sortBy(x=> x(0))
    println(larger100(cheatMap,100))
  }

}
