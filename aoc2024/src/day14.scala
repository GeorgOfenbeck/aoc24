import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import day08.posMap

object day14 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day14_sample.txt"
    } else {
      os.resource / "day14.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  case class Robot(pos: Pos, v: Pos)

  case class Pos(x: Int, y: Int)

  def parseLine(line: String): Robot = {
    val pattern = """p=(\d+),(\d+) v=(-?\d+),(-?\d+)""".r
    line match {
      case pattern(px, py, vx, vy) =>
        Robot(Pos(px.toInt, py.toInt), Pos(vx.toInt, vy.toInt))
      case _ =>
        throw new IllegalArgumentException(s"Invalid line format: $line")
    }
  }

  val robots: Vector[Robot] = inputStrings.map(parseLine)

  def countQuadrants(robots: Vector[Robot], maxP: Pos): Int = {
    val posMap = robots2PosMap(robots)
    val halfx  = maxP.x / 2
    val halfy  = maxP.y / 2
    val a      = countRobots(robots, posMap, Pos(0, 0), Pos(halfx, halfy))
    val b      = countRobots(robots, posMap, Pos(maxP.x - halfx, 0), Pos(maxP.x, halfy))
    val c      = countRobots(robots, posMap, Pos(0, maxP.y - halfy), Pos(maxP.x / 2, maxP.y))
    val d      = countRobots(robots, posMap, Pos(maxP.x - halfx, maxP.y - halfy), maxP)
    val mul    = a * b * c * d
    if (printstuff) {
      println(s"a $a b $b c $c d $d mul ${mul}")
    }
    mul

  }

  def countRobots(robots: Vector[Robot], posMap: Map[Pos, Int], from: Pos, to: Pos): Int = {
    var sum = 0
    for (row <- from.y until to.y) {
      for (cols <- from.x until to.x) {
        posMap.get(Pos(cols, row)) match
          case None =>
          case Some(value) =>
            sum = sum + value
      }
    }
    sum
  }

  def robots2PosMap(robots: Vector[Robot]): Map[Pos, Int] = {
    val posMap = {
      robots.foldLeft(Map.empty[Pos, Int])((acc, ele) => {
        acc.updatedWith(ele.pos)(f => {
          f match
            case None =>
              Some(1)
            case Some(value) =>
              Some(value + 1)
        })
      })
    }

    posMap
  }

  def printRobots(robots: Vector[Robot], maxX: Int, maxY: Int): Unit = {
    val posMap = robots2PosMap(robots)
    for (row <- 0 until maxY) {
      for (cols <- 0 until maxX) {
        posMap.get(Pos(cols, row)) match
          case None =>
            print(".")
          case Some(value) =>
            print(s"$value")
      }
      println()
    }
  }

  def moveRobot(seconds: Int, robot: Robot, maxX: Int, maxY: Int): Robot = {
    val outx = robot.pos.x + seconds * robot.v.x
    val outy = robot.pos.y + seconds * robot.v.y

    val modx = outx % maxX
    val mody = outy % maxY

    val newx = {
      if (modx < 0) {
        maxX + modx
      } else {
        modx
      }
    }
    val newy = {
      if (mody < 0) {
        maxY + mody
      } else {
        mody
      }
    }
    Robot(Pos(newx, newy), robot.v)
  }

  @main
  def day14_01(): Unit = {
    if (printstuff) {
      // printRobots(robots,11,7)
      // val r = Robot(Pos(2, 4), Pos(2, -3))

      /*
      for (i <- 0 until 100) {
        val newr = robots.map( r => moveRobot(i, r, 11, 7))
        if( i == 99)
        printRobots(newr, 11, 7)
        println
      }
       */
      val maxP  = Pos(11, 7)
      val moved = robots.map(r => moveRobot(100, r, maxP.x, maxP.y))
      printRobots(moved, maxP.x, maxP.y)

      val posMap = robots2PosMap(moved)
      val count  = countQuadrants(moved, maxP)
      println(count)
    } else {

      val maxP  = Pos(101, 103)
      val moved = robots.map(r => moveRobot(100, r, maxP.x, maxP.y))
      // printRobots(moved, maxP.x, maxP.y)

      val posMap = robots2PosMap(moved)
      val count  = countQuadrants(moved, maxP)
      println(count)
    }

  }

  @main
  def day14_02(): Unit = {
    val maxP = Pos(101, 103)
    for (i <- 0 until 10000) {
      val moved = robots.map(r => moveRobot(i, r, maxP.x, maxP.y))
      val posMap = robots2PosMap(moved) 
      if(posMap.values.filter(p => p > 1).isEmpty){
        println(s"iteration $i")
        printRobots(moved, maxP.x, maxP.y)
      }
    }
  }
}
