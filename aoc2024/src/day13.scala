import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex

object day13 {

    val test       = false
    val printstuff = test

    val filePath = {
      if (test) {
        os.resource / "day13_sample.txt"
      } else {
        os.resource / "day13.txt"
      }
    }

    case class Matrix(values: Vector[Vector[Long]])
    case class ColVector(col: Vector[Long])

    val inputStrings: Vector[String] = os.read.lines(filePath).toVector

    def parseInput(lines: Vector[String]): Option[(Matrix, ColVector)] = {
      val nonEmptyLines = lines.filter(_.nonEmpty)
      if (nonEmptyLines.length != 3) {
        println(s"Skipping invalid input chunk: $lines")
        return None
      }

      val pattern: Regex = """Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)""".r
      val input          = nonEmptyLines.mkString("\n")
      input match {
        case pattern(ax, ay, bx, by, px, py) =>
          //val matrix = Matrix(Vector(Vector(ax.toInt, ay.toInt), Vector(bx.toInt, by.toInt)))
          val matrix = Matrix(Vector(Vector(ax.toLong, bx.toLong), Vector(ay.toLong, by.toLong)))
          val vector = ColVector(Vector(px.toLong, py.toLong))
          Some((matrix, vector))
        case _ =>
          println(s"Input string does not match the expected format: $input")
          None
      }
    }

    val parsedData: Vector[(Matrix, ColVector)] = inputStrings.grouped(4).flatMap(parseInput).toVector


    def determinant(matrix: Matrix): Long = {
      val a = matrix.values(0)(0)
      val b = matrix.values(0)(1)
      val c = matrix.values(1)(0)
      val d = matrix.values(1)(1)
      a * d - b * c
    }

    def solveLinearSystem(matrix: Matrix, vector: ColVector): Option[Vector[Long]] = {
      val detA = determinant(matrix)
      if (detA == 0) {
        println("The matrix is singular and cannot be inverted.")
        return None
      }

      val a = matrix.values(0)(0)
      val b = matrix.values(0)(1)
      val c = matrix.values(1)(0)
      val d = matrix.values(1)(1)
      val e = vector.col(0)
      val f = vector.col(1)

      val detAx = e * d - b * f
      val detAy = a * f - e * c

      if(detAx%detA != 0 || detAy%detA != 0) return None

      val x = detAx / detA
      val y = detAy / detA

      Some(Vector(x, y))
    }

    @main
    def day13_01(): Unit = {
      var sum = 0L
      parsedData.foreach { case (matrix, vector) =>
        solveLinearSystem(matrix, vector) match {
          case Some(solution) => {
            val costX = solution(0) * 3
            val costY = solution(1)
            val total = costX + costY
            sum = sum + total
            if(printstuff)
             println(s"Solution: x = ${solution(0)}, y = ${solution(1)} total cost= $total")
          }
          case None => if(printstuff) println("No solution found.")
        }
      }
      println(s"Full Sum: $sum")
    }

    @main
    def day13_02(): Unit = {
      var sum = 0L
      parsedData.foreach { case (matrix, vector) =>
        val newvec = ColVector(vector.col.map(x => x + 10000000000000L ))
        solveLinearSystem(matrix, newvec) match {
          case Some(solution) => {
            val costX = solution(0) * 3
            val costY = solution(1)
            val total = costX + costY
            sum = sum + total
            if(printstuff)
             println(s"Solution: x = ${solution(0)}, y = ${solution(1)} total cost= $total")
          }
          case None => if(printstuff) println("No solution found.")
        }
      }
      println(s"Full Sum: $sum")
    }
}
