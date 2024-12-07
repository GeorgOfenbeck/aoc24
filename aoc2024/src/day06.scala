import os._
import scala.util.boundary, boundary.break

object day06 {

  enum Direction:
    case Up, Right, Down, Left

  val filePath = os.resource / "day6.txt"

  val matrix: Vector[Vector[Char]] = os.read
    .lines(filePath)
    .map { line =>
      if (!line.isEmpty()) {
        line.toVector
      } else Vector.empty[Char]
    }
    .toVector

  def findStartPos(matrix: Vector[Vector[Char]], toFind: Char): Option[(Int, Int)] =
    boundary[Option[(Int, Int)]] {
      for (x <- 0 until matrix.size)
        for (y <- 0 until matrix(x).size)
          if (matrix(x)(y) == toFind)
            break(Some(x, y))

      None
    }

  def move(
      matrix: Vector[Vector[Char]],
      curx: Int,
      cury: Int,
      direction: Direction,
      sofar: Set[(Int, Int)],
  ): Set[(Int, Int)] = {
    val (newx, newy) = direction match
      case Direction.Up          => (curx - 1, cury)
      case day06.Direction.Right => (curx, cury + 1)
      case Direction.Down        => (curx + 1, cury)
      case day06.Direction.Left  => (curx, cury - 1)

    if (newx < 0 || newx >= matrix.size)
      return sofar
    if (newy < 0 || newy >= matrix(newx).size)
      return sofar

    val nextpos = matrix(newx)(newy)
    if (nextpos == '#') {
      direction match
        case Direction.Up          => move(matrix, curx, cury, Direction.Right, sofar)
        case day06.Direction.Right => move(matrix, curx, cury, Direction.Down, sofar)
        case Direction.Down        => move(matrix, curx, cury, Direction.Left, sofar)
        case day06.Direction.Left  => move(matrix, curx, cury, Direction.Up, sofar)

    } else {
      val pair: (Int, Int) = (newx, newy)
      move(matrix, newx, newy, direction, sofar + pair)
    }

  }

  def update(matrix: Vector[Vector[Char]], visited: Set[(Int, Int)]): Vector[Vector[Char]] =
    for (x <- 0 until matrix.size) {
      for (y <- 0 until matrix(x).size)
        if (matrix(x)(y) == '^')
          print('^')
        else if (visited.contains((x, y)))
          print('X')
        else
          print(matrix(x)(y))
      println()
    }
    matrix

  @main
  def day06_01(): Unit = {
    val startpos = findStartPos(matrix, '^')
    startpos.map { (x, y) =>
      val set = move(matrix, x, y, Direction.Up, Set((x, y)))
      update(matrix, set)
      println(set.size)
    }
  }

  @main
  def day06_02(): Unit = {
    val startpos = findStartPos(matrix, '^')
    startpos.map { (x, y) =>
      val set = move(matrix, x, y, Direction.Up, Set((x, y)))
      update(matrix, set)
      println(set.size)

      var count = 0
      for (pos <- set)
        if (pos != (x, y)) {
          if (move2(matrix, x, y, pos._1, pos._2, Direction.Up, Set.empty))
            count = count + 1
        }

      println(count)

    }
  }

  def move2(
      matrix: Vector[Vector[Char]],
      curx: Int,
      cury: Int,
      changex: Int,
      changey: Int,
      direction: Direction,
      sofar: Set[(Direction, Int, Int)],
  ): Boolean = {
    val (newx, newy) = direction match
      case Direction.Up          => (curx - 1, cury)
      case day06.Direction.Right => (curx, cury + 1)
      case Direction.Down        => (curx + 1, cury)
      case day06.Direction.Left  => (curx, cury - 1)

    if (newx < 0 || newx >= matrix.size)
      return false
    if (newy < 0 || newy >= matrix(newx).size)
      return false

    val nextpos = matrix(newx)(newy)
    if (nextpos == '#' || (newx == changex && newy == changey)) {
      val hit = (direction, newx, newy)
      if (sofar.contains(hit))
        return true
      direction match
        case Direction.Up          => move2(matrix, curx, cury, changex, changey, Direction.Right, sofar + hit)
        case day06.Direction.Right => move2(matrix, curx, cury, changex, changey, Direction.Down, sofar + hit)
        case Direction.Down        => move2(matrix, curx, cury, changex, changey, Direction.Left, sofar + hit)
        case day06.Direction.Left  => move2(matrix, curx, cury, changex, changey, Direction.Up, sofar + hit)

    } else {
      move2(matrix, newx, newy, changex, changey, direction, sofar)
    }

  }

}
