import os._
object day04 {

  enum Direction:
    case Right, Left, Up, Down, UpRight, DownRight, DownLeft, UpLeft

  @main
  def day04_01(): Unit = {
    val filePath = os.resource / "day4.txt"

    val matrix: Vector[Vector[Char]] = os.read
      .lines(filePath)
      .map { line =>
        if (!line.isEmpty()) {
          line.toVector
        } else Vector.empty[Char]
      }
      .toVector

    val toCheck = String("XMAS").toList
    var sum     = 0L
    for (x <- 0 until matrix.size)
      for (y <- 0 until matrix(x).size)
        for (d <- Direction.values)
          if (check(matrix, x, y, toCheck, d))
            sum = sum + 1

    println(sum)
  }

  def check(matrix: Vector[Vector[Char]], posx: Int, posy: Int, toCheck: List[Char], direction: Direction): Boolean = {
    if (toCheck.isEmpty) return true

    if (posx >= matrix.size || posx < 0)
      return false
    if (posy >= matrix(posx).size || posy < 0)
      return false

    if (matrix(posx)(posy) != toCheck.head) {
      return false
    }

    direction match
      case day04.Direction.Right => check(matrix, posx + 1, posy, toCheck.tail, direction)
      case day04.Direction.Left  => check(matrix, posx - 1, posy, toCheck.tail, direction)
      case Direction.Up          => check(matrix, posx, posy - 1, toCheck.tail, direction)
      case Direction.Down        => check(matrix, posx, posy + 1, toCheck.tail, direction)
      case Direction.UpRight     => check(matrix, posx + 1, posy - 1, toCheck.tail, direction)
      case Direction.DownRight   => check(matrix, posx + 1, posy + 1, toCheck.tail, direction)
      case Direction.DownLeft    => check(matrix, posx - 1, posy + 1, toCheck.tail, direction)
      case Direction.UpLeft      => check(matrix, posx - 1, posy - 1, toCheck.tail, direction)
  }

  @main
  def day04_02(): Unit = {
    val filePath = os.resource / "day4.txt"

    val matrix: Vector[Vector[Char]] = os.read
      .lines(filePath)
      .map { line =>
        if (!line.isEmpty()) {
          line.toVector
        } else Vector.empty[Char]
      }
      .toVector

    var sum = 0L
    for (x <- 0 until matrix.size)
      for (y <- 0 until matrix(x).size)
        if (checkX(matrix, x, y))
          sum = sum + 1

    println(sum)
  }

  def checkX(matrix: Vector[Vector[Char]], x: Int, y: Int): Boolean = {
    if (x < 1 || x + 1 >= matrix.size)
      return false
    if (y < 1 || y + 1 >= matrix(x).size)
      return false

    if (matrix(x)(y) != 'A')
      return false

    val diag1 = Set(matrix(x - 1)(y - 1), matrix(x + 1)(y + 1))
    val diag2 = Set(matrix(x + 1)(y - 1), matrix(x - 1)(y + 1))

    if (!(diag1.contains('M') && diag1.contains('S'))) return false
    if (!(diag2.contains('M') && diag2.contains('S'))) return false
    return true
  }
}
