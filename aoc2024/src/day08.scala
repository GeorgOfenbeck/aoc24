import os._
import scala.util.boundary, boundary.break

object day08 {

  val filePath = os.resource / "day8.txt"

  val matrix = os.read
    .lines(filePath)
    .map { line =>
      line.toVector
    }
    .toVector

  var posMap = Map.empty[Char, Set[(Int, Int)]]

  for (x <- 0 until matrix.size)
    for (y <- 0 until matrix(x).size)
      posMap = add2Map(matrix(x)(y), x, y, posMap)

  @main
  def day08_01(): Unit = {

    val uniqueAnti = scala.collection.mutable.Set.empty[(Int, Int)]
    for (posSet <- posMap.values) {
      val posVec = posSet.toVector
      for (i <- 0 until posVec.size)
        for (j <- 0 until posVec.size)
          if (i != j && i < j) {
            val (ax, ay) = posVec(i)
            val (bx, by) = posVec(j)
            val diffx    = ax - bx
            val diffy    = ay - by

            val (pos1x, pos1y) = (ax + diffx, ay + diffy)
            val (pos2x, pos2y) = (bx - diffx, by - diffy)

            if (isInside(pos1x, pos1y, matrix))
              uniqueAnti.add((pos1x, pos1y))
            if (isInside(pos2x, pos2y, matrix))
              uniqueAnti.add((pos2x, pos2y))
          }
    }

    println(uniqueAnti.size)
  }
  
  @main
  def day08_02(): Unit = {

    val uniqueAnti = scala.collection.mutable.Set.empty[(Int, Int)]
    for (posSet <- posMap.values) {
      val posVec = posSet.toVector
      for (i <- 0 until posVec.size)
        for (j <- 0 until posVec.size)
          if (i != j && i < j) {
            val (ax, ay) = posVec(i)
            val (bx, by) = posVec(j)
            val diffx    = ax - bx
            val diffy    = ay - by

            addRec(ax, ay, diffx, diffy, matrix, uniqueAnti)
            addRec(bx, by, -diffx, -diffy, matrix, uniqueAnti)
          }
    }

    println(uniqueAnti.size)
  }

  def addRec(curx: Int, cury: Int, diffx: Int, diffy: Int, matrix: Vector[Vector[Char]], set: scala.collection.mutable.Set[(Int, Int)]): Unit = {
    if(!isInside(curx, cury, matrix)) return
    else{
        set.add((curx,cury))
        val (newx, newy) = (curx + diffx, cury + diffy)
        addRec(newx, newy, diffx, diffy, matrix, set)
    }
  }


  def isInside(x: Int, y: Int, matrix: Vector[Vector[Char]]): Boolean = {
    if (x < 0 || x >= matrix.size) return false
    if (y < 0 || y >= matrix(x).size) return false
    return true
  }

  def add2Map(c: Char, x: Int, y: Int, sofar: Map[Char, Set[(Int, Int)]]): Map[Char, Set[(Int, Int)]] =
    if (c == '.')
      sofar
    else {
      val pair: (Int, Int) = (x, y)
      sofar + (c -> (sofar.getOrElse(c, Set.empty) + pair))
    }
}
