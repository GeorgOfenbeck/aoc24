import os._

object day10 {

  val test       = true 
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day10_sample.txt"
    } else {
      os.resource / "day10.txt"
    }
  }

  val matrix: Vector[Vector[Int]] = os.read.lines(filePath).map(x => x.toVector.map(_.asDigit)).toVector

  @main
  def day10_02(): Unit = {

    var sum = 0L
    for(i <- 0 until matrix.size ){
        for(j <- 0 until matrix(i).size){
            val set = findposU(matrix, i, j, 0)
            sum = sum + set.size
        }
    }   
    println(sum)
  }
  @main
  def day10_01(): Unit = {

    var sum = 0L
    for(i <- 0 until matrix.size ){
        for(j <- 0 until matrix(i).size){
            val set = findpos(matrix, i, j, 0)
            sum = sum + set.size
        }
    }   
    println(sum)
  }

  

  def findposU(matrix: Vector[Vector[Int]], x: Int, y: Int, cur: Int): List[(Int, Int)] = {
    if (x < 0 || x >= matrix.size) {
      return List.empty
    }
    if (y < 0 || y >= matrix(x).size) {
      return List.empty
    }

    val ele = matrix(x)(y)
    if (ele == cur) {
      if (ele == 9) {
        List((x, y))
      } else {
        val next = cur + 1
        val a = findposU(matrix, x - 1, y, next)
        val b = findposU(matrix, x + 1, y, next)
        val c = findposU(matrix, x, y + 1, next) 
        val d = findposU(matrix, x, y - 1, next)
        a.concat(b).concat(c).concat(d) 
      }
    } else {
      List.empty
    }
  }

  def findpos(matrix: Vector[Vector[Int]], x: Int, y: Int, cur: Int): Set[(Int, Int)] = {
    if (x < 0 || x >= matrix.size) {
      return Set.empty
    }
    if (y < 0 || y >= matrix(x).size) {
      return Set.empty
    }

    val ele = matrix(x)(y)
    if (ele == cur) {
      if (ele == 9) {
        Set((x, y))
      } else {
        val next = cur + 1
        findpos(matrix, x - 1, y, next) ++ findpos(matrix, x + 1, y, next) ++ findpos(matrix, x, y + 1, next) ++
          findpos(matrix, x, y - 1, next)

      }
    } else {
      Set.empty
    }
  }
}
