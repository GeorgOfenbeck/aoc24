import os._
import scala.collection.{mutable => mu}

object day12 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day12_sample.txt"
    } else {
      os.resource / "day12.txt"
    }
  }

  val matrix: Vector[Vector[Char]] = os.read.lines(filePath).map(x => x.toVector).toVector

  case class Pos(x: Int, y: Int)
  case class Plot(x: Int, y: Int, char: Char)
  case class Cost(area: Int, fence: Int)
  case class Fences(up: Boolean, down: Boolean, left: Boolean, right: Boolean)

  @main
  def day12_02(): Unit = {
    val plotMap = scala.collection.mutable.Map.empty[Pos, Char]
    for (i <- 0 until matrix.size) {
      for (j <- 0 until matrix(i).size) {
        plotMap.addOne(Pos(i, j) -> matrix(i)(j))
      }
    }

    var totalCost = 0
    //println(matrix(0)) 
    /*
    val pos = Pos(0,0)
    plotMap.remove(pos)
    val cost = follow2(Set(pos), Map.empty, plotMap, matrix, Cost(0, 0))
    println(cost)
    */
    
    while (plotMap.size > 0) {
      val (pos, char) = plotMap.head
      plotMap.remove(pos)
      val cost = follow2(Set(pos), Map.empty, plotMap, matrix, Cost(0, 0))
      println(s"$char -> ${cost.area} * ${cost.fence} = ${cost.area * cost.fence}")
      totalCost = totalCost + cost.area * cost.fence
    }
    println(totalCost)
   
  }
  @main
  def day12_01(): Unit = {
    val plotMap = scala.collection.mutable.Map.empty[Pos, Char]
    for (i <- 0 until matrix.size) {
      for (j <- 0 until matrix(i).size) {
        plotMap.addOne(Pos(i, j) -> matrix(i)(j))
      }
    }

    var totalCost = 0
    while (plotMap.size > 0) {
      val (pos, char) = plotMap.head
      plotMap.remove(pos)
      val cost = follow(Set(pos), plotMap, matrix, Cost(0, 0))
      println(s"$char -> ${cost.area} * ${cost.fence} = ${cost.area * cost.fence}")
      totalCost = totalCost + cost.area * cost.fence
    }
    println(totalCost)

  }

  def isSame(char: Char, pos: Pos, matrix: Vector[Vector[Char]]): Boolean = {
    if (pos.x < 0 || pos.x >= matrix.size) {
      return false
    }
    if (pos.y < 0 || pos.y >= matrix(pos.x).size) {
      return false
    }

    return matrix(pos.x)(pos.y) == char
  }


  def followFence(fences: mu.Set[Pos], toVisit: Set[Pos], horizontal: Boolean): Unit = {
    if(toVisit.isEmpty) return

    val cur = toVisit.head
    var nvisit = Set.empty[Pos]
    if(horizontal){
        val left = Pos(cur.x, cur.y-1)
        val right = Pos(cur.x, cur.y+1)
        if(fences.contains(left)){
            fences.remove(left)
            nvisit = nvisit + left
        }
        if(fences.contains(right)){
            fences.remove(right)
            nvisit = nvisit + right 
        }
    } else {
        val left = Pos(cur.x-1, cur.y)
        val right = Pos(cur.x+1, cur.y)
        if(fences.contains(left)){
            fences.remove(left)
            nvisit = nvisit + left
        }
        if(fences.contains(right)){
            fences.remove(right)
            nvisit = nvisit + right 
        }
    }
    followFence(fences, toVisit.tail ++ nvisit, horizontal)
  }

  def countSides(fences: mu.Set[Pos], horizontal: Boolean): Long = {
    var count = 0
    while(fences.size > 0){
        val cur = fences.head
        fences.remove(cur)
        followFence(fences, Set(cur), horizontal)
        count = count + 1
    } 
    count
  }

  def fence2MutMap(fences: Map[Pos, Fences], f: Fences => Boolean): mu.Set[Pos] = {
    val fset = mu.Set.empty[Pos]
    fences.map(x => if(f(x._2)) fset.add(x._1)) 
    //println(fset)
    fset
  }


  def follow2(
      toVisit: Set[Pos],
      fences: Map[Pos, Fences],
      remain: mu.Map[Pos, Char],
      matrix: Vector[Vector[Char]],
      costSoFar: Cost,
  ): Cost = {
    if (toVisit.isEmpty) {
      val edgesup = countSides(fence2MutMap(fences, (f: Fences) => f.up), true)
      val edgesdown = countSides(fence2MutMap(fences, (f: Fences) => f.down), true)
      val edgesleft = countSides(fence2MutMap(fences, (f: Fences) => f.left), false)
      val edgesright = countSides(fence2MutMap(fences, (f: Fences) => f.right), false)
      //println(s"$edgesup $edgesdown $edgesleft $edgesright")
      return costSoFar.copy(fence = (edgesright+edgesleft+edgesdown+edgesup).toInt)
    }

    val curX = toVisit.head.x
    val curY = toVisit.head.y
    val cur  = matrix(curX)(curY)
    // println(s"$curX $curY $cur")
    val up    = Pos(curX - 1, curY)
    val down  = Pos(curX + 1, curY)
    val left  = Pos(curX, curY - 1)
    val right = Pos(curX, curY + 1)

    val directions = List(up, down, left, right)
    val localFences = Fences(
      up = !isSame(cur, up, matrix),
      down = !isSame(cur, down, matrix),
      left = !isSame(cur, left, matrix),
      right = !isSame(cur, right, matrix),
    )

    var lvisit = Set.empty[Pos]
    for (dir <- directions) {
      remain
        .get(dir)
        .map { c =>
          if (c == cur) {
            lvisit = lvisit + dir
            remain.remove(dir)
          }
        }
    }

    val newVisit = toVisit.tail ++ lvisit
    follow2(newVisit, fences + (toVisit.head -> localFences), remain, matrix, Cost(costSoFar.area + 1, costSoFar.fence))
  }

  def follow(toVisit: Set[Pos], remain: mu.Map[Pos, Char], matrix: Vector[Vector[Char]], costSoFar: Cost): Cost = {
    if (toVisit.isEmpty) {
      return costSoFar
    }

    val curX = toVisit.head.x
    val curY = toVisit.head.y
    val cur  = matrix(curX)(curY)
    // println(s"$curX $curY $cur")
    val up    = Pos(curX - 1, curY)
    val down  = Pos(curX + 1, curY)
    val left  = Pos(curX, curY - 1)
    val right = Pos(curX, curY + 1)

    val directions  = List(up, down, left, right)
    val fences: Int = directions.map(dir => isSame(cur, dir, matrix)).filter(x => x == false).size

    var lvisit = Set.empty[Pos]
    for (dir <- directions) {
      remain
        .get(dir)
        .map { c =>
          if (c == cur) {
            lvisit = lvisit + dir
            remain.remove(dir)
          }
        }
    }

    val newVisit = toVisit.tail ++ lvisit
    follow(newVisit, remain, matrix, Cost(costSoFar.area + 1, costSoFar.fence + fences))
  }
}
