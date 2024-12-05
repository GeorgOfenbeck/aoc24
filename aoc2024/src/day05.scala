import os._

object day05 {

  val filePath = os.resource / "day5.txt"

  val lines = os.read.lines(filePath)

  val rules   = lines.filter(line => line.contains("|"))
  val updates = lines.filter(line => line.contains(",")).map(x => x.split(",").map(y => y.toInt))

  val rulePairs = rules
    .map(x => x.split("\\|"))
    .map(x => (x(0).toInt, x(1).toInt))

  val backward = rulePairs.foldLeft(Map.empty[Int, Set[Int]]) { (acc, ele) =>
    val (leftnum, rightnum) = ele
    val newset              = acc.getOrElse(rightnum, Set.empty[Int]) + leftnum
    acc + (rightnum -> newset)
  }
  // RuleMap = this are the numbers that need to come before key

  val forward = rulePairs.foldLeft(Map.empty[Int, Set[Int]]) { (acc, ele) =>
    val (leftnum, rightnum) = ele
    val newset              = acc.getOrElse(rightnum, Set.empty[Int]) + leftnum
    acc + (rightnum -> newset)
  }

  val (correct, incorrect) = updates.partition(x => checkCorrect(Set.empty, backward, x.toList))

  @main
  def day05_01(): Unit = {
    val middle = correct.map(x => x.apply(x.length / 2))
    println(middle.sum)
  }

  def checkCorrect(notAllowed: Set[Int], rules: Map[Int, Set[Int]], toCheck: List[Int]): Boolean = {
    if (toCheck.isEmpty) return true
    val head = toCheck.head

    if (notAllowed.contains(head)) return false

    checkCorrect(notAllowed ++ rules.getOrElse(head, Set.empty), rules, toCheck.tail)
  }

  @main
  def day05_02(): Unit = {

    def correctOrder(in: Set[Int], backward: Map[Int, Set[Int]], done: List[Int]): List[Int] = {

      if (in.isEmpty) return done

      val newBackward = in.foldLeft(Map.empty[Int, Set[Int]]) { (acc, ele) =>
        val cur: Set[Int] = backward.getOrElse(ele, Set.empty[Int])
        // all current back edges
        val updated: Set[Int] = in.foldLeft(Set.empty[Int])((a, e) => if (cur.contains(e)) a + e else a)
        acc + (ele -> updated)
      }

      val noparent = newBackward.filter((key, value) => value.isEmpty)
      val picked   = noparent.keySet.head
      return correctOrder(in - picked, newBackward, picked +: done)
    }

    val corrected = incorrect.map(x => correctOrder(x.toSet, backward, List.empty))
    val middle =corrected.map(x => x(x.size/2))
    println(middle.sum)
}
}
