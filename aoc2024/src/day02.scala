import os._

object day02 {

  @main
  def day02_02() = {
    val filePath = os.resource / "day2.txt"

    def string2Nums(s: String): List[Int] =
      s.split("\\s+").map(_.toInt).toList

    def grow(prev: Option[Int], numbers: List[Int], damp: Boolean, inc: Boolean): Boolean =
      prev match
        case None =>
          if (numbers.isEmpty) return true
          if (damp) {
            val rest = numbers.tail
            if (rest.tail.isEmpty) return true
            return grow(Some(rest.head), rest.tail, false, inc) || grow(Some(numbers.head), numbers.tail, true, inc)
          } else {
            return grow(Some(numbers.head), numbers.tail, damp, inc)
          }
        case Some(value) =>
          if (damp) {
            growWithPrev(value, numbers, damp, inc) || growWithPrev(value, numbers.tail, false, inc)
          } else {
            growWithPrev(value, numbers, damp, inc)
          }

    def growWithPrev(value: Int, numbers: List[Int], damp: Boolean, inc: Boolean): Boolean = {
      if (numbers.isEmpty) return true
      if (inc && value >= numbers.head) return false
      if (!inc && value <= numbers.head) return false

      val diff = Math.abs(numbers.head - value)
      if (diff > 0 && diff <= 3) {
        // keep growing
        return grow(Some(numbers.head), numbers.tail, damp, inc)
      } else {
        // dont grow
        return false
      }
    }
    val safety: IndexedSeq[Boolean] = os.read.lines(filePath).map { line =>
      val numbers = string2Nums(line)
      val lineres: Boolean = if (numbers.size > 1) {
        // isSafe(numbers(0), numbers.tail, true) || isSafe(numbers(0), numbers.tail, false)
        // check(numbers.head, numbers.tail, true) || check(numbers.head, numbers.tail, false)
        grow(None, numbers, true, true) || grow(None, numbers, true, false)
      } else {
        true
      }
      lineres
    }

    println(safety.count(_ == true))
  }

  @main
  def day02_01() = {
    val filePath = os.resource / "day2.txt"

    def string2Nums(s: String): List[Int] =
      s.split("\\s+").map(_.toInt).toList

    def isSafe(head: Int, numbers: List[Int], inc: Boolean): Boolean =
      if (numbers.isEmpty) {
        true
      } else {
        if (inc) {
          if (head < numbers.head) {
            if (numbers.head - head > 3) {
              false
            } else {
              isSafe(numbers.head, numbers.tail, inc)
            }
          } else {
            false
          }
        } else {
          if (head > numbers.head) {
            if (head - numbers.head > 3) {
              false
            } else {
              isSafe(numbers.head, numbers.tail, inc)
            }
          } else {
            false
          }
        }
      }

    val safety: IndexedSeq[Boolean] = os.read.lines(filePath).map { line =>
      val numbers = string2Nums(line)
      val lineres: Boolean = if (numbers.size > 1) {
        if (numbers(0) == numbers(1)) {
          false
        } else {
          if (numbers(0) < numbers(1)) {
            // increasing
            isSafe(numbers(0), numbers.tail, true)
          } else {
            // decreasing
            isSafe(numbers(0), numbers.tail, false)
          }
        }
      } else {
        true
      }
      lineres
    }

    println(safety.count(_ == true))
  }
}
