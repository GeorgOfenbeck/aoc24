import os._
import scala.util.matching.Regex
import scala.collection.mutable.Queue

object day03 {

  @main
  def day03_01() = {
    val filePath = os.resource / "day3.txt"

    val fileAsString = os.read(filePath)

    val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val matches: Iterator[Regex.Match] = pattern.findAllMatchIn(fileAsString)

    var sum = 0L
    matches.foreach { m =>
      val x = m.group(1).toInt
      val y = m.group(2).toInt
      println(s"$x $y")
      sum = sum + x * y
    }
    println(sum)
  }

  @main
  def day03_02() = {

    def preprocess(full: String): String = {
      var s = full

      val sb = StringBuilder()

      val offswitch = "don't()"
      val onswitch  = "do()"
      val firstoff  = s.indexOf(offswitch)

      if (firstoff == -1) // we dont have any off
        return full
      else {
        sb.append(full.substring(0, firstoff))
        var on      = false
        var lastidx = firstoff

        while (lastidx != -1)
          if (on) {
            val start = lastidx
            val end   = s.indexOf(offswitch, lastidx)
            if (end == -1)
              sb.append(s.substring(start, s.size))
            else
              sb.append(s.substring(start, end))
            lastidx = end
            on =  false
          } else {
            lastidx = s.indexOf(onswitch, lastidx)
            on = true
          }
        sb.toString()
      }

    }

    val filePath = os.resource / "day3.txt"

    val fileAsString = os.read(filePath)

    val sb = preprocess(fileAsString) 

    os.write(os.pwd / "out1.txt",fileAsString.toString())
    os.write(os.pwd / "out.txt",sb.toString())

    val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val matches: Iterator[Regex.Match] = pattern.findAllMatchIn(sb)

    var sum = 0L
    matches.foreach { m =>
      val x = m.group(1).toInt
      val y = m.group(2).toInt
      // println(s"$x $y")
      sum = sum + x * y
    }
    println(sum)

  }

}
