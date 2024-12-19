import scala.util.matching.Regex

import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import scala.annotation.tailrec

import mu.PriorityQueue

object day19 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day19_sample.txt"
    } else {
      os.resource / "day19.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  def canBeMadeBySubstrings(svec: Vector[String], regex: Regex): Int = {
    val counts = svec.map(s => regex.matches(s))
    counts
      .map(x => {
        if (x) {
          1
        } else {
          0
        }
      })
      .sum
  }

  def createRegex(substrings: Vector[String]): Regex = {
    // Create a regex pattern that matches any of the substrings
    val pattern = substrings.map(Regex.quote).mkString("|")
    // Use the pattern to check if the entire string can be matched
    val regex = s"^($pattern)*$$".r
    regex
  }

  def readInput(): (Vector[String], Vector[String]) = {
    val (towels, patterns) = inputStrings.span(_.nonEmpty)
    val tvec               = towels.head.split(",").map(x => x.trim()).toVector
    (tvec, patterns.filter(_.nonEmpty))
  }

  @main
  def day19_01(): Unit = {
    val (tvec, patterns) = readInput()
    val res              = canBeMadeBySubstrings(patterns, createRegex(tvec))
    println(res)

  }

  def sumNrOfWays(patterns: Vector[String], substrings: Vector[String]): Long = {
    val regex     = createRegex(substrings)
    val stringset = substrings.toList
    patterns
      .map(p => {
        if (regex.matches(p)) {
          val nr = nrOfWays(p, stringset)
          //println(nr)
          nr
        } else {
          0L
        }
      })
      .sum
  }

  def nrOfWays(pattern: String, substrings: List[String]): Long = {
    val cache = Array.ofDim[Long](pattern.size)

    for (i <- 0 until pattern.size) {
      val focusStr = pattern.substring(pattern.size - 1 - i, pattern.size)
      val perm = substrings.map ( sub => {
        val res: Long = if (focusStr.startsWith(sub)) {
          val rest = focusStr.substring(sub.length())
          val count = {
            if (rest.size == 0) {
              1L
            } else {
              cache(rest.size - 1)
            }
          }
          count
        } else 0L 
        res
      }
      ).sum
      
      cache(i) = perm 
    }
    //println(cache.last)
    cache.last
  }

  @main
  def day19_02(): Unit = {
    val (tvec, patterns) = readInput()
    val res              = sumNrOfWays(patterns, tvec)
    println(res)
  }
}
