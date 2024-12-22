import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import mu.PriorityQueue
import scala.annotation.tailrec

object day22 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day22_sample2.txt"
    } else {
      os.resource / "day22.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector
  val inNums                       = inputStrings.map(_.toLong)

  @main
  def day22_01(): Unit = {
    val twok = inNums.map(Secret(_, Vector.empty)).map(_.stream.drop(2000).head.secret)
    println(twok.sum)
  }

  @main
  def day22_02(): Unit = {
    val twok = inNums.map(Secret(_, Vector.empty)).map(_.stream.drop(2000).head)    
    if(printstuff){
    twok.foreach(x => {
      println(x.seq2Value.get(Vector(-2L,1L, -1L, 3).reverse))
    })
   }
    val combined = combineMaps(twok.map(x => x.seq2Value))
    val banana = combined.values.max
    println(banana)
  }

  def combineMaps(maps: Vector[Map[Vector[Long], Long]]): Map[Vector[Long], Long] = {
    maps.foldLeft(Map.empty[Vector[Long], Long]) { (acc, map) =>
      map.foldLeft(acc) { case (innerAcc, (key, value)) =>
        innerAcc.updated(key, innerAcc.getOrElse(key, 0L) + value)
      }
    }
  }
  @inline
  def mix(secret: Long, givenNumber: Long): Long = {
    secret ^ givenNumber
  }

  @inline
  def prune(secret: Long): Long = {
    secret % 16777216
  }

  case class Secret(val secret: Long, change: Vector[Long], seq2Value: Map[Vector[Long], Long] = Map.empty) {

    def stream: LazyList[Secret] = {
      def loop(s: Secret): LazyList[Secret] = s #:: loop(s.next())
      loop(this)
    }

    @inline
    def step1(secret: Long): Long = {
      val mult = secret * 64L
      prune(mix(mult, secret))
    }
    @inline
    def step2(secret: Long): Long = {
      val div = secret / 32L
      prune(mix(div, secret))
    }
    @inline
    def step3(secret: Long): Long = {
      val mult2 = secret * 2048L
      prune(mix(mult2, secret))
    }
    @inline
    def next(): Secret = {
      val nextval   = step3(step2(step1(secret)))
      val prevdigit = secret  % 10
      val newdigit  = nextval % 10
      val changeval = newdigit - prevdigit
      val newchange = changeval +: change.take(3)
      if (seq2Value.contains(newchange) || newchange.size != 4) {
        Secret(step3(step2(step1(secret))), newchange, seq2Value)
      } else {
        Secret(step3(step2(step1(secret))), newchange, seq2Value + (newchange -> newdigit))
      }

    }
  }
}
