import org.scalacheck._

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Prop.forAll

class Day7Spec extends AnyPropSpec with ScalaCheckPropertyChecks {

  val smallInt                              = Gen.choose(1, 100)
  def smallIntList(nr: Int): Gen[List[Int]] = Gen.listOfN(nr, smallInt)
  case class Valid(operators: List[Boolean], nums: List[Int])

  def listofBooleans(n: Int): Gen[List[Boolean]] = Gen.listOfN(n, Gen.oneOf(List(true, false)))
  val genValid: Gen[Valid] = for {
    nr   <- Gen.chooseNum(2, 10)
    list <- smallIntList(nr)
    ops  <- listofBooleans(list.size)
  } yield Valid(ops, list)

  val shrinkValidX: Shrink[Valid] = Shrink { valid =>
    // println(s"Shrinking: $valid") // Debug statement

    def shrinkValid(v: Valid): Stream[Valid] =
      if (v.nums.isEmpty)
        Stream.empty
      else {
        val smaller: Valid = Valid(v.operators, v.nums.take(v.nums.size / 2))
        Stream(smaller) #::: shrinkValid(smaller)
      }
    shrinkValid(valid)
  }

  property("correct numbers") {
    var num = 0
    def test(valid: Valid): Boolean = {
      val nums = valid.nums.toVector
      val ops  = valid.operators.toVector
      var sum  = nums(0).toLong
      for (i <- 1 until nums.size)
        if (ops(i)) {
          sum = sum + nums(i)
        } else {
          sum = sum * nums(i)
        }

      val res = day07.checkLine(sum +: valid.nums.map(_.toLong))
      if (true) {
        num = num + 1
        println(num)
        //val stream = shrinkValidX.shrink(valid)
        //for (s <- stream)
        //  println(s.nums)

      }
      assert(res == sum)
      true
    }

    forAll(genValid)(test)(implicitly, shrinkValidX)
  }
  
  property("incorrect sum") {

    var num = 0
    def test(valid: Valid): Boolean = {
      val nums = valid.nums.toVector
      val ops  = valid.operators.toVector
      var sum  = nums(0).toLong
      for (i <- 1 until nums.size)
        if (ops(i)) {
          sum = sum + nums(i)
        } else {
          sum = sum * nums(i)
        }

      val res = day07.checkLine((sum+100)+: valid.nums.map(_.toLong))
      if (true) {
        num = num + 1
        println(num)
        //val stream = shrinkValidX.shrink(valid)
        //for (s <- stream)
        //  println(s.nums)

      }
      if(res != 0){
        println(sum)
        println(valid.nums)
      }
      assert(res == 0 )
      true
    }

    forAll(genValid)(test)(implicitly, shrinkValidX)
  }
    


}
