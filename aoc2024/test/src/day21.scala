import org.scalatest._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.{AnyOperators, forAll, all}
import org.scalacheck.Gen
import day21._


class Day21Spec extends AnyPropSpec with Checkers {
  val singleDigit       = Gen.choose(0, 9).map(x => x.toString.charAt(0))
  val singleDigitorA    = Gen.oneOf(singleDigit, 'A')
  val digitsFollowedByA = Gen.listOf(singleDigit).map(_.mkString + "A")

  val directions = Gen.oneOf(day21.DA, day21.DDown, day21.DLeft, day21.DRight, day21.DUp)
  property("NumPad to ControllPad conversion single digit") {
    check(
      forAll(singleDigit) { (c: Char) =>
        val numpad  = day21.char2Numpad(c)
        val dpad    = day21.num2Dpad(Vector(numpad))
        val reverse = day21.dpad2Num(dpad)
        reverse == Vector(numpad)
      },
    )
  }
  property("NumPad to ControllPad conversion multiple digits") {
    check(
      forAll(digitsFollowedByA) { (s: String) =>
        val numpad  = s.map(x => day21.char2Numpad(x)).toVector
        val dpad    = day21.num2Dpad(numpad)
        val reverse = day21.dpad2Num(dpad)
        reverse == numpad
      },
    )
  }
  property("Dpad to Dpad conversion mul") {
    check(
      forAll(Gen.listOf(directions)) { (dsl: List[day21.DPad]) =>
        val ds      = dsl.toVector
        val encoded = day21.dpadEncode(ds)
        val decoded = day21.dpadDecode(encoded)
        decoded == ds
      },
    )
  }
  property("Dpad to Dpad conversion mul2") {
    check(
      forAll(Gen.listOf(directions)) { (dsl: List[day21.DPad]) =>
        val ds      = dsl.toVector
        val encoded = day21.dpadEncode(day21.dpadEncode(ds))
        val decoded = day21.dpadDecode(day21.dpadDecode(encoded))
        decoded == ds
      },
    )
  }
  /*
  property("Dpad gotoVariants test") {
    check(
      forAll(directions) { from =>
        forAll(directions) { (to) =>
          val variants = day21.DPad.gotoVariants(from,to)
          
        }
      }
    )
  }*/
}
