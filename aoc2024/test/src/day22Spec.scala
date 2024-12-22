import org.scalatest._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.{AnyOperators, forAll, all}
import org.scalacheck.Gen
import day22._

class Day22Spec extends AnyPropSpec with Checkers {

    property("mix") {
        assert(mix(42, 15) == 37)
    }
    property("prune") {
        assert(prune(100000000L) == 16113920L)
    }

    property("seq check"){
        val secret = Secret(123, Vector.empty)
        val stream = secret.stream.drop(10).head
        val res = stream.seq2Value.get(Vector(-1L,-1L,0L,2L).reverse)
        assert(res == Some(6))
        
    }
}
