package jp.qilab.sushi.runtime

import org.scalatest.{FunSpec, Matchers}

class RichBigIntSpec extends FunSpec with Matchers {

  describe("RichBigInt") {
    describe("isOdd") {
      it("returns true iff self is odd") {
        new RichBigInt(-1).isOdd should equal (true)
        new RichBigInt(0).isOdd should equal (false)
        new RichBigInt(1).isOdd should equal (true)
        new RichBigInt(2).isOdd should equal (false)
        new RichBigInt(3).isOdd should equal (true)
        new RichBigInt(4).isOdd should equal (false)
        new RichBigInt(5).isOdd should equal (true)
        new RichBigInt(6).isOdd should equal (false)
      }
    }

    describe("isEven") {
      it("returns true iff self is odd") {
        new RichBigInt(-1).isEven should equal (false)
        new RichBigInt(0).isEven should equal (true)
        new RichBigInt(1).isEven should equal (false)
        new RichBigInt(2).isEven should equal (true)
        new RichBigInt(3).isEven should equal (false)
        new RichBigInt(4).isEven should equal (true)
        new RichBigInt(5).isEven should equal (false)
        new RichBigInt(6).isEven should equal (true)
      }
    }
  }
}
