package jp.qilab.sushi.runtime

import org.scalatest.{FunSpec, Matchers}

class RichBigIntSpec extends FunSpec with Matchers {

  describe("RichBigInt") {
    describe("isOdd") {
      it("returns true iff self is odd") {
        for (i <- 0 to 10)
          new RichBigInt(i).isOdd should equal ((i % 2).abs == 1)
      }
    }

    describe("isEven") {
      it("returns true iff self is even") {
        for (i <- -10 to 10)
          new RichBigInt(i).isEven should equal (i % 2 == 0)
      }
    }

    describe("half") {
      it("returns half of self if self is even.") {
        for (i <- -10 to 10 by 2)
          new RichBigInt(i).half should equal (i / 2)

        val v = BigInt(123456789) * BigInt(12345)
        new RichBigInt(v * 2).half should equal (v)
      }

      it("returns half of (self - 1) if self is odd.") {
        for (i <- -11 to 11 by 2)
          new RichBigInt(i).half should equal ((i - 1) / 2)

        val v = BigInt(123456789) * BigInt(12345)
        new RichBigInt(v * 2 + 1).half should equal (v)
      }
    }
  }
}
