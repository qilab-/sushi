package jp.qilab.sushi.largenumbers

import org.scalatest.{FunSpec, Matchers}

class packageSpec extends FunSpec with Matchers {

  describe("package") {
    describe("halfOf(x)") {
      it("returns half of x if x is even.") {
        for (i <- -10 to 10 by 2)
          halfOf(i) should equal (i / 2)

        val v = BigInt(123456789) * BigInt(12345)
        halfOf(v * 2) should equal (v)
      }

      it("returns half of (x - 1) if x is odd.") {
        for (i <- -11 to 11 by 2)
          halfOf(i) should equal ((i - 1) / 2)

        val v = BigInt(123456789) * BigInt(12345)
        halfOf(v * 2 + 1) should equal (v)
      }
    }

    describe("pow(base, exp)") {
      it("throws ArithmeticException if exp is negative.") {
        intercept[ArithmeticException] {
          pow(BigInt(1), BigInt(-1))
        }
      }

      it("returns 1 if base == 0 and exp == 0") {
        pow(BigInt(0), BigInt(0)) should equal (BigInt(1))
      }

      it("returns base raised to the expth power.") {
        // base = 0
        pow(BigInt(0), BigInt(1)) should equal (BigInt(0))
        pow(BigInt(0), BigInt(2)) should equal (BigInt(0))
        pow(BigInt(0), BigInt(3)) should equal (BigInt(0))
        // base = 1
        pow(BigInt(1), BigInt(0)) should equal (BigInt(1))
        pow(BigInt(1), BigInt(1)) should equal (BigInt(1))
        pow(BigInt(1), BigInt(2)) should equal (BigInt(1))
        pow(BigInt(1), BigInt(3)) should equal (BigInt(1))
        // base = -1
        pow(BigInt(-1), BigInt(0)) should equal (BigInt(1))
        pow(BigInt(-1), BigInt(1)) should equal (BigInt(-1))
        pow(BigInt(-1), BigInt(2)) should equal (BigInt(1))
        pow(BigInt(-1), BigInt(3)) should equal (BigInt(-1))
        // base > max value of Int
        val v = BigInt(123456789) * BigInt(12345)
        pow(v, 3) should equal (v * v * v)
        // exp > max value of Int => the answer becomes too big.
      }
    }

    describe("pow(coefficient, base, exp)") {
      it("returns coefficient times pow(base, exp)") {
        val c = BigInt(123456789) * BigInt(12345)
        val b = BigInt(Int.MaxValue) + BigInt(2)
        pow(c, b, 3) should equal (c * b * b * b)
      }
    }

    describe("tetration(base, height)") {
      it("returns tetration.") {
        // base == 0
        tetration(0, -1) should equal(BigInt(0))
        tetration(0, 0) should equal(BigInt(1))
        tetration(0, 1) should equal(BigInt(0))
        tetration(0, 2) should equal(BigInt(1))
        tetration(0, 3) should equal(BigInt(0))
        tetration(0, 4) should equal(BigInt(1))
        // height == -1, then return 0
        for (i <- 1 to 9)
          tetration(i, -1) should equal (BigInt(0))
        // height == 0, then return 1
        for (i <- 1 to 9)
          tetration(i, 0) should equal (BigInt(1))
        // height == 1, then return base
        for (i <- 1 to 9)
          tetration(i, 1) should equal (BigInt(i))
        // height == 2
        tetration(1, 2) should equal (BigInt(1))
        tetration(2, 2) should equal (BigInt(4))
        tetration(3, 2) should equal (BigInt(27))
        tetration(4, 2) should equal (BigInt(256))
        tetration(5, 2) should equal (BigInt(3125))
        tetration(6, 2) should equal (BigInt(46656))
        tetration(7, 2) should equal (BigInt(823543))
        tetration(8, 2) should equal (BigInt(16777216))
        tetration(9, 2) should equal (BigInt(387420489))
        tetration(10, 2) should equal (BigInt(10000) * BigInt(1000000))
        // height == 3
        tetration(1, 3) should equal (BigInt(1))
        tetration(2, 3) should equal (BigInt(16))
        tetration(3, 3) should equal (BigInt(3).pow(20) * BigInt(3).pow(7))
        // height == 4
        tetration(1, 4) should equal (BigInt(1))
        tetration(2, 4) should equal (BigInt(65536))
      }
    }
  }
}
