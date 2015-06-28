package jp.qilab.sushi

import jp.qilab.sushi.largenumbers.Implicits._
import scala.annotation.tailrec

package object largenumbers {

  def halfOf(x: BigInt): BigInt = x >> 1

  def pow(base: BigInt, exp: BigInt): BigInt = {
    pow(1, base, exp)
  }

  @tailrec
  def pow(coefficient: BigInt, base: BigInt, exp: BigInt): BigInt = {
    if (exp.isValidInt)
      coefficient * base.pow(exp.toInt)
    else {
      val (c: BigInt, b, e) = if (exp.isOdd) (base, base, exp - 1) else (1, base * base, exp >> 1)
      pow(coefficient * c, b, e)
    }
  }

  @tailrec
  def powRec(base: BigInt, exp: BigInt, n: Int): BigInt = {
    require(n > 0)
    if (n == 1) base
    else if (n == 2) pow(base, exp)
    else powRec(base, pow(base, exp), n - 1)
  }

  def tetration(base: Int, height: Int): BigInt = {
    require(base >= 0 || height >= -1)
    if (base == 0) {
      (height + 1) % 2
    } else if (height == 0) {
      1
    } else if (height == -1) {
      0
    } else {
      powRec(base, base, height)
    }
  }

}
