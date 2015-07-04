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
      val (c: BigInt, b, e) = if (exp.isOdd) (base, base, exp - 1) else (1, base * base, halfOf(exp))
      pow(coefficient * c, b, e)
    }
  }

  @tailrec
  def powRec(base: BigInt, exp: BigInt, n: Int): BigInt = {
    require(n > 0)
    n match {
      case 1 => base
      case 2 => pow(base, exp)
      case _ => powRec(base, pow(base, exp), n - 1)
    }
  }

  def tetration(base: Int, height: Int): BigInt = {
    require(base >= 0 || height >= -1)
    (base, height) match {
      case (0, h)  => (h + 1) % 2
      case (_, -1) => 0
      case (_, 0)  => 1
      case (b, h)  => powRec(b, b, h)
    }
  }

}
