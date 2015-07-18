package jp.qilab.sushi.runtime

import jp.qilab.sushi.largenumbers._
import scala.annotation.tailrec

final class RichInt(val self: Int) {

  def pow(exp: Int): BigInt = BigInt(self).pow(exp)
  def pow(exp: BigInt): BigInt = jp.qilab.sushi.largenumbers.pow(self, exp)

  def !(): BigInt = factorialRec(1, self)

  @tailrec
  private[this] def factorialRec(c: BigInt, n: Int): BigInt = {
    require(n >= 0)
    if (n == 0) c else factorialRec(c * n, n - 1)
  }

  def ^^(exp: Int): BigInt = tetration(self, exp)

  def ^^^(exp: Int): BigInt = pentation(self, exp)

}
