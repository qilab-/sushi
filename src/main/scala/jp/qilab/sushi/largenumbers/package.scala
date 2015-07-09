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
  private[this] def powRec(base: BigInt, exp: BigInt, n: Int): BigInt = {
    require(n > 0)
    n match {
      case 1 => base
      case 2 => pow(base, exp)
      case _ => powRec(base, pow(base, exp), n - 1)
    }
  }

  def tetration(base: Int, height: Int): BigInt = {
    require(base >= 0 && height >= -1)
    (base, height) match {
      case (0, h)  => (h + 1) % 2
      case (_, -1) => 0
      case (_, 0)  => 1
      case (b, h)  => powRec(b, b, h)
    }
  }

  def hyper0(base: Int, exp: Int): BigInt = {
    require(base >= 0 && exp >= 0)
    exp + 1
  }

  def hyper1(base: Int, exp: Int): BigInt = {
    require(base >= 0 && exp >= 0)
    base + exp
  }

  def hyper2(base: Int, exp: Int): BigInt = {
    require(base >= 0 && exp >= 0)
    base * exp
  }

  def hyper3(base: Int, exp: Int): BigInt = {
    require(base >= 0 && exp >= 0)
    pow(base, exp)
  }

  def hyper4(base: Int, exp: Int):BigInt = {
    require(base >= 0 && exp >= 0)
    tetration(base, exp)
  }

//  @tailrec
//  def hyper(rank: Int)(base: Int, exp: Int): Int = {
//    (rank, base, exp) match {
//      case (0, _, e) => e + 1
//      case (1, b, 0) => b
//      case (2, _, 0) => 0
//      case (_, _, 0) => 1
//      case (r, b, e) => hyper(r - 1)(b, hyper(r)(b, e - 1))
//    }
//  }

}
