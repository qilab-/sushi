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

  @tailrec
  private[this] def powRec(base: BigInt, exp: BigInt, n: BigInt): BigInt = {
    require(n > 0)
    if (n.isValidInt)
      powRec(base, exp, n.toInt)
    else
      powRec(base, pow(base, exp), n - 1)
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

  private[this] def tetration(base: BigInt, height: BigInt): BigInt = {
    require(base >= 0 && height >= -1)
    (base, height) match {
      case (b, h) if b.isValidInt && h.isValidInt => tetration(b.toInt, h.toInt)
      case (b, h) if b == 0  => (h + 1) % 2
      case (_, h) if h == -1 => 0
      case (_, h) if h == 0  => 1
      case (b, h)            => powRec(b, b, h)
    }
  }

  @tailrec
  private[this] def tetRec(base: BigInt, height: BigInt, n: Int): BigInt = {
    require(n > 0)
    n match {
      case 1 => base
      case 2 => tetration(base, height)
      case _ => tetRec(base, tetration(base, height), n - 1)
    }
  }

  def pentation(base: Int, exp: Int): BigInt = {
    require(base >= 0 && exp >= -1)
    (base, exp) match {
      case (0, e)  => (e + 1) % 2
      case (_, -1) => 0
      case (_, 0)  => 1
      case (1, _)  => 1
      case (b, e)  => tetRec(b, b, e)
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
