package jp.qilab.sushi.largenumbers

import jp.qilab.sushi.runtime.{RichBigInt, RichInt}
import scala.language.implicitConversions

object Implicits {

  implicit def intWrapper(x: Int) = new RichInt(x)

  implicit def bigIntWrapper(x: BigInt) = new RichBigInt(x)

}
