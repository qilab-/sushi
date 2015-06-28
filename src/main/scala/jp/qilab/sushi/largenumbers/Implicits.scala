package jp.qilab.sushi.largenumbers

import jp.qilab.sushi.runtime.RichBigInt
import scala.language.implicitConversions

object Implicits {

  implicit def bigIntWrapper(x: BigInt) = new RichBigInt(x)

}
