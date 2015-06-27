package jp.qilab.sushi.largenumbers

import jp.qilab.sushi.runtime.RichBigInt

object Implicits {

  implicit def bigIntWrapper(x: BigInt) = new RichBigInt(x)

}
