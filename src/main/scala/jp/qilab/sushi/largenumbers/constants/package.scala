package jp.qilab.sushi.largenumbers

import jp.qilab.sushi.largenumbers.Implicits._

package object constants {

  def Googol: BigInt = 10.pow(100)
  def Googolplex: BigInt = 10.pow(Googol)

}
