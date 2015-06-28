package jp.qilab.sushi.runtime

final class RichBigInt(val self: BigInt) {

  def isOdd: Boolean = self.testBit(0)
  def isEven: Boolean = !isOdd

  def half: BigInt = self >> 1

}
