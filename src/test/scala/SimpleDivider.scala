import org.scalatest._
import chisel3.tester._
import chisel3._

import TestValues._

class SimpleDividerUnitTester extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "SimpleDivider"

  val tests = for {
    //x <- testValuesShort
    //y <- testValuesShort
    x <- testValuesRealShort
    y <- testValuesRealShort
  } yield (x, y)

  def divide(a: BigInt, b: BigInt): BigInt = (a / b)

  def divideExtended(a: BigInt, b: BigInt): BigInt = {
    val d = ((a << 64) / b)
    if ((d >> 64) == 0) d else 0
  }

  def runOneTest(m: SimpleDivider, divide: (BigInt, BigInt) => BigInt) = {
    for ((x, y) <- tests) {

      while (m.io.in.ready.peek().litToBoolean == false) {
        m.clock.step(1)
      }

      m.io.in.bits.dividend.poke(x.U)
      m.io.in.bits.divisor.poke(y.U)
      m.io.in.valid.poke(true.B)
      m.clock.step(1)
      m.io.in.valid.poke(false.B)

      while (m.io.out.valid.peek().litToBoolean == false) {
        m.clock.step(1)
      }

      m.io.out.bits.expect(divide(x, y).U)
    }
  }

  it should "pass a unit test" in {
    test(new SimpleDivider(64)) { m =>
      runOneTest(m, divide)

      m.io.in.bits.extended.poke(true.B)
      runOneTest(m, divideExtended)
    }

  }
}
