import chisel3._
import chisel3.util.{Valid, Decoupled, log2Ceil, Fill}

import Helpers._

class DividerInput(val bits: Int) extends Bundle {
  val dividend = UInt(bits.W)
  val divisor  = UInt(bits.W)
  val is32bit  = Bool()
  val signed   = Bool()
  val extended = Bool()
  val modulus  = Bool()
}

class SimpleDivider(val bits: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new DividerInput(bits)))
    val out = Output(Valid(UInt(bits.W)))
  })

  val dividend = Reg(UInt((bits*2+1).W))
  val divisor = Reg(UInt(bits.W))
  val quotient = Reg(UInt(bits.W))
  val is32bit = Reg(Bool())
  val negativeResult = Reg(Bool())
  val modulus = Reg(Bool())
  val count = Reg(UInt(log2Ceil(bits+1).W))
  val busy = RegInit(false.B)

  io.in.ready := !busy

  when (io.in.valid && !busy) {
    when (io.in.bits.is32bit) {
      val dividendTemp = Mux(io.in.bits.extended, io.in.bits.dividend(31, 0) ## 0.U(32.W), io.in.bits.dividend(31, 0))

      when (io.in.bits.signed) {
        dividend := Mux(dividendTemp(31), (-(dividendTemp(31, 0).asSInt)).asUInt, dividendTemp(31, 0))
        divisor := Mux(io.in.bits.divisor(31), (-(io.in.bits.divisor(31, 0).asSInt)).asUInt, io.in.bits.divisor(31, 0))
        negativeResult := dividendTemp(31) ^ io.in.bits.divisor(31)
      } .otherwise {
        dividend := dividendTemp
        divisor := io.in.bits.divisor(31, 0)
        negativeResult := 0.U
      }
    } .otherwise {
      val dividendTemp = Mux(io.in.bits.extended, io.in.bits.dividend ## 0.U(bits.W), io.in.bits.dividend)

      when (io.in.bits.signed) {
        dividend := Mux(dividendTemp(bits-1), (-(dividendTemp(bits-1, 0).asSInt)).asUInt, dividendTemp(bits-1, 0))
        divisor := Mux(io.in.bits.divisor(bits-1), (-(io.in.bits.divisor(bits-1, 0).asSInt)).asUInt, io.in.bits.divisor(bits-1, 0))
        negativeResult := (dividendTemp(bits-1) =/= io.in.bits.divisor(bits-1))
      } .otherwise {
        dividend := dividendTemp
        divisor := io.in.bits.divisor(bits-1, 0)
        negativeResult := false.B
      }
    }

    is32bit := io.in.bits.is32bit
    quotient := 0.U
    count := 0.U
    modulus := io.in.bits.modulus
    busy := true.B
  }

  when (busy) {
    when ((dividend(bits*2) === 1.U) || (dividend(bits*2-1, bits) >= divisor)) {
        dividend := (dividend(bits*2-1, bits) - divisor) ## dividend(bits-1, 0) ## 0.U(1.W)
        quotient := quotient(bits-2, 0) ## 1.U(1.W)
      } .otherwise {
        dividend := dividend((bits*2-1), 0) ## 0.U(1.W)
        quotient := quotient(bits-2, 0) ## 0.U(1.W)
    }
    count := count + 1.U
  }

  // Did we shift out a 1? This also handles divide by zero
  val overflow = Reg(Bool())
  overflow := quotient(bits-1)
  when (is32bit) {
    overflow := quotient(63, 31).orR
  }

  val result = WireDefault(Mux(negativeResult, -quotient, quotient))
  when (overflow) {
    result := 0.U
  } .elsewhen (is32bit && !modulus) {
    result := Mux(negativeResult, -quotient(31, 0), quotient(31, 0))
  }

  io.out.bits := RegNext(result)
  io.out.valid := RegNext((count === (bits+1).U) && busy)
  when (io.out.valid) {
    busy := false.B
  }
}

object SimpleDividerObj extends App {
  chisel3.Driver.execute(Array[String](), () => new SimpleDivider(64))
}
