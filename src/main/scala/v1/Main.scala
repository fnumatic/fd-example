package v1

import reactive._

object Main extends App  with Platine[Unit, Unit] {
  val readnetto = Read_netto()
  val printBrutto = Print_brutto()
  val calc_mwst = Calc_mwst()

  in >> readnetto.in
  readnetto.out_exit >> calc_mwst.in
  calc_mwst.out >> printBrutto.in
  printBrutto.out >> readnetto.in

  in.fire()

  case class Read_netto() extends FuncUnit[Unit, Option[Float]] {
    val out_exit = out.takeWhile(_ != None).map(_.get)

    def process(i: Unit) = try Some(readLine("netto: ").toFloat) catch {case _ => None}
  }

  case class Print_brutto() extends FuncUnit[Float, Unit] {
    def process(f: Float) {
      println("brutto:  " + f)
    }
  }

}


case class Calc_mwst() extends Platine[Float, Float] {

  val mwst = 0.19F
  val netto_result = Var(0F)

  val sum_netto = Sum_float()
  val calc_steuer = Calc_steuer()
  val calc_brutto = Sum_float()

  in >> sum_netto.in
  sum_netto.out >> netto_result
  sum_netto.out >> calc_steuer.in
  calc_steuer.out >> calc_brutto.in
  calc_brutto.out >> out

  case class Sum_float() extends FuncUnit[Float, Float] {
    def process(f: Float): Float = f + netto_result.now
  }

  case class Calc_steuer() extends FuncUnit[Float, Float] {
    def process(f: Float) = f * mwst
  }

}

trait Fu[T] extends Observing {
  val in = new EventSource[T] {}
}

trait Platine[T, U] extends Fu[T] {
  val out = new EventSource[U] {}
}

trait FuncUnit[T, U] extends Fu[T] {
  val out = in map process _

  def process(i: T): U
}
