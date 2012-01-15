package v2

import reactive._

object Main extends App with Platine[Unit, Unit] {
  val read_netto   = Read_netto()
  val print_brutto = Print_brutto()
  val calc_tax   = Calc_mwst()
  val exit_ondemand   = Exit_ondemand()

  this |> read_netto >> exit_ondemand >> calc_tax >> print_brutto >> read_netto

  in.fire()

  case class Read_netto() extends FuncUnit[Unit, Option[Float]] {
    def process(i: Unit) = try Some(readLine("netto: ").toFloat) catch {
      case _ => None
    }
  }

  case class Print_brutto() extends FuncUnit[Float, Unit] {
    def process(f: Float) {println("brutto:  " + f)}
  }

  case class Exit_ondemand() extends Exit[Option[Float], Float](None) {
    def process(i: Option[Float]) = i.getOrElse(0F)
  }

}


case class Calc_mwst() extends Platine[Float, Float] {

  val taxrate = 0.19F

  val sum_netto   = Sum_float()
  val calc_tax = Calc_steuer()
  val calc_brutto = Sum_float()
  val memo_netto  = Box(0F)

  ( this |> sum_netto ) >> memo_netto >> calc_tax >> calc_brutto >| this


  case class Sum_float() extends FuncUnit[Float, Float] {
    def process(f: Float) = f + memo_netto.value
  }

  case class Calc_steuer() extends FuncUnit[Float, Float] {
    def process(f: Float) = f * taxrate
  }

}

trait Fu[T, U] extends Observing {
  val in  = new EventSource[T] {}
  val out = new EventSource[U] {}

  def >>[S](target: Fu[U, S]) = {
    out.foreach(target.in.fire _)
    target
  }

  /**
   * out to out
   */
  def >|[S](target: Fu[S, U]) = {
    out.foreach(target.out.fire _)
    target
  }

  /**
   * in to in
   */
  def |>[Z](target: Fu[T, Z]) = {
    in.foreach(target.in.fire _)
    target
  }
}

trait Platine[T, U] extends Fu[T, U]

trait FuncUnit[T, U] extends Fu[T, U] {
  in =>> {v => out.fire(process(v))}

  def process(i: T): U
}

case class Box[T](zero: T) extends Fu[T, T] {
  var v = zero

  in =>> ( v = _ )
  in =>> out.fire _

  def value = v
}

abstract class Exit[T, U](exit: T) extends Fu[T, U] {
  in.takeWhile(_ != exit).foreach(v => out.fire(process(v)))

  def process(i: T): U
}

