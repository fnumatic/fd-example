package v3

import reactive._
import FuConvert._

object Main extends App with Platine[Unit, Unit] {

  val read_netto    = tofu(Read_netto)
  val print_brutto  = tofu(Print_brutto)
  val calc_tax      = Calc_mwst()
  val exit_ondemand = toex(Exit_ondemand, Option.empty[Float])

  this #> read_netto >> exit_ondemand >> calc_tax >> print_brutto >> read_netto

  execute()


  def Read_netto(u: Unit) = try Some(readLine("netto: ").toFloat) catch {case _ => None}

  def Print_brutto(f: Float) {println("brutto:  " + f)}

  def Exit_ondemand(o: Option[Float]) = o.getOrElse(0F)

}


case class Calc_mwst() extends Platine[Float, Float] {

  val taxrate = 0.19F

  val sum_netto   = tofu(Sum_float)
  val calc_tax    = tofu(Calc_tax)
  val calc_brutto = tofu(Sum_float)
  val memo_netto  = Box(0F)

  this #> sum_netto >> memo_netto >> calc_tax >> calc_brutto ># this

  def Sum_float(f: Float) = f + memo_netto.value

  def Calc_tax(f: Float) = f * taxrate

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
  def >#[S](target: Fu[S, U]) = {
    out.foreach(target.out.fire _)
    target
  }

  /**
   * in to in
   */
  def #>[Z](target: Fu[T, Z]) = {
    in.foreach(target.in.fire _)
    target
  }

  def execute(t: T) {in.fire(t)}
}

trait Platine[T, U] extends Fu[T, U]

trait FuncUnit[T, U] extends Fu[T, U] {
  in =>> {v => out.fire(process(v))}

  def process(t: T): U
}

case class Box[T](zero: T) extends Fu[T, T] {
  var v = zero

  in =>> ( v = _ )
  in =>> out.fire _

  def value = v
}

abstract class Exit[T, U](exit: T) extends Fu[T, U] {
  in.takeWhile(_ != exit).foreach(v => out.fire(process(v)))

  def process(t: T): U
}

class FuConvert[T, R](f: T => R) {
  def tofu = FuConvert.tofu(f)
}

object FuConvert {
  def tofu[T, R](f: T => R) = new FuncUnit[T, R] {
    def process(t: T) = f(t)
  }

  def toex[T, R](f: T => R, e: T) = new Exit[T, R](e) {
    def process(t: T) = f(t)
  }

  implicit def rich2[T, R](f: T => R): FuConvert[T, R] = new FuConvert[T, R](f)
}