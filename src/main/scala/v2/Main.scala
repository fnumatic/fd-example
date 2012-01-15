package v2

import reactive._

object Main extends App with Platine[Unit, Unit] {
  val readnetto   = Read_netto()
  val printBrutto = Print_brutto()
  val calc_mwst   = Calc_mwst()
  val read_exit   = Read_exit()

  this |> readnetto >> read_exit >> calc_mwst >> printBrutto >> readnetto

  in.fire()

  case class Read_netto() extends FuncUnit[Unit, Option[Float]] {
    val out_exit = out.takeWhile(_ != None).map(_.get)

    def process(i: Unit) = try Some(readLine("netto: ").toFloat) catch {
      case _ => None
    }
  }

  case class Print_brutto() extends FuncUnit[Float, Unit] {
    def process(f: Float) {println("brutto:  " + f)}
  }

  case class Read_exit() extends Exit[Option[Float], Float](None) {
    def process(i: Option[Float]) = i.getOrElse(0F)
  }

}


case class Calc_mwst() extends Platine[Float, Float] {

  val mwst = 0.19F

  val sum_netto   = Sum_float()
  val calc_steuer = Calc_steuer()
  val calc_brutto = Sum_float2()
  val netto_sink  = Box(0F)

  ( this |> sum_netto ) >> netto_sink >> calc_steuer >> calc_brutto >| this


  case class Sum_float() extends FuncUnit[Float, Float] {
    def process(f: Float) = f + netto_sink.value
  }

  case class Sum_float2() extends FuncUnit[Float, Float] {
    def process(f: Float) = f + netto_sink.value
  }

  case class Calc_steuer() extends FuncUnit[Float, Float] {
    def process(f: Float) = f * mwst
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

  in =>> ( v = _)
  in =>> out.fire _

  def value = v
}

abstract class Exit[T, U](exit: T) extends Fu[T, U] {
  in.takeWhile(_ != exit).foreach(v => out.fire(process(v)))

  def process(i: T): U
}
