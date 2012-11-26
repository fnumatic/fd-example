package taxcalc

import fnumatic.ebc.{FuncUnit, Fu, Split, Board}
import fnumatic.ebc.FuConvert._

class ShowResult extends FuncUnit[Float, Unit] {
  def process(f: Float) { println("brutto:  " + f)}
}

case class ReadNetto() extends Split[Unit] {
  val succ = get2[Float]
  val err = get2[Unit]

  def split(t: Unit) {
    try
      succ(readLine("netto: ").toFloat)
    catch {case _ => err()}
  }
}
