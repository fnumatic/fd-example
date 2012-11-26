package taxcalc

import fnumatic.ebc.{Split, Board}
import fnumatic.ebc.FuConvert._

class Ui extends Board[Unit, Unit] {
  lazy val read  = ReadNetto()
  lazy val print = fu(Print_Brutto)
  lazy val netto = get2[Float]

  case class ReadNetto() extends Split[Unit] {
    val succ = get2[Float]

    def split(t: Unit) {
      try
        succ(readLine("netto: ").toFloat)
      catch {case _ =>}
    }
  }

  def Print_Brutto(f: Float) { println("brutto:  " + f) }
}