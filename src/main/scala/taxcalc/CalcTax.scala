package taxcalc

import fnumatic.ebc.{Store, Board}
import fnumatic.ebc.FuConvert._

class CalcTax extends Board[Float, Float] {

  val taxrate = 0.19F

  val sum_netto   = fu(Sum_float)
  val calc_tax    = fu(Calc_tax)
  val calc_brutto = fu(Sum_float)
  val memo_netto  = Store(0F)


  def Sum_float(f: Float):Float = f + memo_netto.value

  def Calc_tax(f: Float):Float = f * taxrate

}