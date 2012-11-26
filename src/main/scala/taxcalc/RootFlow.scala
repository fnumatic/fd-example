package taxcalc

trait RootFlow{
  import Main._

  //main flow
  ui.netto  >> calc_tax >> ui.print >> ui

  lazy val calc_tax =new CalcTax{
    in  >> sum_netto >> memo_netto >> calc_tax >> calc_brutto >> out
  }

  lazy val ui=new Ui{
    in >> read
    read.succ >> netto
  }

}