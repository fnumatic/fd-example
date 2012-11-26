package taxcalc

trait RootFlow{

  //main flow
  Main.start >> read_netto
  read_netto.succ >> calc_tax >> show_result >> read_netto
  read_netto.err >> Main.exit

  lazy val calc_tax =new CalcTax{
    in  >> sum_netto >> memo_netto >> calc_tax >> calc_brutto >> out
  }

  lazy val show_result = new ShowResult

  lazy val read_netto = new ReadNetto


}