package fnumatic.ebc

class FuConvert[T, R](f: T => R) {
  def tofu = FuConvert.fu(f)
}

object FuConvert {
  def fu[T, R](f: T => R)(implicit dbgname:String="") = new FuncUnit[T, R] {
      inpin.dbgname=dbgname

      def process(t:  T) = {
        f(t)
      }

    }

    def ex[T](f: T => Boolean) = new Exit[T] {
      def toExit(t: T) = f(t)

    }

   def get[T]()=fu({ o:Option[T] => o.getOrElse(null.asInstanceOf[T]) })
   def get2[T](implicit dbgname:String="")=fu{ t:T => t }

  implicit def rich2[T, R](f: T => R): FuConvert[T, R] = new FuConvert[T, R](f)

}