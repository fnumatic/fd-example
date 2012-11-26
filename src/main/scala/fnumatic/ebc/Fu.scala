package fnumatic.ebc

import reactive.{ EventStream, EventSource, Observing}
import actors.{IScheduler, Actor}
import fnumatic.ebc.FuConvert._
import java.util.UUID
import actors.scheduler.SingleThreadedScheduler
import scala.actors.Futures._
import scala.util.continuations._
import annotation.tailrec

object FUScheduler{
 def default=new SingleThreadedScheduler()
}

trait Dbg{
  var dbgname=""
}

trait EventsourceStrategy[T] extends Dbg{
  self: EventSource[T] =>

  def act: EventStream[T] = eact(_ => false)

  def eact(toExit: T => Boolean): EventStream[T] = new SimpleStrategy[T]()

  class SimpleStrategy[T] extends ChildEventSource[T, Unit] {
    def handler = {
      case (parentEvent, _) => {
            fire(parentEvent.asInstanceOf[T])
        }
      }
    }


  class DelimitedContStrategy[T] extends ChildEventSource[T, Unit] {

    def handler = {
      case (parentEvent, _) => {
        reset {
          shift {
            (cont: Unit => Unit) => {
             fire(parentEvent.asInstanceOf[T])
            }
          }
        }
      }
    }
  }

  class ActorStrategy[T] extends ChildEventSource[T, Unit] {

    import scala.actors.Actor._

    private def delegate = actor {
      react {
        case x: T => {
          reply(fire(x))
        }
      }
    }

    def handler = {
      case (parentEvent, _) => {
        val res = delegate ! ( parentEvent )
      }
    }
  }

}



object dbginc1 {
  var i = 0

  def inci = {
    i += 1
    i
  }
}

trait Fu[T, R]  {
  implicit val observing=new Observing{}
  val inpin = new EventSource[T] with Dbg with EventsourceStrategy[T] {}
  val outpin = new EventSource[R]

  def >>[X ](target: Fu[R, X]) ={
    outpin >> target.inpin
     target
  }

  def apply(v:T) = inpin.fire(v)

}

trait In[T] {
 self: Fu[T,_]=>
  val in =  new Fu[Unit,T]{}
  inpin.act >> in.outpin
}

trait Out[R] {
  self: Fu[_,R]=>
  val out = new Fu[R,Unit]{}
  out.inpin >> outpin
}

trait InOut2[T,U] extends Fu[T,U] with In[T] with Out[U]

trait Board[T, U] extends InOut2[T,U]

trait Processing[T,U]{
  def process(i:  T): U
}

trait FuncUnit[T, U] extends Fu[T, U] with Processing[T,U]{
    inpin.map(process) >> outpin
}

trait Split[T] extends Fu[T,T] with In[T]{
  inpin =>> (split)
  def split(t:T)
}

case class Store[T](zero: T) extends Fu[T, T] {

  val id=UUID.randomUUID()
  val v = outpin.hold(zero)
  inpin >> outpin

  def value = v.now
}

trait  Exit[T] extends Fu[T, T] {

  //inpin.eact(toExit _).foreach{ outpin.fire}
  inpin.takeWhile(holdOn _) >> outpin

  def toExit(t:T):Boolean

  def holdOn(t:T) =  ! toExit(t)

}

