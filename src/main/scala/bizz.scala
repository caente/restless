/*package bizz1

import ammonite.ops._

trait Found[C, I] {
  type Out
  def found( c: C ): Out
}

object Found {
  type Aux[C, I, O] = Found[C, I] { type Out = O }
}

trait Resolver[C, I, R] {
  def context: C
  def lookup( c: C ): Found[C, I]
  def process( i: Found[C, I] ): R
}

trait Lookup[C, I1, I2] {
  def lookup( c: C ): Either[I1, I2]
}
trait Process[I, R] {
  def process( i: I ): R
}
object real {
  implicit object lookup extends Lookup[Context, State1, State2] {
    def lookup( c: Context ) = if ( c.dialog.isEmpty ) Left( State1() ) else Right( State2() )
  }
  implicit object process1 extends Process[State1, Response] {
    def process( s: State1 ): Response = Response( "state1" )
  }
  implicit object process2 extends Process[State2, Response] {
    def process( s: State2 ): Response = Response( "state2" )
  }
}

object toString {
  implicit object lookup extends Lookup[Context, String, String] {
    def lookup( c: Context ) = if ( c.dialog.isEmpty ) Left( "State1()" ) else Right( "State2()" )
  }
  implicit object process extends Process[String, Response] {
    def process( s: String ): Response = Response( s )
  }
}

object app {
  def res1( c: Context )(
    implicit
    L:  Lookup[Context, State1, State2],
    P1: Process[State1, Response],
    P2: Process[State2, Response]
  ): Response = {
    L.lookup( c ) match {
      case Left( state )  => P1.process( state )
      case Right( state ) => P2.process( state )
    }
  }

  val user1 = User( 1 )
  val user2 = User( 2 )
  val m1 = Message( user1, "user1" )
  val m2 = Message( user2, "user2" )
  val m3 = Message( user1, "more user 1" )
  val users = List( user1, user2 )
  val messages = List( m1, m2, m3 )
  import real._
  println( res1( Context( Nil, messages ) ) )
}
*/
