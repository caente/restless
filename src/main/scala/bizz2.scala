/*package bizz2

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

trait Lookup[C, I] {
  type R
  def lookup( c: C ): Option[R]
}
object Lookup {
  type Aux[C, I, O] = Lookup[C, I] { type R = O }
}
trait Process[I, R] {
  def process( i: I ): R
}

case class User( id: Int )
case class Message( from: User, content: String )
case class Context( users: List[User], dialog: List[Message] )

case class State1()
case class State2()
case class State3()

case class Response( r: String )

object real {
  implicit def lookupState1 =
    new Lookup[Context, State1] {
      type R = State1
      def lookup( c: Context ) = if ( c.dialog.isEmpty ) Some( State1() ) else None
    }
  implicit def lookupState2 =
    new Lookup[Context, State2] {
      type R = State2
      def lookup( c: Context ) = if ( c.dialog.isEmpty ) None else Some( State2() )
    }
  implicit object process1 extends Process[State1, Response] {
    def process( s: State1 ): Response = Response( "state1" )
  }
  implicit object process2 extends Process[State2, Response] {
    def process( s: State2 ): Response = Response( "state2" )
  }
}

object string {
  implicit def lookupState1 =
    new Lookup[Context, State1] {
      type R = String
      def lookup( c: Context ) = if ( c.dialog.isEmpty ) Some( "State1()" ) else Some( "None" )
    }
  implicit def lookupState2 =
    new Lookup[Context, State2] {
      type R = String
      def lookup( c: Context ) = if ( c.dialog.isEmpty ) Some( "None" ) else Some( "State2()" )
    }
  implicit object process extends Process[String, Response] {
    def process( s: String ): Response = Response( "state2" )
  }
}

object app {
  def res1[S1, S2]( c: Context )(
    implicit
    L1: Lookup.Aux[Context, State1, S1],
    L2: Lookup.Aux[Context, State2, S2],
    P1: Process[S1, Response],
    P2: Process[S2, Response]
  ): Option[Response] = {
    L1.lookup( c ).map( P1.process ).orElse( L2.lookup( c ).map( P2.process ) )
  }

  val user1 = User( 1 )
  val user2 = User( 2 )
  val m1 = Message( user1, "user1" )
  val m2 = Message( user2, "user2" )
  val m3 = Message( user1, "more user 1" )
  val users = List( user1, user2 )
  val messages = List( m1, m2, m3 )
  import string._
  println( res1( Context( Nil, messages ) ) )
}
*/
