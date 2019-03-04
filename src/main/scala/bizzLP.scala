package bizz5

import ammonite.ops._

object resolver {
  trait Lookup[C, S, F[_]] {
    def lookup: C => F[S]
  }
  trait Process[S, R] {
    def process: S => R
  }
}

object states {
  import models._
  import resolver._
  implicit object lookupState1 extends Lookup[Context, State1, Option] {
    def lookup = c => Some( State1() )
  }
  implicit object lookupState2 extends Lookup[Context, State2, Option] {
    def lookup = c => Some( State2() )
  }
  implicit object lookupState3 extends Lookup[State1, State3, Option] {
    def lookup = c => Some( State3() )
  }
  implicit object process1 extends Process[State1, Response] {
    def process = s => Response( "state1" )
  }
  implicit object process2 extends Process[State2, Response] {
    def process = s => Response( "state2" )
  }
  implicit object process3 extends Process[State3, Response] {
    def process = s => Response( "state3" )
  }
}
/*
object strings {
  import models._
  import resolver._
  implicit object lookupState1 extends Lookup[Context, State1, Option] {
    def lookup = c => Some( State1() )
  }
  implicit object lookupState2 extends Lookup[Context, State2, Option] {
    def lookup = c => Some( State2() )
  }
  implicit object lookupState3 extends Lookup[State1, State3, Option] {
    def lookup = c => Some( State3() )
  }
  implicit object process1 extends Process[State1, Response] {
    def process = s => Response( "state1" )
  }
  implicit object process2 extends Process[State2, Response] {
    def process = s => Response( "state2" )
  }
  implicit object process3 extends Process[State3, Response] {
    def process = s => Response( "state3" )
  }
}
*/

object program {
  import models._
  import resolver._
  def graph( c: Context )(
    implicit
    L1: Lookup[Context, State1, Option],
    L2: Lookup[Context, State2, Option],
    L3: Lookup[State1, State3, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response],
    P3: Process[State3, Response]
  ): Option[Response] = {
    L1.lookup( c ) match {
      case Some( s1 ) =>
        L3.lookup( s1 ) match {
          case Some( s3 ) => Some( P3.process( s3 ) )
          case None       => Some( P1.process( s1 ) )
        }
      case None => L2.lookup( c ).map( P2.process )
    }
  }
}

object app {
  import models.Context
  import models.values._
  import states._
  println( program.graph( Context( users, messages ) ) )
}
