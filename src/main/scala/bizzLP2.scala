package bizz6

import ammonite.ops._
import scalaz._, Scalaz._

object resolver {
  trait Lookup[C, S, A, F[_]] {
    def lookup( c: C, f: C => F[S] ): F[A]
    def get( s: S ): A
  }
  trait Process[A, R] {
    def process: A => R
  }
}

object states {
  import models._
  import resolver._
  implicit object lookupState1 extends Lookup[Context, State1, State1, Option] {
    def lookup( c: Context, f: Context => Option[State1] ) = f( c ).map( get )
    def get( s: State1 ) = s
  }
  implicit object lookupState2 extends Lookup[Context, State2, State2, Option] {
    def lookup( c: Context, f: Context => Option[State2] ) = f( c ).map( get )
    def get( s: State2 ) = s
  }
  implicit object lookupState3 extends Lookup[State1, State3, State3, Option] {
    def lookup( c: State1, f: State1 => Option[State3] ) = f( c ).map( get )
    def get( s: State3 ) = s
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

object strings {
  import models._
  import resolver._
  implicit object lookupState1 extends Lookup[Context, State1, String, Option] {
    def lookup( c: Context, f: Context => Option[State1] ) = f( c ).map( get )
    def get( s: State1 ) = s.toString
  }
  implicit object lookupState2 extends Lookup[Context, State2, String, Option] {
    def lookup( c: Context, f: Context => Option[State2] ) = f( c ).map( get )
    def get( s: State2 ) = s.toString
  }
  implicit object lookupState3 extends Lookup[String, State3, String, Option] {
    def lookup( c: String, f: String => Option[State3] ) = f( c ).map( s => s"from $c to ${get( s )}" )
    def get( s: State3 ) = s.toString
  }
  implicit object process extends Process[String, Response] {
    def process = s => Response( s )
  }
}

object program {
  import models._
  import resolver._
  def graph[S1, S2, S3]( c: Context )(
    implicit
    L1: Lookup[Context, State1, S1, Option],
    L2: Lookup[Context, State2, S2, Option],
    L3: Lookup[S1, State3, S3, Option],
    P1: Process[S1, Response],
    P2: Process[S2, Response],
    P3: Process[S3, Response]
  ): Option[Response] = {
    L1.lookup( c, _.dialog.isEmpty.option( State1() ) ) match {
      case Some( s1 ) =>
        L3.lookup( s1, _ => Some( State3() ) ) match {
          case Some( s3 ) => Some( P3.process( s3 ) )
          case None       => Some( P1.process( s1 ) )
        }
      case None => L2.lookup( c, _.dialog.nonEmpty.option( State2() ) ).map( P2.process )
    }
  }
}

object app extends App {
  import models.Context
  import models.values._
  import strings._
  println( program.graph( Context( users, messages ) ) )
}
