package bizz7

import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
import ammonite.ops._

object resolver {
  trait Lookup[C, S, A, F[_]] {
    def lookup( c: C, f: C => F[S] ): F[A]
    def success[R]( f: A => F[R] ): F[A] => F[R]
    def failure[R]( f: () => R ): () => R
    def repr( s: S ): A
  }
  trait Process[A, R] {
    def process: A => R
  }
}

object states {
  import models._
  import resolver._
  implicit object lookupState1 extends Lookup[Context, State1, State1, Option] {
    def lookup( c: Context, f: Context => Option[State1] ) = f( c ).map( repr )
    def repr( s: State1 ) = s
    def success[R]( f: State1 => Option[R] ) = fa => fa.flatMap( f )
    def failure[R]( f: () => R ) = () => f()
  }
  implicit object lookupState2 extends Lookup[Context, State2, State2, Option] {
    def lookup( c: Context, f: Context => Option[State2] ) = f( c ).map( repr )
    def repr( s: State2 ) = s
    def success[R]( f: State2 => Option[R] ) = fa => fa.flatMap( f )
    def failure[R]( f: () => R ) = () => f()
  }
  implicit object lookupState3 extends Lookup[State1, State3, State3, Option] {
    def lookup( c: State1, f: State1 => Option[State3] ) = f( c ).map( repr )
    def repr( s: State3 ) = s
    def success[R]( f: State3 => Option[R] ) = fa => fa.flatMap( f )
    def failure[R]( f: () => R ) = () => f()
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
    def lookup( c: Context, f: Context => Option[State1] ) = f( c ).map( repr )
    def repr( s: State1 ) = s.toString
    def success[R]( f: String => Option[R] ) = fa => fa.flatMap( f )
    def failure[R]( f: () => R ) = () => f()

  }
  implicit object lookupState2 extends Lookup[Context, State2, String, Option] {
    def lookup( c: Context, f: Context => Option[State2] ) = f( c ).map( repr )
    def repr( s: State2 ) = s.toString
    def success[R]( f: String => Option[R] ) = fa => fa.flatMap( f )
    def failure[R]( f: () => R ) = () => f()
  }
  implicit object lookupState3 extends Lookup[String, State3, String, Option] {
    def lookup( c: String, f: String => Option[State3] ) = f( c ).map( s => s"$c => ${repr( s )}" )
    def repr( s: State3 ) = s.toString
    def success[R]( f: String => Option[R] ) = fa => fa.flatMap( f )
    def failure[R]( f: () => R ) = () => f()
  }
  implicit object process extends Process[String, String] {
    def process = s => s"Response for $s"
  }
}

object program {
  import models._
  import resolver._
  def graph[S1, S2, S3, R, F[_]](
    c:  Context,
    f1: Context => F[State1],
    f2: Context => F[State2],
    f3: S1 => F[State3]
  )(
    implicit
    L1: Lookup[Context, State1, S1, F],
    L2: Lookup[Context, State2, S2, F],
    L3: Lookup[S1, State3, S3, F],
    P1: Process[S1, R],
    P2: Process[S2, R],
    P3: Process[S3, R]
  ): F[R] = {
    val l1: F[S1] = L1.lookup( c, f1 )
    val l2: () => F[S2] = L1.failure( () => L2.lookup( c, f2 ) )
    val l3: F[S1] => F[S3] = L1.success( s1 => L3.lookup( s1, f3 ) )
    ???
  }
}

object app extends App {
  import models.Context
  import models.values._
  import strings._
  //println( program.graph( Context( users, Nil ) ) )
}
