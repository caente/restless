package bizz8

import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
import ammonite.ops._

sealed trait Res[A]
case class Fail[A]() extends Res[A]
case class Suc[A]( a: A ) extends Res[A]

object resolver {
  trait Resolver[F[_], G[_]] {
    def create[S]( f: G[S] ): F[G[S]]
    def transition[S, Z]( f: F[G[S]] )( g: S => Z ): F[G[Z]]
    def choose[Z](
      origin:   F[G[Z]],
      fallback: F[G[Z]]
    ): F[G[Z]]
  }
  trait Process[A, R] {
    def process: A => R
  }
}
object states {
  import models._
  import resolver._
  case class Res[A]( res: A )
  implicit object lookupState extends Resolver[Res, Option] {
    def create[S]( f: Option[S] ): Res[Option[S]] = Res( f )
    def transition[S, Z]( f: Res[Option[S]] )( g: S => Z ): Res[Option[Z]] = Res( f.res.map( g ) )
    def choose[Z](
      origin:   Res[Option[Z]],
      fallback: Res[Option[Z]]
    ): Res[Option[Z]] = Res( origin.res.orElse( fallback.res ) )
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
  case class Str[A]( str: String )
  implicit object lookupState extends Resolver[Str, Option] {
    def create[S]( f: Option[S] ): Str[Option[S]] = Str( s"created(${f})" )
    def transition[S, Z]( f: Str[Option[S]] )( g: S => Z ): Str[Option[Z]] = {
      Str( s"g(${f.str})" )
    }
    def choose[Z](
      origin:   Str[Option[Z]],
      fallback: Str[Option[Z]]
    ): Str[Option[Z]] = {
      Str( s"${origin.str} || ${fallback.str}" )
    }
  }
  implicit object process2 extends Process[State1, Response] {
    def process = s => Response( "state2" )
  }
  implicit object process3 extends Process[State2, Response] {
    def process = s => Response( "state3" )
  }
}

object program {
  import models._
  import resolver._
  import scalaz._, Scalaz._
  def graph[F[_]](
    f1: Option[State1],
    f2: Option[State2]
  )(
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response]
  ): F[Option[Response]] = {
    import L._
    val l1 = transition( create( f1 ) )( P1.process )
    val l2 = transition( create( f2 ) )( P2.process )
    val l3 = transition( choose( l1, l2 ) )( identity )
    choose( l1, l3 )
  }
}

object app {
  import models._
  import models.values._
  import strings._
  val context = Context( users, Nil )
  val g = program.graph(
    context.dialog.isEmpty.option( State1() ),
    context.dialog.nonEmpty.option( State2() )
  )
  println( g )
}
