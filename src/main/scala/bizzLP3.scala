package bizz7

import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
import ammonite.ops._

object resolver {
  trait Resolver[F[_], G[_]] {
    def create[C, S]( f: C => G[S] ): F[C => G[S]]
    def transition[C, S, Z]( f: F[C => G[S]] )( g: S => Z ): F[C => G[Z]]
    def choose[C, S, S1, S2, Z](
      origin:   F[C => G[Z]],
      fallback: F[C => G[Z]]
    ): F[C => G[Z]]
    def apply[C, Z]( c: C, f: F[C => G[Z]] ): F[G[Z]]
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
    def create[C, S]( f: C => Option[S] ): Res[C => Option[S]] = Res( f )
    def transition[C, S, Z]( f: Res[C => Option[S]] )( g: S => Z ): Res[C => Option[Z]] = Res( c => f.res( c ).map( g ) )
    def choose[C, S, S1, S2, Z](
      origin:   Res[C => Option[Z]],
      fallback: Res[C => Option[Z]]
    ): Res[C => Option[Z]] = Res( c => origin.res( c ).orElse( fallback.res( c ) ) )
    def apply[C, Z]( c: C, f: Res[C => Option[Z]] ): Res[Option[Z]] = Res( f.res( c ) )
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
    def create[C, S]( f: C => Option[S] ): Str[C => Option[S]] = Str( "created" )
    def transition[C, S, Z]( f: Str[C => Option[S]] )( g: S => Z ): Str[C => Option[Z]] = {
      Str( s"g(${f.str})" )
    }
    def choose[C, S, S1, S2, Z](
      origin:   Str[C => Option[Z]],
      fallback: Str[C => Option[Z]]
    ): Str[C => Option[Z]] = {
      Str( s"${origin.str} || ${fallback.str}" )
    }
    def apply[C, Z]( c: C, f: Str[C => Option[Z]] ): Str[Option[Z]] = {
      Str( s"${f.str}(c)" )
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
  def graph[F[_]]( c: Context )(
    f1: Context => Option[State1],
    f2: Context => Option[State2]
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
    val choice = choose( l1, l3 )
    apply( c, choice )
  }
}

object app extends App {
  import models._
  import models.values._
  import strings._
  val g = program.graph( Context( users, messages ) )(
    f1 = _.dialog.isEmpty.option( State1() ),
    f2 = _.dialog.nonEmpty.option( State2() )
  )
  println( g )
}
