package bizz9

import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
import ammonite.ops._

object resolver {
  trait Resolver[F[_], G[_]] {
    def create[S]( f: () => G[S] ): F[() => G[S]]
    def transition[S, Z]( f: F[() => G[S]] )( g: S => Z ): F[() => G[Z]]
    def choose[Z](
      origin:   F[() => G[Z]],
      fallback: F[() => G[Z]]
    ): F[() => G[Z]]
  }
  trait Process[A, R] {
    def process: A => R
  }

  object Process {
    import models._
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
}

object states {
  import models._
  import resolver._
  case class Res[A]( res: A )
  implicit object ResolverRes extends Resolver[Res, Option] {
    def create[S]( f: () => Option[S] ): Res[() => Option[S]] = Res( f )
    def transition[S, Z]( f: Res[() => Option[S]] )( g: S => Z ): Res[() => Option[Z]] = Res( () => f.res().map( g ) )
    def choose[Z](
      origin:   Res[() => Option[Z]],
      fallback: Res[() => Option[Z]]
    ): Res[() => Option[Z]] = Res( () => origin.res().orElse( fallback.res() ) )
  }

}

object strings {
  import models._
  import resolver._
  case class Str[A]( res: String )
  implicit object ResolverStr extends Resolver[Str, Option] {
    def create[S]( f: () => Option[S] ): Str[() => Option[S]] = Str( s"created(${f()})" )
    def transition[S, Z]( f: Str[() => Option[S]] )( g: S => Z ): Str[() => Option[Z]] = {
      Str( s"${f.res}" )
    }
    def choose[Z](
      origin:   Str[() => Option[Z]],
      fallback: Str[() => Option[Z]]
    ): Str[() => Option[Z]] = {
      Str( s"${origin.res} || ${fallback.res}" )
    }
  }

}

object program {
  import models._
  import resolver._
  def simpleGraph[F[_]](
    f1: () => Option[State1],
    f2: () => Option[State2]
  )(
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response]
  ): F[() => Option[Response]] = {
    import L._
    val l1 = transition( create( f1 ) )( P1.process )
    val l2 = transition( create( f2 ) )( P2.process )
    val l3 = transition( choose( l1, l2 ) )( identity )
    choose( l1, l3 )
  }
  def createSimpleGraph[F[_]]( c: Context )(
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response]
  ) =
    program.simpleGraph[F](
      f1 = () => c.dialog.isEmpty.option( State1() ),
      f2 = () => c.dialog.nonEmpty.option( State2() )
    )
}

object app extends App {
  import models._
  import models.values._
  import strings._
  import states._

  val context1 = Context( users, Nil )
  val context2 = Context( users, messages )

  //Res
  val g1Res = program.createSimpleGraph[Res]( context1 )
  println( "Res1: " + g1Res.res() ) // Some(Response(state1))
  val g2Res = program.createSimpleGraph[Res]( context2 )
  println( "Res2: " + g2Res.res() ) // Some(Response(state2))

  //Str
  val g1Str = program.createSimpleGraph[Str]( context1 )
  println( "Str1: " + g1Str.res ) // created(Some(State1())) || created(Some(State1())) || created(None)
  val g2Str = program.createSimpleGraph[Str]( context2 )
  println( "Str2: " + g2Str.res ) // created(None) || created(None) || created(Some(State2()))
}
