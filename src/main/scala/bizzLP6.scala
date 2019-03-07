package bizz10

object resolver {
  trait Resolver[F[_, _], G[_]] {
    def create[C, S]( f: C => G[S] ): F[C, G[S]]
    def transition[C, S, Z]( f: F[C, G[S]] )( g: S => Z ): F[C, G[Z]]
    def choose[C, Z](
      origin:   F[C, G[Z]],
      fallback: F[C, G[Z]]
    ): F[C, G[Z]]
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
  case class Res[C, A]( exec: C => A )
  implicit object ResolverRes extends Resolver[Res, Option] {
    def create[C, S]( f: C => Option[S] ): Res[C, Option[S]] = Res( f )
    def transition[C, S, Z]( f: Res[C, Option[S]] )( g: S => Z ): Res[C, Option[Z]] = Res( c => f.exec( c ).map( g ) )
    def choose[C, Z](
      origin:   Res[C, Option[Z]],
      fallback: Res[C, Option[Z]]
    ): Res[C, Option[Z]] = Res( c => origin.exec( c ).orElse( fallback.exec( c ) ) )
  }

}

object strings {
  import models._
  import resolver._
  case class Str[C, A]( exec: C => String )
  implicit object ResolverStr extends Resolver[Str, Option] {
    def create[C, S]( f: C => Option[S] ): Str[C, Option[S]] = Str( c => s"${f( c ).map( _.toString ).getOrElse( "None" )}" )
    def transition[C, S, Z]( f: Str[C, Option[S]] )( g: S => Z ): Str[C, Option[Z]] = {
      Str( c => s"${f.exec( c )}" )
    }
    def choose[C, Z](
      origin:   Str[C, Option[Z]],
      fallback: Str[C, Option[Z]]
    ): Str[C, Option[Z]] = {
      Str( c => s"(${origin.exec( c )} || ${fallback.exec( c )})" )
    }
  }

}

object program {
  import models._
  import resolver._
  def simpleGraph[F[_, _]](
    f1: Context => Option[State1],
    f2: Context => Option[State2]
  )(
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response]
  ): F[Context, Option[Response]] = {
    import L._
    val l1 = transition( create( f1 ) )( P1.process )
    val l2 = transition( create( f2 ) )( P2.process )
    val l3 = transition( choose( l1, l2 ) )( identity )
    choose( l1, l3 )
  }

  import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean
  def createSimpleGraph[F[_, _]](
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response]
  ) =
    program.simpleGraph[F](
      f1 = ( c: Context ) => c.dialog.isEmpty.option( State1() ),
      f2 = ( c: Context ) => c.dialog.nonEmpty.option( State2() )
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
  val g1Res = program.createSimpleGraph[Res].exec( context1 )
  println( "Res1: " + g1Res ) // Some(Response(state1))
  val g2Res = program.createSimpleGraph[Res].exec( context2 )
  println( "Res2: " + g2Res ) // Some(Response(state2))

  //Str
  val g1Str = program.createSimpleGraph[Str].exec( context1 )
  println( "Str1: " + g1Str ) // (State1() || (State1() || None))
  val g2Str = program.createSimpleGraph[Str].exec( context2 )
  println( "Str2: " + g2Str ) // (None || (None || State2()))
}
