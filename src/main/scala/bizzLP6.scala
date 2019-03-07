package bizz10

object resolver {
  trait Resolver[F[_, _], G[_]] {
    def create[C, S]( f: C => G[S] ): F[C, G[S]]
    def transition[C, S, Z]( f: F[C, G[S]] )( g: S => Z ): F[C, G[Z]]
    def choose[C, Z]( origin: F[C, G[Z]], fallback: F[C, G[Z]] ): F[C, G[Z]]
    def link[C, S1, S2, Z]( s1: F[C, G[S1]], s2: F[C => G[S1], G[C => G[S2]]] ): F[C, G[S2]]
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
    def create[C, S]( f: C => Option[S] ) =
      Res( f )
    def transition[C, S, Z]( f: Res[C, Option[S]] )( g: S => Z ) =
      Res( c => f.exec( c ).map( g ) )
    def choose[C, Z]( origin: Res[C, Option[Z]], fallback: Res[C, Option[Z]] ) =
      Res( c => origin.exec( c ).orElse( fallback.exec( c ) ) )
    def link[C, S1, S2, Z]( s1: Res[C, Option[S1]], s2: Res[C => Option[S1], Option[C => Option[S2]]] ): Res[C, Option[S2]] =
      Res( c => s2.exec( s1.exec ).flatMap( _( c ) ) )
  }

}

object strings {
  import models._
  import resolver._
  case class Str[C, A]( exec: C => String )
  implicit object ResolverStr extends Resolver[Str, Option] {
    def create[C, S]( f: C => Option[S] ) =
      Str( c => s"${f( c ).map( _.toString ).getOrElse( "None" )}" )
    def transition[C, S, Z]( f: Str[C, Option[S]] )( g: S => Z ) =
      Str( c => s"${f.exec( c )}" )
    def choose[C, Z]( origin: Str[C, Option[Z]], fallback: Str[C, Option[Z]] ) =
      Str( c => s"(${origin.exec( c )} || ${fallback.exec( c )})" )
    def link[C, S1, S2, Z]( s1: Str[C, Option[S1]], s2: Str[C => Option[S1], Option[C => Option[S2]]] ): Str[C, Option[S2]] =
      Str( c => s"${s1.exec( c )}->${s2.exec( _ => None )}" )
  }
}

object program {
  import models._
  import resolver._
  import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean

  def simpleGraph[F[_, _]](
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response],
    P3: Process[State3, Response]
  ): F[Context, Option[Response]] = {
    import L._
    val f1 = ( c: Context ) => c.dialog.isEmpty.option( State1() )
    val f2 = ( c: Context ) => c.dialog.nonEmpty.option( State2() )
    val f1_3: ( Context => Option[State1] ) => Option[( Context => Option[State3] )] =
      s1 => Some( c => s1( c ).map( _ => State3() ) )
    val f3: Context => Option[State3] = ( c: Context ) => Option.empty[State3]

    val l1 = transition( create( f1 ) )( P1.process )
    val l1_3 = transition( link( create( f1 ), create( f1_3 ) ) )( P3.process )
    val l2 = transition( create( f2 ) )( P2.process )
    val l3 = transition( create( f3 ) )( P3.process )

    val branch1 = choose( l1_3, l1 )
    val branch2 = choose( branch1, l2 )
    choose( branch2, l3 )
  }

}

object app extends App {
  import models._
  import models.values._
  import strings._
  import states._

  val context1 = Context( users, Nil )
  val context2 = Context( users, messages )

  //Res
  val g1Res = program.simpleGraph[Res].exec( context1 )
  println( "Res1: " + g1Res ) // Some(Response(state1))
  val g2Res = program.simpleGraph[Res].exec( context2 )
  println( "Res2: " + g2Res ) // Some(Response(state2))

  //Str
  val g1Str = program.simpleGraph[Str].exec( context1 )
  println( "Str1: " + g1Str ) // (State1() || (State1() || None))
  val g2Str = program.simpleGraph[Str].exec( context2 )
  println( "Str2: " + g2Str ) // (None || (None || State2()))
}
