package bizz11

object resolver {
  trait Resolver[F[_, _], G[_]] {
    def create[C, S]( f: C => G[S] ): F[C, G[S]]
    def map[C, S, Z]( f: F[C, G[S]] )( g: S => Z ): F[C, G[Z]]
    def flatMap[C, S1, S2, Z]( s1: F[C, G[S1]] )( s2: F[S1, G[S2]] ): F[C, G[S2]]
    def choose[C, Z]( origin: F[C, G[Z]], fallback: F[C, G[Z]] ): F[C, G[Z]]
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
    def map[C, S, Z]( f: Res[C, Option[S]] )( g: S => Z ) =
      Res( c => f.exec( c ).map( g ) )
    def choose[C, Z]( origin: Res[C, Option[Z]], fallback: Res[C, Option[Z]] ) =
      Res( c => origin.exec( c ).orElse( fallback.exec( c ) ) )
    def flatMap[C, S1, S2, Z]( s1: Res[C, Option[S1]] )( s2: Res[S1, Option[S2]] ): Res[C, Option[S2]] =
      Res( c => s1.exec( c ).flatMap( s1 => s2.exec( s1 ) ) )
  }

}

object strings {
  import models._
  import resolver._
  case class Str[C, A]( exec: C => String, value: C => A )
  implicit object ResolverStr extends Resolver[Str, Option] {
    def create[C, S]( f: C => Option[S] ) =
      Str( c => s"${f( c ).map( _.toString ).getOrElse( "None" )}", c => f( c ) )
    def map[C, S, Z]( f: Str[C, Option[S]] )( g: S => Z ) =
      Str( c => s"${f.exec( c )}->${f.value( c ).map( g )}", c => f.value( c ).map( g ) )
    def choose[C, Z]( origin: Str[C, Option[Z]], fallback: Str[C, Option[Z]] ) =
      Str( c => s"(${origin.exec( c )} || ${fallback.exec( c )})", c => origin.value( c ).orElse( fallback.value( c ) ) )
    def flatMap[C, S1, S2, Z]( s1: Str[C, Option[S1]] )( s2: Str[S1, Option[S2]] ): Str[C, Option[S2]] =
      Str( c => s"${s1.value( c )}->${s1.value( c ).flatMap( s1 => s2.value( s1 ) )}", c => s1.value( c ).flatMap( s1 => s2.value( s1 ) ) )
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
    val f1_3 = ( s1: State1 ) => Some( State3() )
    val f3 = ( c: Context ) => Option.empty[State3]

    val l1 = map( create( f1 ) )( P1.process )
    val l1_3 = map( flatMap( create( f1 ) )( create( f1_3 ) ) )( P3.process )
    val l2 = map( create( f2 ) )( P2.process )
    val l3 = map( create( f3 ) )( P3.process )

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
  val g1Res: Option[Response] = program.simpleGraph[Res].exec( context1 )
  println( "Res1: " + g1Res ) // Some(Response(state3))
  val g2Res: Option[Response] = program.simpleGraph[Res].exec( context2 )
  println( "Res2: " + g2Res ) // Some(Response(state2))

  //Str
  val g1Str: String = program.simpleGraph[Str].exec( context1 )
  println( "Str1: " + g1Str ) // (((Some(State1())->Some(State3())->Some(Response(state3)) || State1()->Some(Response(state1))) || None->None) || None->None)
  val g2Str: String = program.simpleGraph[Str].exec( context2 )
  println( "Str2: " + g2Str ) // (((None->None->None || None->None) || State2()->Some(Response(state2))) || None->None)
}
