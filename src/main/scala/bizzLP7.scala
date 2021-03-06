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
  implicit def ResolverStr( implicit R: Resolver[states.Res, Option] ) = new Resolver[Str, Option] {
    def create[C, S]( f: C => Option[S] ) =
      Str( c => s"${f( c ).map( _.toString ).getOrElse( "None" )}", c => R.create( f ).exec( c ) )
    def map[C, S, Z]( f: Str[C, Option[S]] )( g: S => Z ) =
      Str( c => s"${f.exec( c )}->${f.value( c ).map( g )}", c => R.map( R.create( f.value ) )( g ).exec( c ) )
    def choose[C, Z]( origin: Str[C, Option[Z]], fallback: Str[C, Option[Z]] ) =
      Str( c => s"(${origin.exec( c )} || ${fallback.exec( c )})", c => R.choose( R.create( origin.value ), R.create( fallback.value ) ).exec( c ) )
    def flatMap[C, S1, S2, Z]( s1: Str[C, Option[S1]] )( s2: Str[S1, Option[S2]] ): Str[C, Option[S2]] =
      Str(
        c => s"${s1.value( c )}->${s1.value( c ).flatMap( s1 => s2.value( s1 ) )}",
        c => R.flatMap( R.create( s1.value ) )( R.create( s2.value ) ).exec( c )
      )
  }
}

object syntax {
  import resolver._
  implicit class ResolverSyntax[F[_, _], G[_], C, S]( f: F[C, G[S]] ) {
    def map[Z]( g: S => Z )( implicit R: Resolver[F, G] ): F[C, G[Z]] = R.map( f )( g )
    def flatMap[S2, Z]( s2: F[S, G[S2]] )( implicit R: Resolver[F, G] ): F[C, G[S2]] = R.flatMap( f )( s2 )
    def fallbackWith( fallback: F[C, G[S]] )( implicit R: Resolver[F, G] ): F[C, G[S]] = R.choose( f, fallback )
  }
}
object program {
  import models._
  import resolver._
  import syntax._
  import scalaz.syntax.std.boolean.ToBooleanOpsFromBoolean

  def simpleGraph[F[_, _]](
    implicit
    L:  Resolver[F, Option],
    P1: Process[State1, Response],
    P2: Process[State2, Response],
    P3: Process[State3, Response]
  ): F[Context, Option[Response]] = {
    import L._
    val f1 = ( c: Context ) => c.dialog.isEmpty.option( State1( c.users.isEmpty ) )
    val f2 = ( c: Context ) => c.dialog.nonEmpty.option( State2() )
    val f1_3 = ( s1: State1 ) => s1.usersEmpty.option( State3() )
    val f3 = ( c: Context ) => Option.empty[State3]

    val l1 = create( f1 ).map( P1.process )
    val l1_3 = create( f1 ).flatMap( create( f1_3 ) ).map( P3.process )
    val l2 = create( f2 ).map( P2.process )
    val l3 = create( f3 ).map( P3.process )
    //all lx are of the same type F[Context,Option[Response]]
    //the below can be combined in any way, e.g.
    //l1_3.fallbackWith( l1.fallbackWith( l2.fallbackWith( l3 ) ) )
    l1_3
      .fallbackWith( l1 )
      .fallbackWith( l2 )
      .fallbackWith( l3 )
  }

}

object app {
  import models._
  import models.values._
  import strings._
  import states._

  val context1 = Context( Nil, Nil )
  val context2 = Context( users, messages )
  val context3 = Context( users, Nil )

  //Res
  val g1Res: Option[Response] = program.simpleGraph[Res].exec( context1 )
  println( "Res1: " + g1Res ) // Some(Response(state3))
  val g2Res: Option[Response] = program.simpleGraph[Res].exec( context2 )
  println( "Res2: " + g2Res ) // Some(Response(state2))
  val g3Res: Option[Response] = program.simpleGraph[Res].exec( context3 )
  println( "Res3: " + g3Res ) // Some(Response(state1))

  //Str
  val g1Str: String = program.simpleGraph[Str].exec( context1 )
  println( "Str1: " + g1Str )
  // (((Some(State1(true))->Some(State3())->Some(Response(state3)) || State1(true)->Some(Response(state1))) || None->None) || None->None)
  val g2Str: String = program.simpleGraph[Str].exec( context2 )
  println( "Str2: " + g2Str )
  // (((None->None->None || None->None) || State2()->Some(Response(state2))) || None->None)
  val g3Str: String = program.simpleGraph[Str].exec( context3 )
  println( "Str3: " + g3Str )
  // (((Some(State1(false))->None->None || State1(false)->Some(Response(state1))) || None->None) || None->None)
}
