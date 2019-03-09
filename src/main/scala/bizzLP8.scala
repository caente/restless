package bizz12

import syntax._

trait Resolver[F[_, _], G[_]] {
  def create[C, S]( f: C => G[S] ): F[C, G[S]]
  def map[C, S, Z]( f: F[C, G[S]] )( g: S => Z ): F[C, G[Z]]
  def flatMap[C, S1, S2, Z]( s1: F[C, G[S1]] )( s2: F[S1, G[S2]] ): F[C, G[S2]]
  def choose[C, Z]( origin: F[C, G[Z]], fallback: F[C, G[Z]] ): F[C, G[Z]]
}

case class Res[C, A]( exec: C => A )
case class Str[C, A]( exec: C => String, value: C => A )

object Resolver {
  def Composed[F[_, _], G[_], C, S, Z]( f: C => G[S], g: S => Z )( implicit R: Resolver[F, G] ) =
    R.create( f ).map( g )

  def Flatten[F[_, _], G[_], C, S1, S2, Z]( s1: C => G[S1], s2: S1 => G[S2] )( implicit R: Resolver[F, G] ) =
    R.create( s1 ).flatMap( R.create( s2 ) )

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

  implicit def ResolverStr( implicit R: Resolver[Res, Option] ) = new Resolver[Str, Option] {
    def create[C, S]( f: C => Option[S] ) =
      Str(
        c => s"${f( c ).map( _.toString ).getOrElse( "None" )}",
        R.create( f ).exec
      )
    def map[C, S, Z]( f: Str[C, Option[S]] )( g: S => Z ) = {
      Str(
        c => s"${f.exec( c )}->${create( Composed( f.value, g ).exec ).exec( c )}",
        Composed( f.value, g ).exec
      )
    }
    def choose[C, Z]( origin: Str[C, Option[Z]], fallback: Str[C, Option[Z]] ) =
      Str(
        c => s"[${origin.exec( c )} || ${fallback.exec( c )}]",
        R.choose( R.create( origin.value ), R.create( fallback.value ) ).exec
      )
    def flatMap[C, S1, S2, Z]( s1: Str[C, Option[S1]] )( s2: Str[S1, Option[S2]] ): Str[C, Option[S2]] = {
      Str(
        c => s"${s1.exec( c )}->${create( Flatten( s1.value, s2.value ).exec ).exec( c )}",
        Flatten( s1.value, s2.value ).exec
      )
    }
  }
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

object syntax {
  implicit class ResolverSyntax[F[_, _], G[_], C, S]( f: F[C, G[S]] ) {
    def map[Z]( g: S => Z )( implicit R: Resolver[F, G] ): F[C, G[Z]] =
      R.map( f )( g )
    def flatMap[S2, Z]( s2: F[S, G[S2]] )( implicit R: Resolver[F, G] ): F[C, G[S2]] =
      R.flatMap( f )( s2 )
    def oElse( fallback: F[C, G[S]] )( implicit R: Resolver[F, G] ): F[C, G[S]] =
      R.choose( f, fallback )
  }
}

object program {
  import models._
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
    //all lx are of the same type F[Context,Option[Response]]
    //the below can be combined in any way, e.g.
    //l1_3.oElse( l1.oElse( l2 ) )
    l1_3
      .oElse( l1 )
      .oElse( l2 )
  }

}

object app extends App {
  import models._
  import models.values._

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
  // [[State1(true)->State3()->Response(state3) || State1(true)->Response(state1)] || None->None]
  val g2Str: String = program.simpleGraph[Str].exec( context2 )
  println( "Str2: " + g2Str )
  // [[None->None->None || None->None] || State2()->Response(state2)]
  val g3Str: String = program.simpleGraph[Str].exec( context3 )
  println( "Str3: " + g3Str )
  // [[State1(false)->None->None || State1(false)->Response(state1)] || None->None]

}
