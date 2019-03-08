package models

case class User( id: Int )
case class Message( from: User, content: String )
case class Context( users: List[User], dialog: List[Message] )

case class State1( usersEmpty: Boolean )
case class State2()
case class State3()

case class Response( r: String )

object values {
  val user1 = User( 1 )
  val user2 = User( 2 )
  val m1 = Message( user1, "user1" )
  val m2 = Message( user2, "user2" )
  val m3 = Message( user1, "more user 1" )
  val users = List( user1, user2 )
  val messages = List( m1, m2, m3 )
}
