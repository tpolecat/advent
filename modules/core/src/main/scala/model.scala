package muck

import scalaz._

sealed trait TravelCondition
case object Unconditional                           extends TravelCondition
case class  Probabilistic(pct: Int)                 extends TravelCondition
case class  NeedsItem(id: Int)                      extends TravelCondition
case class  PropDependent(prop: Int, cannotBe: Int) extends TravelCondition

object TravelCondition {
  def decode(m: Int): TravelCondition =
         if (m == 0)  Unconditional
    else if (m < 100) Probabilistic(m)
    else if (m < 200) NeedsItem(m - 100)
    else              PropDependent(m % 100, (m / 100) - 3)
}

case class Travel(from: Int, to: Int, condition: TravelCondition, motions: List[Int])
object Travel {
  def decode(x: Int, y: Int, motions: List[Int]): Travel =
    Travel(x, y % 1000, TravelCondition.decode(y / 1000), motions)
}

sealed trait LocationF[A]

case class Room[A](
  id: Int,
  longDescription: String,
  shortDescription: Option[String],
  travel: List[(A, TravelCondition, List[String])]
) extends LocationF[A]

case class Special[A](id: Int)     extends LocationF[A]
case class Message[A](txt: String) extends LocationF[A]

object LocationF {
  implicit val LocationFFunctor: Functor[LocationF] =
    new Functor[LocationF] {
      def map[A, B](fa: LocationF[A])(f: A => B): LocationF[B] =
        fa match {
          case Room(id, l, s, t) => Room(id, l, s, t.map { case (a, c, m) => (f(a), c, m) })
          case Special(n)    => Special(n)
          case Message(t)    => Message(t)
        }
    }
}

case class Item(inventoryDesc: String, state: Int, descs: List[String], location: ItemLocation) {
  def roomDesc: String = descs.lift(state).getOrElse(sys.error(s"item '$inventoryDesc' is in state $state with no room desc."))
}

sealed trait ItemLocation {
  def movable: Boolean
  def inRoom(id: Int): Boolean
}
object ItemLocation {
  case class InRoom(id: Int, movable: Boolean) extends ItemLocation {
    def inRoom(id: Int) = id == this.id
  }
  case class InTwoRooms(id1: Int, id2: Int) extends ItemLocation {
    val movable = false
    def inRoom(id: Int) = id == id1 || id == id2
  }
  case object Held extends ItemLocation {
    val movable = true
    def inRoom(id: Int) = false
  }
}





case class GameState(
  room: Room[Location],
  back: Option[Room[Location]],
  items: List[Item]
) {

  def move(to: Room[Location]) =
    copy(room = to, back = Some(room))

  def itemsInRoom: List[Item] =
    items.filter(_.location.inRoom(room.id))

  def itemsHeld: List[Item] =
    items.filter(_.location == ItemLocation.Held)

}
