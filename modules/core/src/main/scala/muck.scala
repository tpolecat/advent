package muck

import scalaz._, Scalaz.{ char => _, _ }, scalaz.effect._, IO._
import atto._, Atto._, compat.scalaz._
import scala.io.Source

// import matryoshka._
import matryoshka.implicits._
// import matryoshka.data._
// import matryoshka.patterns._

// object Main2 {
//
//   sealed trait AdventureOp[A]
//   case class  Say(s: String)            extends AdventureOp[Unit]
//   case object ItemsInRoom               extends AdventureOp[List[Item]]
//   case object Travels                   extends AdventureOp[List[Travel]]
//   case object Room                      extends AdventureOp[Room]
//   case object PreviousRoom              extends AdventureOp[Option[Room]] // TODO: Option[Travel]
//   case class  SetRoom(r: Room[Location]) extends AdventureOp[Unit]
//   case object GetInput                  extends AdventureOp[String]
//
//
//   val describe: AdventureIO[Unit] =
//     room.map(_.longDescription).flatMap(say) *>
//     itemsInRoom.map(_.roomDesc).flatMap(say) *> interact
//
//   val back: AdventureIO[Unit] =
//     previousRoom flatMap {
//       case None    => say("BUT YOU JUST GOT HERE!") *> interact
//       case Some(b) => moveTo(b)
//     }
//
//   val moveTo(r: Room[Location]): AdventureIO[Unit] =
//     setRoom(r) *> arrive
//
//   val arrive: AdventureIO[Unit] =
//     describe *> interact
//
// }


object Main extends SafeApp {



  def describe(gs: GameState): IO[Unit] =
    putStrLn(gs.room.longDescription) *>
    gs.itemsInRoom.traverse(i => putStrLn(i.roomDesc)) *>
    gs.room.travel.traverse { case (_, _, vs) =>
      putStrLn(vs.mkString(" - ", " ", ""))
    } *> ioUnit

  def getInput: IO[String] =
    putStr("\n> ") *> readLn.map(s => Option(s).fold("QUIT")(_.trim.toUpperCase)) <* putStrLn("")

  def back(gs: GameState): IO[Unit] =
    gs.back match {
      case None    => putStrLn("BUT YOU JUST GOT HERE!") *> interact(gs)
      case Some(b) => arrive(gs.move(b))
    }

  def inventory(gs: GameState): IO[Unit] =
    gs.itemsHeld match {
      case Nil => IO.putStrLn("YOU'RE NOT CARRYING ANYTHING.")    *> interact(gs)
      case is  => is.traverse(i => IO.putStrLn(i.inventoryDesc)) *> interact(gs)
    }

  def look(gs: GameState): IO[Unit] = // msg 15
    IO.putStrLn("SORRY, BUT I AM NOT ALLOWED TO GIVE MORE DETAIL.  I WILL REPEAT THE") *>
    IO.putStrLn("LONG DESCRIPTION OF YOUR LOCATION.") *>
    arrive(gs)

  def arrive(gs: GameState): IO[Unit] =
    describe(gs) *> interact(gs)

  def interact(gs: GameState): IO[Unit] =
    getInput flatMap {
      case "LOOK"             => look(gs)
      case "QUIT"             => ioUnit
      case "BACK" | "GO BACK" => back(gs)
      case "INVENTORY"        => inventory(gs)
      case s      =>
        gs.room.travel.find(_._3.exists(s.startsWith)) match {
          case Some((next, _, _)) =>
            next.project match {
              case Message(txt)          => putStrLn(txt) *> interact(gs)
              case Special(n)            => putStrLn(s"Not implemented: special $n") *> interact(gs)
              case to @ Room(_, _, _, _) => arrive(gs.move(to))
            }
          case _ => putStrLn("WHAT?") *> interact(gs)
        }
    }

  override def runc: IO[Unit] =
    for {
      src <- IO(Source.fromFile("advent.dat").mkString) // leaks, I don't care
      _   <- arrive(Parser.parse(src))
    } yield ()

}
