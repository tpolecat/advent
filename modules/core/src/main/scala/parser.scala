package muck

import scalaz._, Scalaz.{ char => _, _ }, scalaz.effect._, IO._
import atto._, Atto._, compat.scalaz._
import scala.io.Source

import matryoshka._
import matryoshka.implicits._
import matryoshka.data._
import matryoshka.patterns._

object Parser {

  // helpers
  def intercalateMap[F[_]: Foldable, A, B: Monoid](ps: F[(A, B)], b: B): Map[A, B] =
    ps.foldMap { case (n, s) => Map(n -> List(s)) } map { case (n, ss) => n -> ss.intercalate(b) }

  def smashMap[F[_]: Foldable, A](ps: F[(A, String)]): Map[A, String] =
    intercalateMap(ps, "\n")

  // combinators
  def tok[A](p: Parser[A]): Parser[A] = p <~ opt(char('\t'))
  def sec[A](p: Parser[A]): Parser[List[A]] = nat ~> rol ~> many(p) <~ string("-1") <~ rol

  // parsers
  val nat  = int.filter(_ >= 0)
  val eol  = char('\n').void
  val rol  = takeWhile(_ != '\n') <~ eol
  val int3 = manyN(3, digit).map(_.mkString.toInt)
  val desc = (nat ~ eol.as("")) | (tok(nat) ~ rol)
  val nats = many(tok(nat)) <~ eol
  val adef = tok(nat) ~ nat <~ eol
  val hint = (tok(nat) |@| tok(nat) |@| tok(nat) |@| tok(nat) |@| nat).tupled <~ eol
  val end  = string("0") <~ eol <~ endOfInput

  val item = (desc |@| many(token(int3) ~> rol)) { case ((id, desc), ss) =>
    id -> ((loc: ItemLocation) => Item(desc, 0, ss, loc))
  }

  val iloc = (tok(nat) |@| tok(nat) |@| opt(int) <~ eol) {
    case (id, r1, None)     => id -> ItemLocation.InRoom(r1, true)
    case (id, r1, Some(-1)) => id -> ItemLocation.InRoom(r1, false)
    case (id, r1, Some(r2)) => id -> ItemLocation.InTwoRooms(r1, r2)
  }

  val trav = (tok(nat) |@| tok(nat) |@| many(tok(nat)) <~ eol)(Travel.decode)

  // final parser
  val main =
    for {
      lfd <- sec(desc).map(smashMap(_)) //  1. long-form description
      sfd <- sec(desc).map(_.toMap)     //  2. short-form description
      tt  <- sec(trav)                  //  3. travel table
      voc <- sec(desc).map(_.toMap)     //  4. vocabulary
      is  <- sec(item).map(_.toMap)     //  5. items
      ms  <- sec(desc).map(smashMap(_)) //  6. messages
      ols <- sec(iloc).map(_.toMap)     //  7. object locations
      ads <- sec(adef)                  //  8. action defaults
      las <- sec(nats)                  //  9. liquid assets
      cms <- sec(desc).map(_.toMap)     // 10. class messages
      hs  <- sec(hint)                  // 11. hints
      maj <- sec(desc) <~ end           // 12. magic messages
    } yield Data(lfd, sfd, tt, voc, ms, is, ols) //, ads, las, cms, hs, maj)


  // Two cookies are needed in order to handle `Message` and `Special` (for now)
  def decodeLoc(data: Data): Coalgebra[LocationF, Int] = { id =>
           if (id > 500) Message(data.messages.get(id - 500).getOrElse(s"no such message: $id"))
      else if (id > 300) Special(id - 300)
      else {
        val ts0 = data.travels.filter(_.from == id).map(t => (t.to, t.condition, t.motions.flatMap(data.vocabulary.get)))
        Room(
          id,
          data.longDescriptions.get(id).getOrElse(sys.error(s"no such long desc: $id")),
          data.shortDescriptions.get(id),
          ts0
        )
     }
  }

  case class Data(
    longDescriptions:  Map[Int, String],
    shortDescriptions: Map[Int, String],
    travels: List[Travel],
    vocabulary: Map[Int, String],
    messages: Map[Int, String],
    unmooredItems: Map[Int, ItemLocation => Item],
    itemLocatons: Map[Int, ItemLocation]
  ) {

    def items: Map[Int, Item] =
      unmooredItems.map { case (id, f) => id -> f(itemLocatons(id))}

  }

  def parse(source: String): GameState = {
    val data = main.parseOnly(source).option.getOrElse(sys.error("Fatal: parse error.")) // TODO: this, better
    1.ana[Location](decodeLoc(data)).project match {
      case r @ Room(_, _, _, _) => GameState(r, None, data.items.values.toList)
      case _                    => sys.error("Unpossible. Start location is a Room.")
    }
  }

}
