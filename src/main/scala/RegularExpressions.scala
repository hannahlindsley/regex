
package iguana.regex

import iguana.regex.automaton.{Automaton, AutomatonOperations}
import iguana.regex.matcher.Matcher

import scala.collection.mutable
import iguana.utils._
import iguana.utils.Unicode._


trait RegularExpression extends Serializable  {
  def name: String
  def lookaheads: Set[CharacterRange]
  def lookbehinds: Set[CharacterRange]
  def firstSet: Set[CharacterRange]
  def isNullable : Boolean

  lazy val toAutomaton: Automaton = AutomatonOperations.convert(this)

  lazy val matcher: Matcher = Matcher.getMatcher(this)

  // Utility methods to be compatible with Java code
  def getLookaheads = lookaheads
  def getLookbehinds = lookbehinds
  def getFirstSet = firstSet
  def getName = name

  // Combinators
  def ~(r: RegularExpression): Sequence[RegularExpression] = Sequence(this, r)
  def |(rs: RegularExpression): Alt[RegularExpression] = Alt(List(this,rs))
  def *(): Star = Star(this)
  def +(): Plus = Plus(this)
  def ?(): Opt = Opt(this)
}

abstract class AbstractRegularExpression(val lookaheads: Set[CharacterRange] = Set(),
                                         val lookbehinds: Set[CharacterRange] = Set()) extends  RegularExpression

case class Character(value: Int) extends AbstractRegularExpression {
  override def name: String = charName(value)
  override def isNullable: Boolean = false
  override lazy val firstSet: Set[CharacterRange] = Set(CharacterRange(value, value))

  def --(c: Character): CharacterRange = CharacterRange(this.value, c.value)
  def !(): CharacterClass = (1--(value - 1), (value + 1)--MAX_UTF32_VAL)

  override def toString = name
}

case class CharacterRange (start: Int, end: Int) extends AbstractRegularExpression with iguana.utils.collections.rangemap.Range {
  require (start <= end, s"$start should be greater than or equal to $end")

  val name: String = if (start == -1 && end == -1) "epsilon"
                     else s"[${charName(start)}-${charName(end)}]"

  def isNullable: Boolean = false
  lazy val firstSet: Set[CharacterRange] = Set(this)

  override def getStart: Int = start
  override def getEnd: Int = end

  override def toString = name

  def !(): RegularExpression = (1--(start - 1), (end + 1)--MAX_UTF32_VAL)
}

case class Star(r: RegularExpression) extends AbstractRegularExpression {
  override def name = r.name + "*"
  override val isNullable: Boolean = true
  override lazy val firstSet: Set[CharacterRange] = r.firstSet
}

case class Plus(r: RegularExpression) extends AbstractRegularExpression {
  override def name: String = r.name + "+"
  override lazy val isNullable: Boolean = r.isNullable
  override lazy val firstSet: Set[CharacterRange] = r.firstSet
}

case class Opt(r: RegularExpression) extends AbstractRegularExpression {
  override def name: String = r.name + "?"
  override val isNullable: Boolean = true
  override lazy val firstSet: Set[CharacterRange] = r.firstSet
}

case class Alt[T <: RegularExpression](rs: List[T]) extends AbstractRegularExpression {
  override val name: String = rs.mkString("(", "|", ")")
  override lazy val isNullable: Boolean =  rs.exists(_ isNullable)
  override lazy val firstSet: Set[CharacterRange] = rs.flatMap(r => r.firstSet).toSet
}

case class Sequence[T <: RegularExpression](val rs: List[T]) extends AbstractRegularExpression {
  override val name: String = rs.mkString("(", " ", ")")
  override lazy val isNullable: Boolean = rs.forall(_ isNullable)
  override lazy val firstSet: Set[CharacterRange] = getFirstSet(rs)

  def getFirstSet(rs: List[T]): Set[CharacterRange] =
    rs.head.firstSet ++ (if (rs.head.isNullable) getFirstSet(rs.tail) else Set())
}

object Sequence {
  def apply[T <: RegularExpression](rs: T*): Sequence[T] = Sequence(rs.toList)
  def apply(s: String): Sequence[Character] = Sequence(s.toList.map(Character(_)))
}

case class Name(val name: String) extends AbstractRegularExpression {
  override def firstSet: Set[CharacterRange] = throw new RuntimeException("Names should be first inlined.")
  override def isNullable: Boolean = throw new RuntimeException("Names should be first inlined.")
}

case object Epsilon extends AbstractRegularExpression {
  def instance = this
  def VALUE = -2
  override def name: String = "EOF"
  override def isNullable: Boolean = true
  override lazy val firstSet: Set[CharacterRange] = Set(CharacterRange(VALUE, VALUE))
}

case object EOF extends AbstractRegularExpression {
  def instance = this
  def VALUE = -1
  override def name: String = "EOF"
  override def isNullable: Boolean = false
  override lazy val firstSet: Set[CharacterRange] = Set(CharacterRange(VALUE, VALUE))
}

object CharacterRange {

  def apply(c: Int): CharacterRange = apply(c, c)

  def toNonOverlapping(rs: CharacterRange*): Set[CharacterRange] =
    toNonOverlapping(rs.toSet)

  def toNonOverlapping(rs: Set[CharacterRange]): Set[CharacterRange] =
    toNonOverlappingMap(rs).values.flatMap(s => s).toSet

  def toNonOverlappingMap(rs: CharacterRange*): Map[CharacterRange, Set[CharacterRange]] =
    toNonOverlappingMap(rs.toSet)

  def toNonOverlappingMap(rs: Set[CharacterRange]): Map[CharacterRange, Set[CharacterRange]] = {
    if (rs.isEmpty) return Map()
    if (rs.size == 1) return Map(rs.head -> Set(rs.head))

    // Sort ranges
    val l0 = rs.toSeq.sortWith((r1, r2) => if (r1.start == r2.start) r1.end <= r2.end else r1.start < r2.start)

    // partition the consecutive overlapping ranges in the sorted list
    val l1 = l0.takeWhen(_ overlaps _)

    l1.map(convert(_)).reduce(_ ++ _)
  }

  /**
    * Converts the given sequence of ranges into non-overlapping
    */
  private def convert(ranges: Seq[CharacterRange]): Map[CharacterRange, Set[CharacterRange]] = {
    if (ranges.isEmpty) return Map()
    if (ranges.size == 1) return Map(ranges.head -> Set(ranges.head))

    val l = ranges.flatMap(r => List(r.start - 1, r.end)).distinct.sorted

    val b = List.newBuilder[CharacterRange]
    var start = l.head + 1
    for (i <- l.tail) {
      b += CharacterRange(start, i)
      start = i + 1
    }
    val result = b.result()

    val x = for {
      r1 <- ranges
      r2 <- result
      if (r1 contains r2)
    } yield (r1, r2)

    x.groupBy(_._1).mapValues(_.map(_._2).toSet)
  }

}

object CharacterClass {

  def apply(rs: CharacterRange*): CharacterClass = Alt.from(rs:_*)

  def not(cc: CharacterClass): CharacterClass = {
    val builder = List.newBuilder[CharacterRange]

    val ranges = cc.rs

    if (ranges.head.start >= 1) builder += CharacterRange(1, ranges.head.start - 1)

    builder ++= ranges.sliding(2)
                        .withFilter { case List(r1, r2) => r2.start > r1.end + 1 }
                        .map { case List(r1, r2) => CharacterRange(r1.end + 1, r2.start - 1) }

    builder += CharacterRange(ranges.last.end + 1, MAX_UTF32_VAL)

    CharacterClass(builder.result():_*)
  }
}

object Alt {
  def from[T <: RegularExpression](ts:T*): Alt[T] = Alt(ts.toList)
}
