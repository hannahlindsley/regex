
package iguana.regex

import scala.collection.SortedSet

trait RegularExpression extends Serializable  {
  def name: String
  def lookaheads: Set[CharacterRange]
  def lookbehinds: Set[CharacterRange]
  def firstSet: Set[CharacterRange]
  def isNullable : Boolean

  // Utility methods to be compatible with Java code
  def getLookaheads = lookaheads
  def getLookbehinds = lookbehinds
  def getFirstSet = firstSet
  def getName = name
}

abstract class AbstractRegularExpression(val lookaheads: Set[CharacterRange] = Set(),
                                         val lookbehinds: Set[CharacterRange] = Set()) extends  RegularExpression

case class Character(c: Int) extends AbstractRegularExpression {
  override def name: String = Character.getName(c)
  override def isNullable: Boolean = false
  override lazy val firstSet: Set[CharacterRange] = Set(CharacterRange(c, c))
}

case class CharacterRange(start: Int, end: Int) extends AbstractRegularExpression with iguana.utils.collections.rangemap.Range {
  override def name: String = s"[${Character.getName(start)}-${Character.getName(end)}]"
  override def isNullable: Boolean = false
  override lazy val firstSet: Set[CharacterRange] = Set(this)

  override def getStart: Int = start
  override def getEnd: Int = end
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

case class Alt[T <: RegularExpression](val rs: List[T]) extends AbstractRegularExpression {
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

object Character {

  def from(c: Int): Character = Character(c)

  def getName(c: Int): String =
    if (isPrintableAscii(c)) {
      c.toChar + ""
    } else {
      val s: String = "\\u" + "%04X".format(c)
      if ((s == "\\u000D") || (s == "\\u000A")) "\\" + s else s
    }

  def isPrintableAscii(codePoint: Int): Boolean =
    '\u0020' < codePoint && codePoint < '\u007f'
}

object CharacterRange {
  def in(start: Int, end: Int): CharacterRange = CharacterRange(start, end)
  def from(c: Int): CharacterRange = CharacterRange(c, c)

  def toNonOverlapping(rs: List[CharacterRange]): Map[CharacterRange, List[CharacterRange]] = {
    if (rs.isEmpty) return Map()
    if (rs.size == 1) return Map(rs.head -> List(rs.head))

    // Sort ranges
    val l0 = rs.sortWith((r1, r2) => if (r1.start == r2.start) r1.end > r2.end else r1.start > r1.end)

    // partition the consecutive ranges in the sorted list
    val l1 = l0.foldLeft(List(List(l0.head)))((r, a) => if(r.head.head overlaps a) (a :: r.head) :: r.tail  else List(a) :: r).reverse

    l1.map(x => convert(x)).reduce(_ ++ _)
  }

  private def convert(ranges: List[CharacterRange]): Map[CharacterRange, List[CharacterRange]] = {
    if (ranges.isEmpty) return Map()

    val l = ranges.flatMap(r => List(r.start - 1, r.end)).to[SortedSet]
    ???
  }
}

object Alt {
  def from[T <: RegularExpression](ts:T*): Alt[T] = Alt(ts.toList)
}
