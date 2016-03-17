package iguana.regex.matcher

import iguana.regex._
import iguana.regex.automaton._
import iguana.utils.collections.rangemap.{ArrayIntRangeTree, AVLIntRangeTree, IntRangeTree}
import iguana.utils.input.Input

import scala.annotation.tailrec
import scala.collection._

trait Matcher {
  /**
    * @param i     the input position to start matching
    * @return      the length of the matched input, or -1 in case of no match.
    */
  def m(input: Input, i: Int): Int

  /**
    * @return      true if can match the whole input
    */
  def m(input: Input): Boolean = m(input, 0) == input.length - 1

  /**
    * @return true if can match a prefix of the input
    */
  def mPrefix(input: Input): Boolean = m(input, 0) > -1

  /**
    * @param start start position to match
    * @param end   end position to match
    * @return      true if it can match from start to end
    */
  def m(input: Input, start: Int, end: Int): Boolean = m(input, start) == end - start
}

class DFAMatcher(table: Array[IntRangeTree], finalStates: Array[Boolean], start: Int) extends Matcher {

  val ERROR_STATE = -2

  override def m(input: Input, inputIndex: Int): Int = {

    @tailrec
    def loop(i: Int, state: Int, length: Int, maximumMatched: Int): Int = {
      val c = input(i)
      if (c == -1) return maximumMatched

      val s = table(state).get(c)
      if (s == ERROR_STATE)
        maximumMatched
      else
        loop(i + 1, s, length + 1, if (finalStates(s)) length + 1 else maximumMatched)
    }

    loop(inputIndex, start, 0, if (finalStates(start)) 0 else -1)
  }
}

class BackwardDFAMatcher(table: Array[IntRangeTree], finalStates: Array[Boolean], start: Int) extends Matcher {

  val ERROR_STATE = -2

  override def m(input: Input, inputIndex: Int): Int = {

    if (inputIndex == 0) return -1

    @tailrec
    def loop(i: Int, state: Int, length: Int, maximumMatched: Int): Int = {

      val c = input(i)
      if (c == -1) return maximumMatched

      val s = table(state).get(c)
      if (s == ERROR_STATE)
        maximumMatched
      else
        loop(i - 1, s, length + 1, if (finalStates(s)) length + 1 else maximumMatched)
    }

    loop(inputIndex, start, 0, if (finalStates(start)) 0 else -1)
  }
}


object Matcher {

  val matchers: mutable.Map[RegularExpression, Matcher] = mutable.HashMap.empty
  val backwardMatchers: mutable.Map[RegularExpression, Matcher] = mutable.HashMap.empty

  def getMatcher(r: RegularExpression): Matcher = matchers.getOrElseUpdate(r, r match {

    case Character(v) => (input: Input, i: Int) => if (input(i) == v) 1 else -1

    case CharacterRange(start, end) =>
      (input: Input, i: Int) => if (input(i) >= start && input(i) <= end) 1 else -1

    case Epsilon => (input: Input, i: Int) => 0

    case _ => getMatcher(AutomatonOperations.convert(r).determinize)

  })

  def getBackwardMatcher(r: RegularExpression): Matcher = backwardMatchers.getOrElseUpdate(r, r match {
    case Character(v) => (input: Input, i: Int) => if (i == 0) -1
                                                   else if (input(i - 1) == v) 1
                                                   else -1

    case CharacterRange(start, end) => (input: Input, i: Int) => if (i == 0) -1
                                                                 else if (input(i) >= start && input(i) <= end) 1
                                                                 else -1

    case Epsilon => (input: Input, i: Int) => 0

    case _ => getMatcher(AutomatonOperations.convert(r).determinize)

  })

  def getMatcher(dfa: DFA): Matcher = {
    val table = Array.ofDim[IntRangeTree](dfa.statesCount)
    val finalStates = Array.ofDim[Boolean](dfa.statesCount)
    val start: Int = 0

    (0 until table.length) foreach { i => table(i) = new AVLIntRangeTree }

    val ids = mutable.HashMap.empty[State, Int]
    def getId(s: State): Int = ids.getOrElseUpdate(s, ids.size)

    for ((origin, m) <- dfa.reachableStates) {
      for ((r, dest) <- m) {
        val originId = getId(origin)
        val destId = getId(dest)
        table(originId).insert(r, destId)
      }
    }

    for (s <- dfa.states) finalStates(getId(s)) = dfa.isFinal(s)

    (0 until table.length) foreach { i => table(i) = new ArrayIntRangeTree(table(i)) }
    new DFAMatcher(table, finalStates, start)
  }

  implicit def toMatcher(f: (Input, Int) => Int): Matcher = new Matcher {
    override def m(input: Input, i: Int): Int = f(input, i)
  }
}
