package iguana.regex.automaton

import iguana.regex._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AutomatonOperations {

  def convert(r: RegularExpression): Automaton = r match {

    case Character(value) =>
      val startState = State()
      val finalState = State()
      val t = Transition(CharacterRange(value), startState, finalState)
      DFA(Set(t), startState, Set(finalState))

    case CharacterRange(start, end) =>
      val startState = State()
      val finalState = State()
      val t = Transition(CharacterRange(start, end), startState, finalState)
      DFA(Set(t), startState, Set(finalState))

    case EOF =>
      val startState = State()
      val finalState = State()
      val t = Transition(CharacterRange(EOF.VALUE), startState, finalState)
      DFA(Set(t), startState, Set(finalState))

    case Epsilon =>
      val state = State()
      DFA(Set(), state, Set(state))

    case Star(s) =>
      val startState = State()
      val finalState = State()

      val automaton = convert(s)

      val builder = Set.newBuilder[Transition]
      builder ++= automaton.transitions

      builder += Transition.epsilon(startState, automaton.startState)

      for (s <- automaton.finalStates) {
        builder += Transition.epsilon(s, finalState)
        builder += Transition.epsilon(s, automaton.startState)
      }

      builder += Transition.epsilon(startState, finalState)
      NFA(builder.result(), startState, Set(finalState))

    case Plus(s) => convert(Sequence(s, Star(s)))

    case Opt(s) =>
      val startState = State()
      val finalState = State()

      val automaton = convert(s)

      val builder = Set.newBuilder[Transition]
      builder ++= automaton.transitions

      builder += Transition.epsilon(startState, automaton.startState)

      for (s <- automaton.finalStates)
        builder += Transition.epsilon(s, finalState)

      builder += Transition.epsilon(startState, finalState)
      NFA(builder.result(), startState, Set(finalState))

    case Alt(rs) =>
      if (rs.size == 1) return convert(rs.head)

      val automatons = rs.map(convert(_))

      val startState = State()
      val finalState = State()

      val builder = Set.newBuilder[Transition]

      for (automaton <- automatons) {
        builder ++= automaton.transitions

        builder += Transition.epsilon(startState, automaton.startState)

        for (s <- automaton.finalStates) {
          builder += Transition.epsilon(s, finalState)
        }
      }

      NFA(builder.result(), startState, Set(finalState))

    case Sequence(rs) =>
      val automatons = rs.map(convert(_))

      val builder = Set.newBuilder[Transition]
      for (a <- automatons) builder ++= a.transitions

      automatons.sliding(2).foreach {
        case List(current, next) =>
          for (s <- current.finalStates) {
            builder += Transition.epsilon(s, next.startState)
          }
        }

      NFA(builder.result(), automatons.head.startState, automatons.last.finalStates)
  }

}
