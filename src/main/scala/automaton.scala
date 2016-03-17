
package iguana.regex.automaton

import iguana.regex.CharacterRange
import iguana.regex.matcher.Matcher
import iguana.utils._

import scala.annotation.tailrec
import scala.collection.mutable

case class State private(id: Int)

object State {
  def apply(): State = State(id)

  var id = 0
  def newId = {
    id += 1
    id
  }
}

case class Transition(range: CharacterRange, origin: State, dest: State) {
  def isEpsilon = false
}

object Transition {

  def apply(v: Int, origin: State, dest: State) = new Transition(CharacterRange(v, v), origin, dest)

  def epsilon(origin: State, dest: State): Transition = new Transition(CharacterRange(-1, -1), origin, dest) {
    override def isEpsilon = true
  }
}

trait Automaton {
  def transitions: Set[Transition]
  def startState: State
  def finalStates: Set[State]
  def isFinal(s: State): Boolean = finalStates.contains(s)

  def determinize: DFA

  def canMove(s: State, r: CharacterRange): Boolean

  lazy val statesCount: Int = states.size

  lazy val transitionsCount: Int = transitions.size

  lazy val isLanguageEmpty: Boolean =
    finalStates.isEmpty || (finalStates.contains(startState) && outgoingTransitions.getOrElse(startState, Set()).forall(t => t.isEpsilon && t.dest == startState))

  lazy val getMatcher: Matcher = Matcher.getMatcher(this.determinize)

  def get(i: Int): State = ???

  /*
   * Creates the reverse of the given automaton. A reverse automaton
   * accept the reverse language accepted by the original automaton. To construct
   * a reverse automaton, all final states of the original automaton are becoming
   * start states, transitions are reversed and the start state becomes the
   * only final state.
   */
  lazy val reverse: NFA = {
    val start = State()
    val l = Set.newBuilder[Transition]
    l ++= finalStates.map(s => Transition.epsilon(startState, s))
    l ++= transitions.map(t => Transition(t.range, t.dest, t.origin))
    NFA(l.result(), start, Set(startState))
  }

  lazy val alphabet: Set[CharacterRange] =
    transitions.withFilter(!_.isEpsilon).map(_.range)

  lazy val states: Set[State] =
    transitions.flatMap(t => List(t.origin, t.dest))

  lazy val outgoingTransitions: Map[State, Set[Transition]] =
    transitions.groupBy(_.origin)

}

case class NFA(transitions: Set[Transition], startState: State, finalStates: Set[State]) extends Automaton {

  type ReachGraph = Map[State, Set[State]]

  lazy val reachableStates: Map[State, Map[CharacterRange, Set[State]]] =
    transitions.filter(!_.isEpsilon).groupBy(_.origin).mapValues(_.groupBy(_.range).mapValues(_.map(_.dest)))

  lazy val epsilonClosures: Map[State, Set[State]] = {

    val initial = transitions.filter(_.isEpsilon).groupBy(_.origin).mapValues(_.map(_.dest))

    @tailrec
    def loop(workingList: ReachGraph, all: ReachGraph): ReachGraph = {
      println("all = " + all)
      val newMap = for {
        (s, set) <- workingList
        newSet = set.flatMap(all.getOrElse(_, Set()))
        if { println("--->" + newSet); set != newSet}
      } yield s -> (set ++ newSet)

      println(newMap)
      if (newMap.isEmpty) all
      else loop(newMap, all merge newMap)
    }

    println(initial)

    loop(initial, initial) merge states.map(s => s -> Set(s)).toMap
  }

  def reachableStates(s: State, r: CharacterRange): Set[State] =
    reachableStates.get(s).flatMap(_.get(r)).getOrElse(Set())

  def canMove(s: State, r: CharacterRange): Boolean = !reachableStates(s, r).isEmpty

  private def epsilonClosure(states: Map[CharacterRange, Set[State]]): Map[CharacterRange, Set[State]] =
    states.mapValues(s => s ++ epsilonClosure(s))

  private def epsilonClosure(states: Set[State]): Set[State] =
    states.flatMap(epsilonClosures.getOrElse(_, Set.empty[State]))

  lazy val determinize: DFA = {

    import iguana.regex._
    this.visualize()

    val statesMap = mutable.HashMap.empty[Set[State], State]
    def fresh(set: Set[State]): State = statesMap.getOrElseUpdate(set, State())

    // compute non-overlapping alphabet
    val alphabet = CharacterRange.toNonOverlappingMap(this.alphabet)

    // calculate a new reachability graph based on the new non-overlapping alphabet
    val reachableStates = this.reachableStates.mapValues(_.flatMap { case (r, s) =>  for (x <- alphabet.get(r).toSeq; y <- x) yield y -> s})

    def move(states: Set[State]): Map[CharacterRange, Set[State]] =
      states.flatMap(reachableStates.get(_)).reduceOption(_ merge _).getOrElse(Map())

    @tailrec
    def loop(workingSet: Set[Set[State]], allStates: Set[Set[State]], acc: Set[Transition]): Set[Transition] = {
      val tb = Set.newBuilder[Transition]
      val sb = Set.newBuilder[Set[State]]

      for (states <- workingSet) {
        val next = epsilonClosure(move(states))
        for (((r, dest)) <- next) {
          tb += Transition(r, fresh(states), fresh(dest))
          if (!allStates.contains(dest)) sb += dest
        }
      }

      val newStates = sb.result()
      val newTransitions = tb.result()

      if (newStates.isEmpty) acc ++ newTransitions
      else loop(newStates, allStates ++ newStates, acc ++ newTransitions)
    }

    val initialSet = epsilonClosure(Set(startState))
    val transitions = loop(Set(initialSet), Set(initialSet), Set())

    val newStartState = statesMap(initialSet)
    val newEndStates = statesMap withFilter { case (states, s) => states.exists(finalStates.contains(_)) } map { case (states, s) => s } toSet

    DFA(transitions, newStartState, newEndStates)
  }

}

case class DFA(transitions: Set[Transition], startState: State, finalStates: Set[State]) extends Automaton {

  lazy val reachableStates: Map[State, Map[CharacterRange, State]] =
    outgoingTransitions.mapValues(_.map(t => t.range -> t.dest).toMap)

  def minimize: DFA = ???

  def reachableState(s: State, r: CharacterRange): Option[State] =
    reachableStates.get(s).flatMap(_.get(r))

  def canMove(s: State, r: CharacterRange): Boolean =
    reachableState(s, r).isDefined

  def determinize: DFA = this

  def union(other: DFA): DFA = DFA.product(this, other, (s1, s2) => isFinal(s1) || other.isFinal(s2))
  def \/(other: DFA): DFA = union(other)

  def intersect(other: DFA): DFA = DFA.product(this, other, (s1, s2) => isFinal(s1) && other.isFinal(s2))
  def /\(other:DFA): DFA = intersect(other)

  // difference
  def -(other: DFA): DFA = DFA.product(this, other, (s1, s2) => isFinal(s1) && !other.isFinal(s2))
}

object DFA {

  def makeComplete(a: DFA, alphabet: Set[CharacterRange]): DFA = {
    val dummyState = State()

    val selfTransitions = alphabet.map(a => Transition(a, dummyState, dummyState))

    val transitions = for {
      r <- alphabet
      s <- a.states
      if (!a.canMove(s, r))
    } yield Transition(r, s, dummyState)

    DFA(selfTransitions ++ transitions, a.startState, a.finalStates)
  }

  def product(da1: DFA, da2: DFA, isFinal: (State, State) => Boolean): DFA = {
    val alphabet = CharacterRange.toNonOverlapping(da1.alphabet ++ da2.alphabet)
    val a1 = makeComplete(da1, alphabet)
    val a2 = makeComplete(da2, alphabet)

    val statesMap: mutable.Map[(State, State), State] = mutable.HashMap.empty
    val transitions: mutable.Set[Transition] = mutable.HashSet.empty
    val finalStates: mutable.Set[State] = mutable.HashSet.empty
    var startState: State = null

    for (s1 <- da1.states)
      for (s2 <- da2.states)
        for (r <- alphabet)
          if (da1.canMove(s1, r) && da2.canMove(s2, r)) {
            val origin = statesMap.getOrElse((s1, s2), State())
            val dest = statesMap.getOrElse((da1.reachableState(s1, r).get, da2.reachableState(s2, r).get), State())
            transitions += Transition(r, origin, dest)
            if (a1.startState == s1 && a2.startState == s2) startState = origin
            if (isFinal(s1, s2)) finalStates += dest
          }

    DFA(transitions.toSet, startState, finalStates.toSet)
  }
}
