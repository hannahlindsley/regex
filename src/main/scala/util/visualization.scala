
package iguana.regex.util.visualization

import iguana.regex.automaton.{Transition, State, Automaton}
import iguana.utils.visualization.GraphVizUtil

object AutomatonToDot {

  def toDot(a: Automaton, dir: String, file: String) {

    val NODE = "[shape=circle, height=0.1, width=0.1, color=black, label=\"%d\", fontcolor=black, fontsize=10];"
    val FINAL_NODE = "[shape=doublecircle, height=0.1, width=0.1, color=black, label=\"%d\", fontcolor=black, fontsize=10];"
    val EDGE = "edge [color=black, style=solid, penwidth=0.5, arrowsize=0.7, label=\"%s\"];"

    def StateToDot(s: State) = s.id  + (if (a.isFinal(s)) FINAL_NODE.format(s.id) else NODE.format(s.id)) + "\n"
    def TransitionToDot(t: Transition) = EDGE.format(t.range) + t.origin.id + "-> {" + t.dest.id + "}\n"

    val dot = (a.states.map(StateToDot(_)) ++ a.transitions.map(TransitionToDot(_))).mkString

    GraphVizUtil.generateGraph(dot.replaceAll("epsilon", "&epsilon;"), dir, file, 1)
  }



}