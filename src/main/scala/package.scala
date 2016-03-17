
package iguana

import iguana.regex.automaton.Automaton
import iguana.regex.util.visualization.AutomatonToDot

package object regex {

  type CharacterClass = Alt[CharacterRange]

  implicit def toCharacter(c: Char): Character = Character(c)

  implicit def toCharacter(i: Int): Character = Character(i)

  implicit def toSequence(s: String): Sequence[Character] = Sequence(s)

  implicit def toCharacterClass(t: Tuple2[CharacterRange, CharacterRange]): CharacterClass = CharacterClass(t._1, t._2)
  implicit def toCharacterClass(t: Tuple3[CharacterRange, CharacterRange, CharacterRange]): CharacterClass = CharacterClass(t._1, t._2, t._3)
  implicit def toCharacterClass(t: Tuple4[CharacterRange, CharacterRange, CharacterRange, CharacterRange]): CharacterClass = CharacterClass(t._1, t._2, t._3, t._4)
  implicit def toCharacterClass(t: Tuple5[CharacterRange, CharacterRange, CharacterRange, CharacterRange, CharacterRange]): CharacterClass = CharacterClass(t._1, t._2, t._3, t._4, t._5)


  implicit class VisualizeAutomaton(a: Automaton) {
    def visualize() {
      AutomatonToDot.toDot(a, "/Users/afroozeh/output", "automaton")
    }
  }

  implicit class EnhancedCharacterClass(cc: CharacterClass) {
    def !(): CharacterClass = CharacterClass.not(cc)
  }
}