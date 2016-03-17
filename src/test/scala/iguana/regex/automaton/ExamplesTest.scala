/*
 * Copyright (c) 2015, Ali Afroozeh and Anastasia Izmaylova, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this 
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this 
 *    list of conditions and the following disclaimer in the documentation and/or 
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE.
 *
 */

package iguana.regex.automaton

import iguana.regex.Sequence
import iguana.utils.input.Input
import org.scalatest.FunSuite


class ExamplesTest extends FunSuite {
	
	test("Id") {
		val id = RegularExpressionExamples.id
		val automaton = id.toAutomaton
		
		assertResult(15)(automaton.statesCount)
		
		assertResult(6)(automaton.determinize)

		val matcher = id.matcher

		assert(matcher.m(Input("a")))
		assert(!matcher.m(Input("9")))
		assert(matcher.m(Input("abc")))
		assert(matcher.m(Input("Identifier")))
		assert(matcher.m(Input("Identifier12")))
		assert(matcher.m(Input("Identifier12Assdfd")))
	}
	
	test("IntersectionKeywordId") {
		val a1 = RegularExpressionExamples.id.toAutomaton
		val a2 = Sequence("for").toAutomaton

		val a = a1.determinize /\ a2.determinize

		assert(a.isLanguageEmpty)
	}
	
	test("float") {
		val float = RegularExpressionExamples.float
		
		val automaton = float.toAutomaton
		assertResult(10)(automaton.statesCount)

    assertResult(6)(automaton.determinize.statesCount)
		
		val matcher = float.matcher

		assert(matcher.m(Input("1.2")))
    assert(matcher.m(Input("1.2"), 0, 3))
		assert(!matcher.m(Input("9")))
		assert(!matcher.m(Input(".9")))
		assert(!matcher.m(Input("123.")))
		assert(matcher.m(Input("12.2")))
		assert(matcher.m(Input("1342343.27890")))
		assert(matcher.m(Input("908397439483.278902433")))
	}
	
  test("unicodeEscape") {
		val regex = RegularExpressionExamples.unicodeEscape
		val automaton = regex.toAutomaton
		assertResult(34)(automaton.statesCount)

    assertResult(37)(automaton.determinize.statesCount)
		
		val matcher = regex.matcher
    assert(matcher.m(Input("\\u0123")))
	}
	
	test("Character") {
		val regex = RegularExpressionExamples.character
		val automaton = regex.toAutomaton
		assertResult(15)(automaton.statesCount)

    assertResult(7)(automaton.determinize.statesCount)
		
		val matcher = regex.matcher
    assert(matcher.m(Input("'ab'")))
	}
	
	test("StringPart") {
		val regex = RegularExpressionExamples.stringPart
		val automaton = regex.toAutomaton
		assertResult(18)(automaton.statesCount)

    assertResult(7)(automaton.determinize.statesCount)
		
		val matcher = regex.matcher

    assert(matcher.m(Input("abcd")))
    assert(!matcher.m(Input("\\aa")))
    assert(!matcher.m(Input("\"aaa")))
	}
	
	test("MultilineComment") {
//		assertTrue(matcher.match(Input.fromString("/*a*/")));
	}	
}
