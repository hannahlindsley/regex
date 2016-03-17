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

import iguana.regex._
import iguana.utils.input.Input
import org.scalatest.FunSuite

class StarTest extends FunSuite {
	
	test("1") {
		val regex = Character('a').*

		val automaton = regex.toAutomaton
		assertResult(4)(automaton.statesCount)
    assertResult(5)(automaton.transitionsCount)

		assertResult(2)(automaton.determinize.statesCount)
    assertResult(2)(automaton.determinize.transitionsCount)

		val matcher = regex.matcher

		assertResult(0)(matcher.m(Input.empty, 0))
		assertResult(1)(matcher.m(Input("a"), 0))
		assertResult(2)(matcher.m(Input("aa"), 0))
		assertResult(3)(matcher.m(Input("aaa"), 0))
		assertResult(6)(matcher.m(Input("aaaaaa"), 0))
		assertResult(17)(matcher.m(Input("aaaaaaaaaaaaaaaaa"), 0))
		assertResult(33)(matcher.m(Input("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"), 0))
	}
		
	test("2") {
		// ([a-a]+)*
		val regex = ('a'--'a').+.*

		val automaton = regex.toAutomaton
		assertResult(8)(automaton.statesCount)
    assertResult(11)(automaton.transitionsCount)

    automaton.determinize.visualize()
    assertResult(3)(automaton.determinize.statesCount)

		val matcher = regex.matcher

    assertResult(0)(matcher.m(Input.empty, 0))
    assertResult(1)(matcher.m(Input("a"), 0))
    assertResult(5)(matcher.m(Input("aaaaa"), 0))
	}
	
	
	test("3") {

    // ([a-z]+ | [(-)] | "*")*
    val regex = (('a'--'z').+ | '-' | '(' | ')' | '*').*

		val automaton = regex.toAutomaton
    assertResult(16)(automaton.statesCount)

    assertResult(6)(automaton.determinize.statesCount)
		
		val matcher = regex.matcher
    assertResult(0)(matcher.m(Input.empty, 0))
    assertResult(1)(matcher.m(Input("a"), 0))
    assertResult(1)(matcher.m(Input("m"), 0))
    assertResult(1)(matcher.m(Input("z"), 0))
    assertResult(1)(matcher.m(Input("*"), 0))
    assertResult(1)(matcher.m(Input("("), 0))
    assertResult(1)(matcher.m(Input(")"), 0))
    assertResult(3)(matcher.m(Input("a)*"), 0))
    assertResult(3)(matcher.m(Input("*(a"), 0))
    assertResult(3)(matcher.m(Input(")*a"), 0))
    assertResult(11)(matcher.m(Input("ab(*za()((a"), 0))
	}
	

}
