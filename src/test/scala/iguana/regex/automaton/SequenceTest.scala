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

class SequenceTest extends FunSuite {
	
	test("Automaton1") {
		val automaton = "ab".toAutomaton
		assertResult(3)(automaton.statesCount)
	}

	test("DFAMatcher1") {
		val matcher = "ab".matcher
		assertResult(true)(matcher.m(Input("ab")))
		assertResult(false)(matcher.m(Input("ac")))
		assertResult(false)(matcher.m(Input("da")))
	}
	
	test("Automaton2") {
		val automaton = (('a'--'z') ~ ('0'--'9')).toAutomaton
		assertResult(3)(automaton.statesCount)
	}
	
	test("DFAMatcher2") {
		val matcher = (('a'--'z') ~ ('0'--'9')).matcher

		assertResult(true)(matcher.m(Input("a0")))
		assertResult(true)(matcher.m(Input("a5")))
		assertResult(true)(matcher.m(Input("a9")))
		assertResult(true)(matcher.m(Input("c7")))
		assertResult(true)(matcher.m(Input("z0")))
		assertResult(true)(matcher.m(Input("z9")))

		assertResult(false)(matcher.m(Input("ac")))
		assertResult(false)(matcher.m(Input("da")))
	}
	
	/**
	 * Two character classes with overlapping ranges
	 */
	test("Automaon3") {
		val automaton = (('a'--'z') ~ ('b'--'m')).toAutomaton
    assertResult(3)(automaton.statesCount)
	}
	
	test("DFAMatcher3") {
    val matcher = (('a'--'z') ~ ('b'--'m')).matcher
    assertResult(true)(matcher.m(Input("dm")))
	}
	
}
