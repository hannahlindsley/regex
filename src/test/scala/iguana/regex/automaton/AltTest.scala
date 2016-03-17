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

import iguana.regex.Alt
import iguana.regex.Character
import iguana.regex.Sequence
import iguana.utils.input.Input
import org.scalatest.FunSuite


class AltTest extends FunSuite {

	test("1") {
		val a = Character('a')
		val b = Character('b')
		
		val regex = Alt.from(a, b)
		
		val automaton = regex.toAutomaton
		assertResult(6)(automaton.statesCount)
		
		assertResult(3)(automaton.determinize)
		
		val matcher = regex.matcher
		assertResult(1)(matcher.m(Input("a"), 0))
		assertResult(1)(matcher.m(Input("b"), 0))
	}
	
	test("2") {
		val alt = Alt.from(Sequence("for"), Sequence("forall"))
		val matcher = alt.matcher
		assertResult(3)(matcher.m(Input("for"), 0))
		assertResult(6)(matcher.m(Input("forall"), 0))
	}
	
	test("3") {
		val regex = Alt.from(Sequence("when"), Sequence("if"))
		val matcher = regex.matcher
		
		assertResult(4)(matcher.m(Input("when"), 0))
		assertResult(2)(matcher.m(Input("if"), 0))
	}
	
}
