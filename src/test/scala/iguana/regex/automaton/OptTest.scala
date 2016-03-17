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

class OptTest extends FunSuite {
	
	test("1") {
		val regex = 'a'.?
		val automaton = regex.toAutomaton
		assertResult(4)(automaton.statesCount)
		
		assertResult(2)(automaton.determinize.statesCount)

		val matcher = regex.matcher
		assert(matcher.m(Input("a")))
		assertResult(0)(matcher.m(Input.empty, 0))
	}
	
	test("2") {
		val regex = "integer".?
		val automaton = regex.toAutomaton
		assertResult(16)(automaton.statesCount)

    assertResult(8)(automaton.determinize.statesCount)

		val matcher = regex.matcher
		assert(matcher.m(Input("integer")))
		assert(!matcher.m(Input("int")))
		assert(matcher.mPrefix(Input("int")))
		assertResult(0)(matcher.m(Input.empty, 0))
	}

}
