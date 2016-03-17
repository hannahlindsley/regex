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

import iguana.utils.input.Input
import org.junit.Test
import org.scalatest.FunSuite

class IntersectionTest extends FunSuite {

	test("1") {
		
		val x0 = State()
		val x1 = State()
		
		val t1 = Transition('0', x0, x0)
		val t2 = Transition('1', x0, x1)
    val t3 = Transition('0', x1, x1)
    val t4 = Transition('1', x1, x0)

		val a1 = NFA(Set(t1, t2, t3, t4), x0, Set(x1))

		// Matches an odd number of 1's.

		val y0 = State()
		val y1 = State()

    val s1 = Transition('0', y0, y1)
    val s2 = Transition('1', y0, y1)
    val s3 = Transition('0', y1, y0)
    val s4 = Transition('1', y1, y0)

		// Matches an string of even length.
		val a2 = NFA(Set(s1, s2, s3, s4), y0, Set(y0))
		
		val intersect = a1.determinize /\ a2.determinize
		val matcher = intersect.getMatcher
		
		assert(!intersect.isLanguageEmpty)
		assert(matcher.m(Input("111001110001")))
		assert(!matcher.m(Input("111001010001")))
		assert(!matcher.m(Input("1110011100010")))
	}
	
	test("2") {
		val f = RegularExpressionExamples.float
		val id = RegularExpressionExamples.id
		
		val intersect = f.toAutomaton.determinize /\ id.toAutomaton.determinize
		
		// Should not overlap, therefore the intersection should be empty.
		assert(intersect.isLanguageEmpty)
	}
		
}
