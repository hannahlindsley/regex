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

class UnionTest extends FunSuite {
	
	test("1") {
		val a = "if".toAutomaton.determinize \/ "when".toAutomaton.determinize
		val matcher = a.getMatcher

		assert(matcher.m(Input("if")))
		assert(matcher.m(Input("when")))
		assert(!matcher.m(Input("i")))
		assert(!matcher.m(Input("w")))
		assert(!matcher.m(Input("wh")))
		assert(!matcher.m(Input("whe")))
		assert(!matcher.m(Input("whenever")))
		assert(!matcher.m(Input("else")))
	}
	
	test("2") {
		val a = "if".toAutomaton.determinize \/ "when".toAutomaton.determinize \/ "new".toAutomaton.determinize
		val matcher = a.getMatcher

		assert(matcher.m(Input("if")))
		assert(matcher.m(Input("when")))
		assert(matcher.m(Input("new")))
		assert(!matcher.m(Input("i")))
		assert(!matcher.m(Input("w")))
		assert(!matcher.m(Input("n")))
		assert(!matcher.m(Input("ne")))
		assert(!matcher.m(Input("news")))
		assert(!matcher.m(Input("else")))
	}

}
