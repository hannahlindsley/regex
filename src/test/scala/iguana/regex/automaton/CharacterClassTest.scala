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
import org.scalatest.FunSuite
import iguana.regex._
import iguana.utils.Unicode._

class CharacterClassTest extends FunSuite {

	test("non-overlapping character class") {
		val regex = ('a'--'z', '1'--'8')

		val automaton = regex.toAutomaton
		assertResult(6)(automaton.statesCount)

    assertResult(3)(automaton.determinize.statesCount)

		val matcher = regex.matcher

    assert(matcher.m(Input('a')))
    assert(matcher.m(Input('f')))
    assert(matcher.m(Input('z')))
    assert(matcher.m(Input('1')))
    assert(matcher.m(Input('5')))
    assert(matcher.m(Input('8')))

    assert(!matcher.m(Input('0')))
    assert(!matcher.m(Input('9')))
    assert(!matcher.m(Input('*')))
	}
	
	test("overlapping character class") {
		val regex = ('1'--'5', '1'--'7', '3'--'8')

		val automaton = regex.toAutomaton
		assertResult(8)(automaton.statesCount)

		assertResult(5)(automaton.determinize.statesCount)

    val matcher = regex.matcher

    assert(matcher.m(Input('1')))
    assert(matcher.m(Input('2')))
    assert(matcher.m(Input('3')))
    assert(matcher.m(Input('4')))
    assert(matcher.m(Input('5')))
    assert(matcher.m(Input('6')))
    assert(matcher.m(Input('7')))
    assert(matcher.m(Input('8')))

    assert(!matcher.m(Input('0')))
    assert(!matcher.m(Input('9')))
	}

  test("not") {
    val c: CharacterClass = ('0'--'9', 'a'--'z')
    val expected: CharacterClass = (1--('0' - 1), ('9' + 1)--('a' - 1), ('z' + 1)--MAX_UTF32_VAL)
    assertResult(expected)(c.!)
	}
	
}
