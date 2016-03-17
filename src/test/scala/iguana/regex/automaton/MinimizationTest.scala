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


import org.scalatest.FunSuite
import iguana.regex._

class MinimizationTest extends FunSuite {
	
	test("1") {
		val q0 = State()
		val q1 = State()
		val q2 = State()
		val q3 = State()
		val q4 = State()
		val q5 = State()
		val q6 = State()
		val q7 = State()

    val transitions = Set(
      Transition('0', q0, q7),
      Transition('1', q0, q1),
      Transition('0', q1, q7),
      Transition('1', q1, q0),
      Transition('0', q2, q4),
      Transition('1', q2, q5),
      Transition('0', q3, q4),
      Transition('1', q3, q5),
      Transition('1', q4, q6),
      Transition('0', q5, q5),
      Transition('1', q5, q5),
      Transition('0', q6, q6),
      Transition('1', q6, q5),
      Transition('0', q7, q2),
      Transition('1', q7, q2))

    NFA(transitions, q0, Set(q5, q6)).determinize.minimize
	}

  test("2") {
		val a = State()
		val b = State()
		val c = State()
		val d = State()
		val e = State()

    val transitions = Set(
      Transition('0', a, b),
      Transition('1', a, c),
      Transition('0'--'1', b, d),
      Transition('0'--'1', c, d),
      Transition('0'--'1', d, e))

      NFA(transitions, a, Set(e))
	}

	
	test("3") {
		val a = State()
		val b = State()
		val c = State()
		val d = State()
		val e = State()

    val transitions = Set(
      Transition('0', a, a),
      Transition('1', a, b),
      Transition('0', b, c),
      Transition('1', b, d),
      Transition('0', c, c),
      Transition('1', c, e),
      Transition('0', d, c),
      Transition('1', d, d),
      Transition('0', e, e),
      Transition('1', e, e))

    NFA(transitions, a, Set(a, c, e))
	}

	test("4") {
		val a = State()
		val b = State()
		val c = State()
		val d = State()
		val e = State()
		val f = State()
		val g = State()
		val h = State()
		val i = State()

    val transitions = Set(
		  Transition('1', a, b),
      Transition('2', a, c),
		  Transition('3', a, d),
		  Transition('1'--'3', b, e),
      Transition('1'--'3', c, e),
		  Transition('1'--'3', d, e),
		  Transition('1', e, f),
		  Transition('2', e, g),
		  Transition('3', e, h),
		  Transition('1'--'3', f, i),
		  Transition('1'--'3', g, i),
		  Transition('1', h, i))

    NFA(transitions, a, Set(i))
	}
	
}
