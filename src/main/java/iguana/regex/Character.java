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

package iguana.regex;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * 
 * @author Ali Afroozeh
 *
 */
public class Character extends AbstractRegularExpression {
	
	private static final long serialVersionUID = 1L;
	
	private final int c;

	private Character(Builder builder) {
		super(builder);
		this.c = builder.c;
	}	
	
	public static Character from(int c) {
		return new Builder(c).build();
	}
	
	public int getValue() {
		return c;
	}

	@Override
	public int hashCode() {
		return c;
	}

	@Override
	public boolean equals(Object obj) {
		
		if (this == obj)
			return true;

		if (!(obj instanceof Character))
			return false;
		
		Character other = (Character) obj;
		
		return c == other.c;
	}

	public static String getName(int c) {
		if(CharacterRanges.isPrintableAscii(c)) {
			return (char) c + "";			
		} else {
			String s = "\\u" + String.format("%04X", c);
			// Escape newline inside strings
			return s.equals("\\u000D") || s.equals("\\u000A") ? "\\" + s  : s;
		}
	}
	
	@Override
	public boolean isNullable() {
		return false;
	}
	
	@Override
	public Set<CharacterRange> getFirstSet() {
		Set<CharacterRange> firstSet = new HashSet<>();
		firstSet.add(CharacterRange.in(c, c));
		return firstSet;
	}
	
	@Override
	public Set<CharacterRange> getNotFollowSet() {
		return Collections.emptySet();
	}

    @Override
    public int length() {
        return 1;
    }

    public static Builder builder(int c) {
		return new Builder(c);
	}
	
	@Override
	public RegexBuilder<Character> copyBuilder() {
		return new Builder(this);
	}

	public static class Builder extends RegexBuilder<Character> {
		
		private int c;
		
		public Builder(int c) {
			super(getName(c));
			this.c = c; 
		}

		public Builder(Character character) {
			super(character);
			this.c = character.c;
		}
		
		@Override
		public Character build() {
			return new Character(this);
		}
	}
	
	@Override
	public <T> T accept(RegularExpressionVisitor<T> visitor) {
		return visitor.visit(this);
	}
}
