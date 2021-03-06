/*
 * Copyright (c) 2013 Jascha Neutelings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package nl.ru.cs.spl.parser

sealed trait TokenTemplate

sealed abstract class WildcardTemplate(val name: String) extends TokenTemplate {
  override def toString = "<" + name + ">"
}

sealed trait Token extends TokenTemplate {
  def ignorable = false
  def matches(template: TokenTemplate): Boolean = this == template
}

sealed abstract class FixedToken(fixedValue: String) extends Token {
  override def toString = "'" + fixedValue + "'"
}

sealed abstract class VariableToken[T](wildcard: WildcardTemplate) extends Token {
  def value: T

  override def matches(template: TokenTemplate) = if (super.matches(template)) true else template match {
    case `wildcard` => true
    case _          => false
  }

  override def toString = "<" + wildcard.name + " : '" + value + "'>"
}

package wildcards {

  case object IntLiteral                     extends WildcardTemplate("integer-literal")
  case object Identifier                     extends WildcardTemplate("identifier")
  case object Comment                        extends WildcardTemplate("comment")
  case object Whitespace                     extends WildcardTemplate("whitespace")
  case object Unknown                        extends WildcardTemplate("unknown")

}

package tokens {

  case object Void                           extends FixedToken("Void")
  case object Int                            extends FixedToken("Int")
  case object Bool                           extends FixedToken("Bool")

  case object If                             extends FixedToken("if")
  case object Else                           extends FixedToken("else")
  case object While                          extends FixedToken("while")
  case object Return                         extends FixedToken("return")

  case object True                           extends FixedToken("True")
  case object False                          extends FixedToken("False")

  final case class IntLiteral(value: BigInt) extends VariableToken[BigInt](wildcards.IntLiteral)
  final case class Identifier(value: String) extends VariableToken[String](wildcards.Identifier)

  case object Assign                         extends FixedToken("=")

  case object Plus                           extends FixedToken("+")
  case object Minus                          extends FixedToken("-")
  case object Multiply                       extends FixedToken("+")
  case object Divide                         extends FixedToken("/")
  case object Modulo                         extends FixedToken("%")

  case object EQ                             extends FixedToken("==")
  case object LT                             extends FixedToken("<")
  case object GT                             extends FixedToken(">")
  case object LE                             extends FixedToken("<=")
  case object GE                             extends FixedToken(">=")
  case object NE                             extends FixedToken("!=")

  case object And                            extends FixedToken("&&")
  case object Or                             extends FixedToken("||")
  case object Not                            extends FixedToken("!")

  case object Cons                           extends FixedToken(":")

  case object LParen                         extends FixedToken("(")
  case object RParen                         extends FixedToken(")")

  case object LSquare                        extends FixedToken("[")
  case object RSquare                        extends FixedToken("]")

  case object LCurly                         extends FixedToken("{")
  case object RCurly                         extends FixedToken("}")

  case object Comma                          extends FixedToken(",")
  case object Semicolon                      extends FixedToken(";")

  final case class Comment(value: String)    extends VariableToken[String](wildcards.Comment)    { override def ignorable = true }
  final case class Whitespace(value: String) extends VariableToken[String](wildcards.Whitespace) { override def ignorable = true }
  case object EndOfFile                      extends FixedToken("")  { override def toString = "<end-of-file>" }

  final case class Unknown(value: Char)      extends VariableToken[Char](wildcards.Unknown)

}

