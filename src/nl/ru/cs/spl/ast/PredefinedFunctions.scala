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

package nl.ru.cs.spl.ast

object PredefinedFunctions {
  private val functionNames = Set("print", "println", "printchar", "isEmpty", "head", "tail", "fst", "snd")

  def isPredefined(name: String) = functionNames(name)

  def register(env: Environment) {
    env.declareFunction(FunDecl("print"    , VoidType()                  , Seq(TypeArg(Left("t")))))
    env.declareFunction(FunDecl("println"  , VoidType()                  , Seq.empty))
    env.declareFunction(FunDecl("printchar", VoidType()                  , Seq(IntType())))
    env.declareFunction(FunDecl("isEmpty"  , BoolType()                  , Seq(ListType(TypeArg(Left("t"))))))
    env.declareFunction(FunDecl("head"     , TypeArg(Left("t"))          , Seq(ListType(TypeArg(Left("t"))))))
    env.declareFunction(FunDecl("tail"     , ListType(TypeArg(Left("t"))), Seq(ListType(TypeArg(Left("t"))))))
    env.declareFunction(FunDecl("fst"      , TypeArg(Left("a"))          , Seq(TupleType(TypeArg(Left("a")), TypeArg(Left("b"))))))
    env.declareFunction(FunDecl("snd"      , TypeArg(Left("b"))          , Seq(TupleType(TypeArg(Left("a")), TypeArg(Left("b"))))))
  }

  val optimize: PartialFunction[(String, Seq[Expression]), Expression] = {
    case ("isEmpty", Seq(Literal(NilValue)))         => Literal(False)
    case ("isEmpty", Seq(Literal(_)))                => Literal(True)
    case ("head"   , Seq(Literal(ConsValue(x, _))))  => Literal(x)
    case ("tail"   , Seq(Literal(ConsValue(_, xs)))) => Literal(xs)
    case ("fst"    , Seq(Literal(TupleValue(a, _)))) => Literal(a)
    case ("snd"    , Seq(Literal(TupleValue(_, b)))) => Literal(b)
  }

  def checkValidity(name: String, operands: Seq[Expression]): Option[String] = (name, operands) match {
    case ("head"   , Seq(Literal(NilValue))) => Some("Applying 'head' to an empty list is undefined behaviour")
    case ("tail"   , Seq(Literal(NilValue))) => Some("Applying 'tail' to an empty list is undefined behaviour")
    case _                                   => None
  }
}
