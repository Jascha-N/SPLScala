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

package nl.ru.cs.spl.intermediate

import nl.ru.cs.spl.ast._

sealed trait Instruction

case class LoadVariable(from: VarDecl) extends Instruction
case class LoadConstant(from: Value) extends Instruction

case class StoreVariable(to: VarDecl, init: Boolean) extends Instruction

case class Discard(tpe: Type) extends Instruction

case object Add extends Instruction
case object Subtract extends Instruction
case object Multiply extends Instruction
case object Divide extends Instruction
case object Modulo extends Instruction
case object Negate extends Instruction
case object And extends Instruction
case object Or extends Instruction
case object Not extends Instruction
case object EQ extends Instruction
case object NE extends Instruction
case object LT extends Instruction
case object LE extends Instruction
case object GT extends Instruction
case object GE extends Instruction
case class Cons(tuple: Boolean, typeInfo: Map[String, Type]) extends Instruction

case class Call(name: String, typeInfo: Map[String, Type]) extends Instruction





