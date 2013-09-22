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
import scala.collection.mutable

case class IRProgram(env: Environment, ast: Program, functions: Seq[IRFunction], initialiser: Seq[Instruction])

case class IRFunction(ast: FunDef, env: Environment, blocks: Seq[BasicBlock]) {
//  def reversePostOrder: Seq[BasicBlock] = {
//    val visited = mutable.Set[BasicBlock]()
//
//    def recurse(block: BasicBlock): Seq[BasicBlock] = {
//      visited += block
//
//      block +: entry.successors.toSeq.withFilter(!visited(_)).flatMap(recurse)
//    }
//
//    recurse(entry)
//  }

  def isPolymorphic = env.collectTypeVariables.isEmpty

  def name = ast.name
}

abstract class BasicBlock(id: Option[String]) {
  def successors: Seq[BasicBlock]
  def replaceSuccessor(oldBlock: BasicBlock, newBlock: BasicBlock)

  def predecessors_+=(b: BasicBlock)
  def predecessors_-=(b: BasicBlock)
  def predecessors: Seq[BasicBlock]

  def createName(prefix: String) = prefix + id.fold("")("__" + _)
}

class EntryBlock(private var succ: BasicBlock) extends BasicBlock(None) {
  succ.predecessors_+=(this)

  def successors = Seq(succ)
  def replaceSuccessor(oldBlock: BasicBlock, newBlock: BasicBlock) {
    assert(oldBlock == succ)
    succ = newBlock
  }

  def predecessors_+=(b: BasicBlock) {
    sys.error("Not allowed")
  }
  def predecessors_-=(b: BasicBlock) {
    sys.error("Not allowed")
  }
  def predecessors = Seq()
}

class ExitBlock extends BasicBlock(Some("exit")) {
  def successors = Seq()
  def replaceSuccessor(oldBlock: BasicBlock, newBlock: BasicBlock) {
    sys.error("Not allowed")
  }

  val predecessors = mutable.Buffer[BasicBlock]()
  def predecessors_+=(b: BasicBlock) {
    predecessors += b
  }
  def predecessors_-=(b: BasicBlock) {
    predecessors -= b
  }
}

class NormalBlock(id: String) extends BasicBlock(Some(id)) {
  private var succ = Seq[BasicBlock]()
  val instructions = mutable.Buffer[Instruction]()

  def successors = succ
  def replaceSuccessor(oldBlock: BasicBlock, newBlock: BasicBlock) {
    succ = succ.map{case `oldBlock` => newBlock; case x => x}
  }

  def successor(condition: Boolean) = {
    val Seq(trueBranch, falseBranch) = succ
    if (condition)
      trueBranch
    else
      falseBranch
  }

  def successor = {
    val Seq(s) = succ
    s
  }

  val predecessors = mutable.Buffer[BasicBlock]()
  def predecessors_+=(b: BasicBlock) {
    predecessors += b
  }
  def predecessors_-=(b: BasicBlock) {
    predecessors -= b
  }

  def linkTo(trueBranch: BasicBlock, falseBranch: BasicBlock) {
    succ = Seq(trueBranch, falseBranch)
    trueBranch.predecessors_+=(this)
    falseBranch.predecessors_+=(this)
  }

  def linkTo(to: BasicBlock) {
    succ = Seq(to)
    to.predecessors_+=(this)
  }

  def isBranch = succ.size == 2

  def remove() {
    successors.foreach { s =>
      s.predecessors_-=(this)
      predecessors.foreach(s.predecessors_+=)
    }
    if (succ.size == 1)
      predecessors.foreach(_.replaceSuccessor(this, succ(0)))
  }
}
