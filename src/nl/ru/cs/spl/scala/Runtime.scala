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

package nl.ru.cs.spl.scala

trait Runtime {

  def main()

  def print(value: Any) {
    println(value)
  }

  def isEmpty[T](list: List[T]) = list.isEmpty

  def head[T](list: List[T]) = list.head

  def tail[T](list: List[T]) = list.tail

  def fst[A, B](tuple: (A, B)) = tuple._1

  def snd[A, B](tuple: (A, B)) = tuple._2

  def main(args: Array[String]) {
    val profile = args.contains("-profile")

    val start = System.currentTimeMillis()
    main()
    val end = System.currentTimeMillis()

    if (profile) Console.out.println("Execution time: "+(end-start)+" ms")
  }

}