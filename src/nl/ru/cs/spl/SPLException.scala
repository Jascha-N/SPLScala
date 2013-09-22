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

package nl.ru.cs.spl

import nl.ru.cs.spl.parser.{Position, Positional}

class SPLException(val message: String,
                   val position: Option[Position] = None,
                   val cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)
                                                     with Ordered[SPLException] {
  def this(message: String, positional: Positional) = this(message, positional.position)
  def this(message: String, position: Position) = this(message, Some(position))
  def this(message: String, cause: Throwable) = this(message, None, Option(cause))

  def withPosition(position: Position) = new SPLException(message, Some(position))
  def withPosition(positional: Positional) = new SPLException(message, positional.position)

  def compare(that: SPLException): Int = position.compare(that.position)
}