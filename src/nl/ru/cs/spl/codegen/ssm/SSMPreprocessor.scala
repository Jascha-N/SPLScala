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

package nl.ru.cs.spl.codegen.ssm

import scala.io.Source
import org.apache.commons.lang3.StringEscapeUtils
import java.io.File

class SSMPreprocessor(annotate: Boolean = false, referenceCounting: Boolean = false) {

  def preprocess(input: String): String =
    Seq(
      (s: String) => "(?m)\\s*(?:(?:;|(?://)).*)?$".r.replaceAllIn(s, ""),
      (s: String) => "(?m)^(\\s*)#((?:release)|(?:addref)|(?:addref_right)|(?:addref_left))\\s+([-]?\\d+)\\s*\n".r.replaceAllIn(s, m =>
        if (referenceCounting) s"${m.group(1)}lds ${m.group(3)}\n${m.group(1)}bsr __${m.group(2)}\n" else ""
      ),
      (s: String) => "(?m)^(\\s*)#print\\s+\"((?:[^\"\\\\]|\\\\.)*)\"\\s*\n".r.replaceAllIn(s, m =>
        StringEscapeUtils.unescapeJava(m.group(2)).map(c => s"${m.group(1)}ldc ${c.toInt}\n${m.group(1)}trap 1\n").mkString
      ),
      (s: String) => "(?m)^\\s*#include\\s+\"([^\"]*)\"\\s*$".r.replaceAllIn(s, m =>
        preprocess(Source.fromFile(new File("include", m.group(1))).mkString + "\n")
      )
    ).foldLeft(input)((s, f) => f(s))


  def main(args: Array[String]) {
    print(preprocess(Source.fromFile("test").mkString))
  }

}
