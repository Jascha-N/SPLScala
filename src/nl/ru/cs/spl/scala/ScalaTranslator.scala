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

import scala.io.Source
import scala.tools.nsc.{Settings, Global}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.ConsoleReporter

import java.io.{StringWriter, PrintWriter, File}

import nl.ru.cs.spl.parser.Parser

object ScalaTranslator {

  def main(args: Array[String]) {
    val arg = if (args.length == 0) None else Some(args(0))
    val file = new File(arg getOrElse "Example0_Factorial.spl").getCanonicalFile
    val source = Source.fromFile(file)
    val content = source.mkString
    source.close()
    val program = new Parser(content) program()

    val writer = new StringWriter()
    val printer = new ScalaPrinter(new PrintWriter(writer))
    val fileNameNoExt = file.getName.takeWhile(_ != '.')

    printer.printObject(fileNameNoExt, program)
    printer.flush()
    val scalaSource = new BatchSourceFile(fileNameNoExt + ".scala", writer.toString)

    val settings = new Settings()
    settings.outputDirs.setSingleOutput("gen")
    settings.usejavacp.value = true

    val reporter = new ConsoleReporter(settings)
    val compiler = new Global(settings, reporter)
    val run = new compiler.Run()
    run.compileSources(List(scalaSource))

    if (compiler.reporter.hasErrors) {
      sys.exit(1)
    }
  }

}