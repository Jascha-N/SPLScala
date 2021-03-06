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

import java.io.{StringWriter, PrintWriter, File}
import nl.ru.cs.spl.ast.{Analyser, Program}
import nl.ru.cs.spl.codegen.ssm.{SSMPreprocessor, SSMGenerator}
import nl.ru.cs.spl.intermediate.IRProgram
import nl.ru.cs.spl.parser.Parser
import nl.ru.cs.spl.SPLException
import nl.ru.cs.spl.util.{IRDotPrinter, PrettyPrinter, DefaultErrorHandler, ChachingErrorHandler}
import scala.io.Source
import scala.Some
import scala.util.control.Exception._

object Compiler {

  def main(args: Array[String]) {
    new File("examples").listFiles().withFilter(_.getName.endsWith(".spl")).foreach { f =>
    //val f = new File("examples/Example12_TypeErrors.spl"); {
      println(f)

      val output = new PrintWriter("outputs/" + f.getName.replace(".spl", ".txt"))

      def printHeader(s: String) {
        output.println("=========================================================================")
        output.println(s)
        output.println("=========================================================================")
      }

      def executePhase(phaseName: String)(stmt: => Boolean): Boolean = {
        printHeader("PHASE: " + phaseName)
        val tick = System.nanoTime()
        val success = failAsValue(classOf[SPLException]){/* output.println("... [more errors]"); */false}(stmt)
//        try {
//          stmt
//          success = true
//        } catch {
//          case e: SPLException => output.println(e)
//        }
        output.println(phaseName + (if (success) " successful" else " failed") + " (" + (System.nanoTime() - tick) / 1000000.0 + " ms)")
        output.println()
        success
      }

      try {
        val source = Source.fromFile(f).mkString
        val handler = new ChachingErrorHandler(new DefaultErrorHandler(20, output, Some(source)))
        printHeader("INPUT: " + f.getName)
        output.println(source)
        output.println()
        val parser = new Parser(source, handler)
        var program: Program = null
        val success = executePhase("Parsing") {
          program = parser.program()
          val r = !handler.errorOccurred
          handler.flush()
          r
        }
        if (success) {
          val analyser = new Analyser(handler, optimize = false)

          var irProgram: IRProgram = null
          val success = executePhase("Static analysis") {
            irProgram = analyser.analyse(program)
            val r = !handler.errorOccurred
            handler.flush()
            r
          }

          if (success) {
            printHeader("OUTPUT: Type-augmented pretty printed AST")

            val printer = new PrettyPrinter(output, printTypeInfo = true)
            printer.println(irProgram.ast)
            printer.println()

            printHeader("OUTPUT: CFG with Intermediate Code")
            val dotPrinter = new IRDotPrinter(output)
            dotPrinter.print(f.getName.replace(".spl", ""), irProgram)

            printHeader("OUTPUT: SSM Code")
            //val success = executePhase("SSM code generation") {
            val temp = new StringWriter()
            val generator = new SSMGenerator(new PrintWriter(temp))
            generator.print(irProgram)
            //}

            output.write(temp.toString)

            val ssmOutput = new PrintWriter("ssm/" + f.getName.replace(".spl", ".ssm"))
            try {
              val preprocessor = new SSMPreprocessor
              ssmOutput.write(preprocessor.preprocess(temp.toString))
            } finally {
              ssmOutput.close()
            }
          }
        }
      } catch {
        case e: SPLException => output.println("FATAL: " + e.getMessage)
      } finally {
        output.close()
      }
    }
  }

}
