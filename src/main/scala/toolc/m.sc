
package toolc

import utils._
import java.io.File

import lexer._
import ast._
import eval._

/*
 * this is for the test of the evaluator -- to be del
*/
import ast.Trees._
import utils._
//----------------------

object m {
val args  = Array("Pi.tool")                      //> args  : Array[String] = Array(Pi.tool)
main(args)                                        //> java.io.FileNotFoundException: Pi.tool (�萇�璇𤑳孕����蒴儮�
                                                  //| 	at java.io.FileInputStream.open(Native Method)
                                                  //| 	at java.io.FileInputStream.<init>(FileInputStream.java:146)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:91)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:76)
                                                  //| 	at toolc.lexer.Lexer.lex(Lexer.scala:34)
                                                  //| 	at toolc.ast.Parser.parse(Parser.scala:14)
                                                  //| 	at toolc.m$$anonfun$main$1.main$1(toolc.m.scala:36)
                                                  //| 	at toolc.m$$anonfun$main$1.apply$mcV$sp(toolc.m.scala:20)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at toolc.m$.main(toolc.m.scala:18)
                                                  //| 	at toolc.m.main(toolc.m.scala)
  def processOptions(args: Array[String]): Context = {
    val (opts, files) = args.toSeq.partition(_.startsWith("--"))
    val reporter = new Reporter()

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, files = new File(files.head) :: Nil)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val program = new Parser().parse(ctx, ctx.files.head)
    println(program)
    println(ctx)

    //val evaluator = new Evaluator(ctx, program)
    //evaluator.eval()

  }
}