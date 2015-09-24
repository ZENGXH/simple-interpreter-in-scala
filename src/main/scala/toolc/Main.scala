package toolc

import utils._
import java.io.File

import lexer._
import ast._
import eval._



object Main {

  def processOptions(args: Array[String]): Context = {
    val (opts, files) = args.toSeq.partition(_.startsWith("--"))
    val reporter = new Reporter()

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = new File(files.head))
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

<<<<<<< HEAD
    val program = new Parser().parse(ctx, ctx.files.head)
    println(program)
    //println(ctx)

    val evaluator = new Evaluator(ctx, program)
    
    evaluator.eval()
=======
    val pipeline = Lexer andThen
                   PrintTokens

    val result = pipeline.run(ctx)(ctx.file)
>>>>>>> origin/lab02

    ctx.reporter.terminateIfErrors
      
  }
}




