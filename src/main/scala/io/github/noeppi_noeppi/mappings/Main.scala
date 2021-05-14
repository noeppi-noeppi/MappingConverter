package io.github.noeppi_noeppi.mappings

import io.github.noeppi_noeppi.mappings.goal.MappingEnv

import java.nio.file.Paths
import scala.io.StdIn

object Main extends App {
  
  private val R_VAR = """([A-Za-z][A-Za-z0-9]*)\s*=(.*)""".r
  
  if (args.isEmpty) {
    val env = new MappingEnv
    while (true) {
      val line = StdIn.readLine(" > ")
      if (line == null || line.trim == "exit") {
        println()
        System.exit(0)
      }
      env.run(line)
    }
  } else {
    val path = Paths.get(args.head)
    val varMap = args.tail.map{
      case R_VAR(k, v) => (k, v)
      case x => throw new IllegalStateException("Invalid variable: '" + x + "'")
    }.toMap
    MCM.runFile(path, varMap)
  }
}