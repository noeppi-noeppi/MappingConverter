package io.github.noeppi_noeppi.mappings

import io.github.noeppi_noeppi.mappings.goal.MappingEnv

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters._

object MCM {
  
  def runFile(file: Path, vars: java.util.Map[String, String]): Unit = {
    runFile(file, vars.asScala.toMap)
  }
  
  def runFile(file: Path, vars: Map[String, String]): Unit = {
    val varMap = vars.map(x => ("${" + x._1 + "}", x._2))
    val reader = Files.newBufferedReader(file)
    val lines = reader.lines().toArray.map(_.toString).map(_.trim)
    val env = new MappingEnv
    for (line <- lines if !line.startsWith("#") && line.trim.nonEmpty) {
      var replaced = line.trim
      for ((k, v) <- varMap) {
        replaced = replaced.replace(k, v)
      }
      println(replaced)
      if (!env.run(replaced)) {
        throw new IllegalStateException("Execution of MappingConverter file failed.")
      }
    }
  }
}
