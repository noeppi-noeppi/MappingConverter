package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.format.MappingFormat
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}

import java.nio.file.{Files, Path}

class OutputGoal(goal: Goal, format: MappingFormat, path: Path) extends Goal {
  override def perform(env: MappingEnv): Mappings = {
    val mappings = goal.perform(env)
    val out = Files.newOutputStream(path)
    MappingIO.write(format, out, mappings)
    out.close()
    mappings
  }
}
