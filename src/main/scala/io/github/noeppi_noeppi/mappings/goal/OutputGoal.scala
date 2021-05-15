package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.format.MappingFormat
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}

import java.nio.file.{Files, Path, StandardOpenOption}

class OutputGoal(goal: Goal, format: MappingFormat, path: Path) extends Goal {
  override def perform(env: MappingEnv): Mappings = {
    val mappings = goal.perform(env)
    val absPath = path.toAbsolutePath.normalize()
    if (!Files.exists(absPath.getParent)) {
      Files.createDirectories(absPath.getParent)
    }
    val out = Files.newOutputStream(absPath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    MappingIO.write(format, out, mappings)
    out.close()
    mappings
  }
}
