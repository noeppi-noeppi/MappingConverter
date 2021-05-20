package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.format.MappingFormat
import io.github.noeppi_noeppi.mappings.mappings.{MappingDiff, MappingIO, Mappings, Names}

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

class DiffOutputGoal(names: Names, from: Goal, to: Goal, path: Path) extends Goal {
  override def perform(env: MappingEnv): Mappings = {
    val f = from.perform(env)
    val t = to.perform(env)
    val absPath = path.toAbsolutePath.normalize()
    if (!Files.exists(absPath.getParent)) {
      Files.createDirectories(absPath.getParent)
    }
    val out = Files.newBufferedWriter(absPath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    MappingDiff.writeDiffReport(names, f, t, out)
    out.close()
    f
  }
}