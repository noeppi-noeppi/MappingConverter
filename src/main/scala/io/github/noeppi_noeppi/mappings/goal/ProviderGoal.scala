package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.format.MappingFormat
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}
import io.github.noeppi_noeppi.mappings.provider.MappingProvider
import io.github.noeppi_noeppi.mappings.version.MappingVersion

import java.nio.file.{Files, Path}

class ProviderGoalFile(format: MappingFormat, path: Path) extends Goal {
  override def perform(env: MappingEnv): Mappings = {
    val in = Files.newInputStream(path)
    val mappings = MappingIO.read(format, in)
    in.close()
    mappings
  }
}

class ProviderGoalVar(name: String) extends Goal {
  override def perform(env: MappingEnv): Mappings = env.variable(name)
}

class ProviderGoalSpecial[T <: MappingVersion](provider: MappingProvider[T], ver: T) extends Goal {
  override def perform(env: MappingEnv): Mappings = provider.provide(ver)
}