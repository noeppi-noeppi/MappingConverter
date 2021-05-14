package io.github.noeppi_noeppi.mappings.provider

import io.github.noeppi_noeppi.mappings.mappings.Mappings
import io.github.noeppi_noeppi.mappings.version.MappingVersion

trait MappingProvider[-T <: MappingVersion] {
  
  def provide(v: T): Mappings
}
