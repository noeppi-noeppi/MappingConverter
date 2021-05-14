package io.github.noeppi_noeppi.mappings.util

import io.github.noeppi_noeppi.mappings.remapper.ClassRemapper

trait Remappable {
  
  def remap(remapper: ClassRemapper): Remappable
}
