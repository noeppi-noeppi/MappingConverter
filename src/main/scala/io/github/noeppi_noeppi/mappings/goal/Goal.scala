package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.mappings.Mappings

trait Goal {

  def perform(env: MappingEnv): Mappings
}
