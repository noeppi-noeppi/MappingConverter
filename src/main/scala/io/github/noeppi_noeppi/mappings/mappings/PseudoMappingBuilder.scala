package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.util.NamedConstructor

import scala.collection.mutable

// Builder for mappings with just one Nmes
// These can only have constructor mappings
class PseudoMappingBuilder(val names: Names) {
  
  private val constructors = mutable.Set[NamedConstructor]()
  
  def map(k: NamedConstructor): Unit = {
    if (constructors.contains(k)) {
      throw new IllegalStateException("Duplicate constructor: " + k)
    } else {
      constructors.addOne(k)
    }
  }
  
  def build(): Mappings = {
    Mappings.createPseudo(names, constructors.toSet)
  }
}
