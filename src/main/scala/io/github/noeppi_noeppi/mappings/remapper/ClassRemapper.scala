package io.github.noeppi_noeppi.mappings.remapper

import io.github.noeppi_noeppi.mappings.util.{ClassEntry, TypeEntry}

class ClassRemapper(mappingsRaw: Map[ClassEntry, ClassEntry]) {
  
  private val mappings = mappingsRaw.map(entry => TypeEntry.distinct(entry._1) -> entry._2)
  
  def remap(clazz: ClassEntry): ClassEntry = mappings.getOrElse(clazz, clazz)
  
  def remap(clazz: TypeEntry): TypeEntry = clazz match {
    case x: ClassEntry => remap(x)
    case x => x
  }
}

object ClassRemapper{
  
  val NOOP: ClassRemapper = new ClassRemapper(Map())
}