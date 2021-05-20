package io.github.noeppi_noeppi.mappings.version

case class McBuildVersion(mc: MinecraftVersion, build: Int) extends MappingVersion {
  
  override def toString: String = mc.toString + "-" + build
}
