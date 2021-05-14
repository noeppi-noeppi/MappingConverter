package io.github.noeppi_noeppi.mappings.version

case class McpVersion(channel: String, version: Int, mc: MinecraftVersion) extends MappingVersion {

  override def toString: String = channel + "_" + version + "-" + mc
}
