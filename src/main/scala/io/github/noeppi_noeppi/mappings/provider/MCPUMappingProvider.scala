package io.github.noeppi_noeppi.mappings.provider

import io.github.noeppi_noeppi.mappings.format.FormatMCP
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}
import io.github.noeppi_noeppi.mappings.version.McBuildVersion

import java.net.URL

object MCPUMappingProvider extends MappingProvider[McBuildVersion] {
  
  override def provide(v: McBuildVersion): Mappings = {
    val mappingIn = new URL("https://noeppi-noeppi.github.io/MappingUtilities/mcp_unofficial/" + v.toString + ".zip").openStream()
    val mappings = MappingIO.read(FormatMCP, mappingIn)
    mappingIn.close()
    mappings
  }
}
