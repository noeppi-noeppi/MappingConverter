package io.github.noeppi_noeppi.mappings.provider

import io.github.noeppi_noeppi.mappings.format.FormatMCP
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}
import io.github.noeppi_noeppi.mappings.version.McpVersion

import java.net.URL

object MCPMappingProvider extends MappingProvider[McpVersion] {
  
  override def provide(v: McpVersion): Mappings = {
    val mappingIn = new URL("https://maven.minecraftforge.net/de/oceanlabs/mcp/mcp_" + v.channel + "/" + v.version + "-" + v.mc.toString + "/mcp_" + v.channel + "-" + v.version + "-" + v.mc.toString + ".zip").openStream()
    val mappings = MappingIO.read(FormatMCP, mappingIn)
    mappingIn.close()
    mappings
  }
}
