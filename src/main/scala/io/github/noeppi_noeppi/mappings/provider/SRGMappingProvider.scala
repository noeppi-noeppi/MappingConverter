package io.github.noeppi_noeppi.mappings.provider

import io.github.noeppi_noeppi.mappings.format.FormatMCPC
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}
import io.github.noeppi_noeppi.mappings.version.{MinecraftRelease, MinecraftSnapshot, MinecraftVersion}

import java.net.URL

object SRGMappingProvider extends MappingProvider[MinecraftVersion] {
  
  // 1.12.2 ist the first version with new TSRG config. In case forge will get snapshot
  // SRGs out we'll use the new URL since the first 1.13 snapshot
  private val OLD_FORMAT = (MinecraftRelease(1, 12, 0), MinecraftSnapshot(17, 43, "a"))
  
  override def provide(v: MinecraftVersion): Mappings = {
    if (v < OLD_FORMAT) {
      try {
        // try CSRG mappings first. If not found, use SRG mappings.
        oldMappingsC(v)
      } catch {
        case e: Exception => oldMappingsS(v)
      }
    } else {
      newMappings(v)
    }
  }
  
  private def oldMappingsC(v: MinecraftVersion): Mappings = {
    val mappingsIn = new URL("https://maven.minecraftforge.net/de/oceanlabs/mcp/mcp/" + v.toString + "/mcp-" + v.toString + "-csrg.zip").openStream()
    val mappings = MappingIO.read(FormatMCPC, mappingsIn)
    mappingsIn.close()
    mappings
  }
  
  private def oldMappingsS(v: MinecraftVersion): Mappings = {
    val mappingsIn = new URL("https://maven.minecraftforge.net/de/oceanlabs/mcp/mcp/" + v.toString + "/mcp-" + v.toString + "-srg.zip").openStream()
    val mappings = MappingIO.read(FormatMCPC, mappingsIn)
    mappingsIn.close()
    mappings
  }
  
  private def newMappings(v: MinecraftVersion): Mappings = {
    val mappingsIn = new URL("https://maven.minecraftforge.net/de/oceanlabs/mcp/mcp_config/" + v.toString + "/mcp_config-" + v.toString + ".zip").openStream()
    val mappings = MappingIO.read(FormatMCPC, mappingsIn)
    mappingsIn.close()
    mappings
  }
}
