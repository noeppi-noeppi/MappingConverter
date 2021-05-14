package io.github.noeppi_noeppi.mappings.provider

import io.github.noeppi_noeppi.mappings.format.FormatTINYv2
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings}
import io.github.noeppi_noeppi.mappings.version.{MinecraftVersion, YarnVersion}

import java.net.URL
import java.util.zip.ZipInputStream

object YarnMappingProvider extends MappingProvider[YarnVersion] {
  
  override def provide(v: YarnVersion): Mappings = {
    val jarIn = new ZipInputStream(new URL("https://maven.fabricmc.net/net/fabricmc/yarn/" + v.toMavenString + "/yarn-" + v.toMavenString + "-v2.jar").openStream())
    var entry = jarIn.getNextEntry
    while (entry != null) {
      var name = entry.getName
      if (!name.endsWith("/")) {
        if (name.startsWith("/")) name = name.substring(1)
      }
      if (name == "mappings/mappings.tiny") {
        val mappings = MappingIO.read(FormatTINYv2, jarIn)
        jarIn.close()
        return mappings
      }
      entry = jarIn.getNextEntry
    }
    throw new IllegalStateException("Yarn mappings JAR file contains no mappings.")
  }
}
