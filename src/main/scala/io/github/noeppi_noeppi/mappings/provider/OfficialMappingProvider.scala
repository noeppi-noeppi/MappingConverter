package io.github.noeppi_noeppi.mappings.provider

import com.google.gson.{JsonObject, JsonParser}
import io.github.noeppi_noeppi.mappings.format.FormatPRO
import io.github.noeppi_noeppi.mappings.mappings.{MappingIO, Mappings, Obfuscated}
import io.github.noeppi_noeppi.mappings.version.MinecraftVersion

import java.io.InputStreamReader
import java.net.URL
import io.github.noeppi_noeppi.mappings.i._
import io.github.noeppi_noeppi.mappings.util.Server

object OfficialMappingProvider extends MappingProvider[MinecraftVersion] {
  
  override def provide(v: MinecraftVersion): Mappings = {
    val manifest = findLauncherJSON(v)
    val serverMappingsIn = new URL(manifest.getAsJsonObject("downloads").getAsJsonObject("server_mappings").get("url").getAsString).openStream()
    val serverMappings = Mappings.forceKnown(MappingIO.read(FormatPRO, serverMappingsIn), Server)
    serverMappingsIn.close()
    val clientMappingsIn = new URL(manifest.getAsJsonObject("downloads").getAsJsonObject("client_mappings").get("url").getAsString).openStream()
    val clientMappings = Mappings.forceKnown(MappingIO.read(FormatPRO, clientMappingsIn), Server)
    clientMappingsIn.close()
    Mappings.merge(Obfuscated, None, serverMappings, clientMappings)
  }
  
  private def findLauncherJSON(v: MinecraftVersion): JsonObject = {
    val versionManifestIn = new InputStreamReader(new URL("https://launchermeta.mojang.com/mc/game/version_manifest_v2.json").openStream())
    val versionManifest = JsonParser.parseReader(versionManifestIn)
    versionManifestIn.close()
    val versionList = versionManifest.getAsJsonObject.get("versions").getAsJsonArray
    for (ve <- versionList; version = ve.getAsJsonObject) {
      if (version.get("id").getAsString == v.toString) {
        val manifestIn = new InputStreamReader(new URL(version.get("url").getAsString).openStream())
        val manifest = JsonParser.parseReader(manifestIn)
        manifestIn.close()
        return manifest.getAsJsonObject
      }
    }
    throw new IllegalStateException("Version not found: " + v.toString)
  }
}
