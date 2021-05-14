package io.github.noeppi_noeppi.mappings.format
import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util.{Client, Server}

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import scala.collection.mutable

// Can read old and new config ZIPs.
object FormatMCPC extends MappingFormat {
  
  override def required: Set[Names] = Set(Obfuscated, SRG)

  override def read(in: InputStream): Mappings = {
    val readFiles = mutable.Map[String, Mappings]()
    val zip = new ZipInputStream(in)
    var entry = zip.getNextEntry
    while (entry != null) {
      var name = entry.getName
      if (!name.endsWith("/")) {
        if (name.startsWith("/")) name = name.substring(1)
        name match {
          case "joined.srg" | "config/joined.srg" => readFiles.put(name, MappingIO.read(FormatSRG, zip))
          case "joined.csrg" | "config/joined.csrg" => readFiles.put(name, MappingIO.read(FormatCSRG, zip))
          case "joined.tsrg" | "config/joined.tsrg" => readFiles.put(name, MappingIO.read(FormatTSRG, zip))
          case "client.srg" | "config/client.srg" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatSRG, zip), Client))
          case "client.csrg" | "config/client.csrg" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatCSRG, zip), Client))
          case "client.tsrg" | "config/client.tsrg" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatTSRG, zip), Client))
          case "server.srg" | "config/server.srg" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatSRG, zip), Server))
          case "server.csrg" | "config/server.csrg" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatCSRG, zip), Server))
          case "server.tsrg" | "config/server.tsrg" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatTSRG, zip), Server))
          case "constructors.txt" | "config/constructors.txt" => readFiles.put(name, MappingIO.read(FormatCTOR, zip))
          case "joined.exc" | "config/joined.exc" => readFiles.put(name, MappingIO.read(FormatEXC, zip))
          case "client.exc" | "config/client.exc" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatEXC, zip), Client))
          case "server.exc" | "config/server.exc" => readFiles.put(name, Mappings.forceKnown(MappingIO.read(FormatEXC, zip), Server))
          case _ =>
        }
      }
      entry = zip.getNextEntry
    }
    val srgMapping = if (readFiles.contains("config/client.tsrg") && readFiles.contains("config/server.tsrg")) {
      Mappings.merge(Obfuscated, None, readFiles("config/client.tsrg"), readFiles("config/server.tsrg"))
    } else if (readFiles.contains("config/client.csrg") && readFiles.contains("config/server.csrg")) {
      Mappings.merge(Obfuscated, None, readFiles("config/client.csrg"), readFiles("config/server.csrg"))
    } else if (readFiles.contains("config/joined.tsrg")) {
      readFiles("config/joined.tsrg")
    } else if (readFiles.contains("config/joined.csrg")) {
      readFiles("config/joined.csrg")
    } else if (readFiles.contains("config/client.srg") && readFiles.contains("config/server.srg")) {
      Mappings.merge(Obfuscated, None, readFiles("config/client.srg"), readFiles("config/server.srg"))
    } else if (readFiles.contains("config/joined.srg")) {
      readFiles("config/joined.srg")
    } else if (readFiles.contains("config/server.tsrg")) {
      readFiles("config/server.tsrg")
    } else if (readFiles.contains("config/client.tsrg")) {
      readFiles("config/client.tsrg")
    } else if (readFiles.contains("config/server.csrg")) {
      readFiles("config/server.csrg")
    } else if (readFiles.contains("config/client.csrg")) {
      readFiles("config/client.csrg")
    } else if (readFiles.contains("config/server.srg")) {
      readFiles("config/server.srg")
    } else if (readFiles.contains("config/client.srg")) {
      readFiles("config/client.srg")
    } else if (readFiles.contains("client.tsrg") && readFiles.contains("server.tsrg")) {
      Mappings.merge(Obfuscated, None, readFiles("client.tsrg"), readFiles("server.tsrg"))
    } else if (readFiles.contains("client.csrg") && readFiles.contains("server.csrg")) {
      Mappings.merge(Obfuscated, None, readFiles("client.csrg"), readFiles("server.csrg"))
    } else if (readFiles.contains("joined.tsrg")) {
      readFiles("joined.tsrg")
    } else if (readFiles.contains("joined.csrg")) {
      readFiles("joined.csrg")
    } else if (readFiles.contains("client.srg") && readFiles.contains("server.srg")) {
      Mappings.merge(Obfuscated, None, readFiles("client.srg"), readFiles("server.srg"))
    } else if (readFiles.contains("joined.srg")) {
      readFiles("joined.srg")
    } else if (readFiles.contains("server.tsrg")) {
      readFiles("server.tsrg")
    } else if (readFiles.contains("client.tsrg")) {
      readFiles("client.tsrg")
    } else if (readFiles.contains("server.csrg")) {
      readFiles("server.csrg")
    } else if (readFiles.contains("client.csrg")) {
      readFiles("client.csrg")
    } else if (readFiles.contains("server.srg")) {
      readFiles("server.srg")
    } else if (readFiles.contains("client.srg")) {
      readFiles("client.srg")
    } else {
      throw new IllegalStateException("No valid mappings found in MCP-CONFIG zip. Tried joined/client/server in formats TSRG/CSRG/SRG")
    }
    
    val ctorMapping = if (readFiles.contains("config/constructors.txt")) {
      readFiles("config/constructors.txt")
    } else if (readFiles.contains("constructors.txt")) {
      readFiles("constructors.txt")
    } else if (readFiles.contains("config/joined.exc")) {
      // There not much point in constructor side information, so we take joined.exc over client and server.
      readFiles("config/joined.exc")
    } else if (readFiles.contains("config/client.exc") && readFiles.contains("config/server.exc")) {
      Mappings.merge(SRG, None, readFiles("config/client.exc"), readFiles("config/server.exc"))
    } else if (readFiles.contains("config/client.exc")) {
      readFiles("config/client.exc")
    } else if (readFiles.contains("config/server.exc")) {
      readFiles("config/server.exc")
    } else if (readFiles.contains("joined.exc")) {
      readFiles("joined.exc")
    } else if (readFiles.contains("client.exc") && readFiles.contains("server.exc")) {
      Mappings.merge(SRG, None, readFiles("client.exc"), readFiles("server.exc"))
    } else if (readFiles.contains("client.exc")) {
      readFiles("client.exc")
    } else if (readFiles.contains("server.exc")) {
      readFiles("server.exc")
    } else {
      null
    }
    
    if (ctorMapping != null) {
      Mappings.applyConstructors(SRG, srgMapping, ctorMapping)
    } else {
      srgMapping
    }
  }

  override def write(out: OutputStream, mappings: Mappings): Unit = {
    // We must close the zip stream to write ZIP END header but we can't close the given stream.
    val bout = new ByteArrayOutputStream()
    val zout = new ZipOutputStream(bout)
    zout.putNextEntry(new ZipEntry("config/"))
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/joined.srg"))
    MappingIO.write(FormatSRG, zout, mappings)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/joined.csrg"))
    MappingIO.write(FormatCSRG, zout, mappings)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/joined.tsrg"))
    MappingIO.write(FormatTSRG, zout, mappings)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/joined.exc"))
    MappingIO.write(FormatEXC, zout, mappings)
    zout.closeEntry()
    
    val client = Mappings.filterSide(mappings, None, requiresClient = true, requiresServer = false)
    zout.putNextEntry(new ZipEntry("config/client.srg"))
    MappingIO.write(FormatSRG, zout, client)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/client.csrg"))
    MappingIO.write(FormatCSRG, zout, client)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/client.tsrg"))
    MappingIO.write(FormatTSRG, zout, client)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/client.exc"))
    MappingIO.write(FormatEXC, zout, client)
    zout.closeEntry()
    
    val server = Mappings.filterSide(mappings, None, requiresClient = false, requiresServer = true)
    zout.putNextEntry(new ZipEntry("config/server.srg"))
    MappingIO.write(FormatSRG, zout, server)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/server.csrg"))
    MappingIO.write(FormatCSRG, zout, server)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/server.tsrg"))
    MappingIO.write(FormatTSRG, zout, server)
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("config/server.exc"))
    MappingIO.write(FormatEXC, zout, server)
    zout.closeEntry()
    
    zout.putNextEntry(new ZipEntry("config/constructors.txt"))
    MappingIO.write(FormatCTOR, zout, mappings)
    zout.closeEntry()
    
    zout.close()
    out.write(bout.toByteArray)
  }
}
