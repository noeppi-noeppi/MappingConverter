package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util.{Side, Unknown}
import org.apache.commons.text.StringEscapeUtils

import java.io._
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import scala.collection.mutable

object FormatMCP extends MappingFormat {
  
  override def required: Set[Names] = Set(SRG, Mapped)

  override def read(in: InputStream): Mappings = {
    var fields: Map[String, (String, Side, String)] = null
    var methods: Map[String, (String, Side, String)] = null
    var params: Map[(Int, Int), String] = Map()
    var cparams: Map[(Int, Int), (String, Side)] = Map()
    val zip = new ZipInputStream(in)
    var entry = zip.getNextEntry
    while (entry != null) {
      var name = entry.getName
      if (!name.endsWith("/")) {
        if (name.startsWith("/")) name = name.substring(1)
        name match {
          case "fields.csv" => fields = parseFM(new BufferedReader(new InputStreamReader(zip)), "fields.csv")
          case "methods.csv" => methods = parseFM(new BufferedReader(new InputStreamReader(zip)), "methods.csv")
          case "params.csv" =>
            val result = parseParams(new BufferedReader(new InputStreamReader(zip)))
            params = result._1
            cparams = result._2
          case _ =>
        }
      }
      entry = zip.getNextEntry
    }
    if (fields == null) throw new IllegalStateException("MCP mappings ZIP is missing required 'fields.csv'")
    if (methods == null) throw new IllegalStateException("MCP mappings ZIP is missing required 'methods.csv'")
    val builder = new MappingBuilder(SRG, Mapped)
    for (entry <- fields) {
      builder.uniqueF(entry._1, entry._2._1, entry._2._2, entry._2._3)
    }
    val pmap: Map[Int, Map[Int, String]] = params.groupBy(entry => entry._1._1).map(entry => entry._1 -> entry._2.map(x => (x._1._2, x._2)))
    for (entry <- methods) {
      val id = methodId(entry._1)
      builder.uniqueM(entry._1, entry._2._1, pmap.getOrElse(id, Map()), entry._2._2, entry._2._3)
    }
    cparams.groupBy(entry => entry._1._1).foreach(entry => {
      val pmap = entry._2.map(entry => (entry._1._2, entry._2._1))
      val side = entry._2.map(entry => entry._2._2).fold(Unknown)(Side.join)
      builder.uniqueC(entry._1.toString, entry._1.toString, pmap, side, "")
    })
    builder.build()
  }
  
  private def methodId(name: String): Int = name match {
    case SrgUtil.FUNC_REGEX(id) => id.toInt
    case _ => -1
  }

  private def parseFM(in: BufferedReader, fname: String): Map[String, (String, Side, String)] = {
    // Skip header
    in.readLine()
    val lines = in.lines().toArray.map(_.toString)
    val map = mutable.Map[String, (String, Side, String)]()
    for (line <- lines if line.trim.nonEmpty) {
      val tokens = line.split(",")
      if (tokens.length < 2) throw new IllegalStateException("Invalid mapping in '" + fname + "': '" + line + "'")
      map.put(tokens(0).trim, (tokens(1).trim, if  (tokens.length >= 3) { Side.byMcpIdx(tokens(2).trim) } else { Unknown }, if (tokens.length >= 4) unquote(tokens.slice(3, tokens.length).mkString(",")) else ""))
    }
    map.toMap
  }
  
  private def parseParams(in: BufferedReader): (Map[(Int, Int), String], Map[(Int, Int), (String, Side)]) = {
    // Skip header
    in.readLine()
    val lines = in.lines().toArray.map(_.toString)
    val map = mutable.Map[(Int, Int), String]()
    val cmap = mutable.Map[(Int, Int), (String, Side)]()
    var hasInvalid = false
    for (line <- lines if line.trim.nonEmpty) {
      val tokens = line.split(",").map(_.trim)
      if (tokens.length < 2) throw new IllegalStateException("Invalid mapping in 'params.csv': '" + line + "'")
      val id = if (tokens(0).startsWith("p_i") && tokens(0).endsWith("_")) {
        parseId(tokens(0).substring(3, tokens(0).length - 1))
      } else if (tokens(0).startsWith("p_") && tokens(0).endsWith("_")) {
        parseId(tokens(0).substring(2, tokens(0).length - 1))
      } else {
        null
      }
      if (id == null) {
        if (!hasInvalid) {
          println("Parameter in 'params.csv' is not in SRG style. Skipping: '" + tokens(0) + "'")
          hasInvalid = true
        }
      } else {
        if (tokens(0).startsWith("p_i")) {
          cmap.put(id, (tokens(1), if (tokens.length >= 3) Side.byMcpIdx(tokens(2)) else Unknown))
        } else {
          map.put(id, tokens(1))
        }
      }
    }
    (map.toMap, cmap.toMap)
  }
  
  private def parseId(id: String): (Int, Int) = {
    if (!id.contains('_')) {
      return null
    }
    try {
      (id.substring(0, id.indexOf('_')).toInt, id.substring(id.indexOf('_') + 1, id.length).toInt)
    } catch {
      case _: NumberFormatException => null
    }
  }
  
  override def write(out: OutputStream, mappings: Mappings): Unit = {
    val fields = mutable.Map[String, (String, Side, String)]()
    val methods = mutable.Map[String, (String, Side, String)]()
    val params = mutable.Map[String, (String, Side)]()
    var skipParam = false
    for (entry <- mappings.fieldMappings) {
      val k = entry.name(SRG)
      val v = entry.name(Mapped)
      if (k.name.startsWith("field_")) {
        fields.put(k.name, (v.name, k.side, v.javadoc))
      }
    }
    for (entry <- mappings.methodMappings) {
      val k = entry.name(SRG)
      val v = entry.name(Mapped)
      if (k.name.startsWith("func_")) {
        methods.put(k.name, (v.name, k.side, v.javadoc))
      }
      for (idx <- k.params.indices) {
        val pk = k.params(idx)
        val pv = v.params(idx)
        if (pk.isDefined && pv.isDefined && pk.get.startsWith("p_")) {
          params.put(pk.get, (pv.get, k.side))
        }
      }
    }
    for (entry <- mappings.constructorMappings) {
      val k = entry.name(SRG)
      val v = entry.name(Mapped)
      for (idx <- k.params.indices) {
        val pk = k.params(idx)
        val pv = v.params(idx)
        if (pk.isDefined && pv.isDefined && pk.get.startsWith("p_i")) {
          if (!skipParam) {
            if (params.contains(pk.get)) {
              println("Skipping parameters in MCP output as parameters are not unique. Duplicate constructor parameter: '" + pk.get + "'")
              skipParam = true
            } else {
              params.put(pk.get, (pv.get, k.side))
            }
          }
        }
      }
    }
    for (entry <- mappings.uniqueFieldNames) {
      if (!fields.contains(entry._1)) {
        fields.put(entry._1, entry._2)
      }
    }
    for (entry <- mappings.uniqueMethodNames) {
      if (!methods.contains(entry._1)) {
        methods.put(entry._1, (entry._2._1, entry._2._3, entry._2._4))
      }
      entry._1 match {
        case SrgUtil.FUNC_REGEX(id) =>
          for (param <- entry._2._2) {
            val pname = "p_" + id + "_" + param._1 + "_"
            if (!params.contains(pname)) {
              params.put(pname, (param._2, entry._2._3))
            }
          }
        case _ =>
      }
    }
    for (entry <- mappings.uniqueConstructorNames) {
      entry._1.toIntOption.foreach(id => {
        for (param <- entry._2._2) {
          val pname = "p_i" + id + "_" + param._1 + "_"
          if (!params.contains(pname)) {
            params.put(pname, (param._2, entry._2._3))
          }
        }
      })
    }
    
    // We must close the zip stream to write ZIP END header but we can't close the given stream.
    val bout = new ByteArrayOutputStream()
    val zout = new ZipOutputStream(bout)
    zout.putNextEntry(new ZipEntry("fields.csv"))
    val fw = new BufferedWriter(new OutputStreamWriter(zout))
    fw.write("searge,name,side,desc\n")
    for (entry <- fields) {
      fw.write(entry._1 + "," + entry._2._1 + "," + entry._2._2.mcpIdx.toString + "," + quote(entry._2._3) + "\n")
    }
    fw.flush()
    zout.closeEntry()
    zout.putNextEntry(new ZipEntry("methods.csv"))
    val mw = new BufferedWriter(new OutputStreamWriter(zout))
    mw.write("searge,name,side,desc\n")
    for (entry <- methods) {
      mw.write(entry._1 + "," + entry._2._1 + "," + entry._2._2.mcpIdx.toString + "," + quote(entry._2._3) + "\n")
    }
    mw.flush()
    zout.closeEntry()
    if (!skipParam) {
      zout.putNextEntry(new ZipEntry("params.csv"))
      val pw = new BufferedWriter(new OutputStreamWriter(zout))
      pw.write("param,name,side\n")
      for (entry <- params) {
        pw.write(entry._1 + "," + entry._2._1 + "," + entry._2._2.mcpIdx.toString + "\n")
      }
      pw.flush()
      zout.closeEntry()
    }
    zout.close()
    out.write(bout.toByteArray)
  }
  
  private def unquote(str: String): String = {
    if ((str.startsWith("\"") && str.startsWith("\"")) || (str.startsWith("\'") && str.startsWith("\'"))) {
      StringEscapeUtils.unescapeJava(str.substring(1, str.length - 1))
    } else {
      StringEscapeUtils.unescapeJava(str)
    }
  }
  
  private def quote(str: String): String = {
    if (str.nonEmpty) {
      "\"" + StringEscapeUtils.escapeJava(str) + "\""
    } else {
      ""
    }
  }
}
