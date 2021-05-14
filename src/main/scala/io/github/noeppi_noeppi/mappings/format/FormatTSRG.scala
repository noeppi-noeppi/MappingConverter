package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util._

import java.io.{BufferedReader, BufferedWriter}
import scala.collection.mutable

object FormatTSRG extends MappingFormatText {
  
  // Because skip whitespace in CommonParsers we must filter this here
  private val PK_REMAP = """([A-Za-z0-9.]+/)+ [A-Za-z0-9./]+""".r

  override def required: Set[Names] = Set(Obfuscated, SRG)
  
  override def read(in: BufferedReader): Mappings = {
    val builder = new TSRGMappingBuilder(Obfuscated, SRG)
    in.lines().toArray.map(_.toString).flatMap[TSRGMappingBuilder => Unit] {
      case PK_REMAP(_) => Some(_ => {})
      case x if x.startsWith("\t") && x.split(" ").count(_.trim.nonEmpty) == 2 => ParserTSRG.parseIt(ParserTSRG.stmt_fd, x)
      case x if x.startsWith("\t") && x.split(" ").count(_.trim.nonEmpty) == 3 => ParserTSRG.parseIt(ParserTSRG.stmt_md, x)
      case x if x.split(" ").count(_.trim.nonEmpty) == 2 => ParserTSRG.parseIt(ParserTSRG.stmt_cl, x)
      case x => throw new IllegalStateException("TSRG Statement expected, got: '" + x + "'")
    }.foreach(_(builder))
    builder.build()
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("TSRG")
    val classMap = mutable.Map[ClassEntry, (mutable.Set[Mapping[NamedField]], mutable.Set[Mapping[NamedMethod]])]()
    mappings.classMappings.foreach(m => {
      classMap.put(m.name(Obfuscated), (mutable.Set(), mutable.Set()))
    })
    mappings.fieldMappings.foreach(m => {
      val id = m.name(Obfuscated).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set()))
      }
      classMap(id)._1.addOne(m)
    })
    mappings.methodMappings.foreach(m => {
      val id = m.name(Obfuscated).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set()))
      }
      classMap(id)._2.addOne(m)
    })
    for ((cls, (fields, methods)) <- classMap) {
      out.write(cls.internalString + " " + mappings.map(Obfuscated, SRG, cls).internalString + "\n")
      for (field <- fields) {
        out.write("\t" + field.name(Obfuscated).name + " " + field.name(SRG).name + "\n")
      }
      for (method <- methods) {
        out.write("\t" + method.name(Obfuscated).name + " " + method.name(Obfuscated).sig.toString + " " + method.name(SRG).name + "\n")
      }
    }
    out.flush()
  }
}

object ParserTSRG extends CommonParsers {
  
  type X = TSRGMappingBuilder => Unit
  
  def stmt_cl: Parser[X] = class_internal ~ class_internal ^^ { case from ~ to => builder => builder.map(from, to) }
  def stmt_fd: Parser[X] = ident ~ ident ^^ { case from ~ to => builder => builder.map(NamedField(builder.last, from, None, Unknown), to) }
  def stmt_md: Parser[X] = ident ~ msig ~ ident ^^ { case from ~ fromSig  ~ to => builder => SrgUtil.mapMethod(builder, NamedMethod(builder.last, from, fromSig, Unknown), to) }
}

class TSRGMappingBuilder(from: Names, to: Names) extends MappingBuilder(from, to) {
  
  private[this] var lastClass: ClassEntry = _

  override def map(k: ClassEntry, v: ClassEntry): Unit = {
    super.map(k, v)
    lastClass = k
  }
  
  def last: ClassEntry = {
    if (lastClass == null) {
      throw new IllegalStateException("Invalid TSRG: field or method mapping before a class mapping.")
    }
    lastClass
  } 
}
