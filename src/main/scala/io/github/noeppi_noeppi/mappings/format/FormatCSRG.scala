package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util.{CommonParsers, NamedField, NamedMethod, Unknown}

import java.io.{BufferedReader, BufferedWriter}

object FormatCSRG extends MappingFormatText {
  
  // Because skip whitespace in CommonParsers we must filter this here
  private val PK_REMAP = """([A-Za-z0-9.]+/)+ [A-Za-z0-9./]+""".r

  override def required: Set[Names] = Set(Obfuscated, SRG)
  
  override def read(in: BufferedReader): Mappings = {
    val builder = new MappingBuilder(Obfuscated, SRG)
    in.lines().toArray.map(_.toString).flatMap[MappingBuilder => Unit] {
      case PK_REMAP(_) => Some(_ => {})
      case x if x.split(" ").count(_.trim.nonEmpty) == 2 => ParserCSRG.parseIt(ParserCSRG.stmt_cl, x)
      case x if x.split(" ").count(_.trim.nonEmpty) == 3 => ParserCSRG.parseIt(ParserCSRG.stmt_fd, x)
      case x if x.split(" ").count(_.trim.nonEmpty) == 4 => ParserCSRG.parseIt(ParserCSRG.stmt_md, x)
      case x => throw new IllegalStateException("CSRG Statement expected, got: '" + x + "'")
    }.foreach(_(builder))
    builder.build()
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("CSRG")
    out.write("./ net/minecraft/src\n")
    mappings.classMappings.foreach(m => {
      out.write(m.name(Obfuscated).internalString + " " + m.name(SRG).internalString + "\n")
    })
    mappings.fieldMappings.foreach(m => {
      out.write(m.name(Obfuscated).cls.internalString + " " + m.name(Obfuscated).name + " " + m.name(SRG).name + "\n")
    })
    mappings.methodMappings.foreach(m => {
      out.write(m.name(Obfuscated).cls.internalString + " " + m.name(Obfuscated).name + " " + m.name(Obfuscated).sig.toString + " " + m.name(SRG).name + "\n")
    })
    out.flush()
  }
}

object ParserCSRG extends CommonParsers {
  
  type X = MappingBuilder => Unit
  
  def stmt_cl: Parser[X] = class_internal ~ class_internal ^^ { case from ~ to => builder => builder.map(from, to) }
  def stmt_fd: Parser[X] = class_internal ~ ident ~ ident ^^ { case fromCls ~ from ~ to => builder => builder.map(NamedField(fromCls, from, None, Unknown), to) }
  def stmt_md: Parser[X] = class_internal ~ ident ~ msig ~ ident ^^ { case fromCls ~ from ~ fromSig  ~ to => builder => SrgUtil.mapMethod(builder, NamedMethod(fromCls, from, fromSig, Unknown), to) }
}

