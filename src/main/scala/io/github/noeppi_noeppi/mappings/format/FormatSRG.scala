package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util.{Both, CommonParsers, NamedField, NamedMethod, Unknown}

import java.io.{BufferedReader, BufferedWriter}

object FormatSRG extends MappingFormatText {
  
  override def required: Set[Names] = Set(Obfuscated, SRG)

  override def read(in: BufferedReader): Mappings = {
    val builder = new MappingBuilder(Obfuscated, SRG)
    in.lines().toArray.map(_.toString).flatMap(ParserSRG.parseIt(ParserSRG.stmt, _)).foreach(_(builder))
    builder.build()
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("SRG")
    out.write("PK: . net/minecraft/src\n")
    mappings.classMappings.foreach(m => {
      out.write("CL: " + m.name(Obfuscated).internalString + " " + m.name(SRG).internalString + "\n")
    })
    mappings.fieldMappings.foreach(m => {
      out.write("FD: " + m.name(Obfuscated).cls.internalString + "/" + m.name(Obfuscated).name + " " + m.name(SRG).cls.internalString + "/" + m.name(SRG).name + "\n")
    })
    mappings.methodMappings.foreach(m => {
      out.write("MD: " + m.name(Obfuscated).cls.internalString + "/" + m.name(Obfuscated).name + " " + m.name(Obfuscated).sig.toString + " " + m.name(SRG).cls.internalString + "/" + m.name(SRG).name + " " + m.name(Obfuscated).sig.toString + "\n")
    })
    out.flush()
  }
}

object ParserSRG extends CommonParsers {
  
  type X = MappingBuilder => Unit
  
  def stmt: Parser[X] = stmt_pk | stmt_cl | stmt_fd | stmt_md | failure("SRG Statement expected")
  
  // package remapping is ignored
  def stmt_pk: Parser[X] = "PK" ~> ":" ~> ".*".r ^^ (_ => _ => {})
  def stmt_cl: Parser[X] = "CL" ~> ":" ~> class_internal ~ class_internal ^^ { case from ~ to => builder => builder.map(from, to) }
  def stmt_fd: Parser[X] = "FD" ~> ":" ~> class_internal_ident ~ class_internal_ident ^^ { case from ~ to => builder => builder.map(NamedField(from._2, from._1, None, Unknown), to._1) }
  def stmt_md: Parser[X] = "MD" ~> ":" ~> class_internal_ident ~ msig ~ class_internal_ident ~ msig ^^ { case from ~ fromSig  ~ to ~ toSig => builder => SrgUtil.mapMethod(builder, NamedMethod(from._2, from._1, fromSig, Unknown), to._1) }
}

