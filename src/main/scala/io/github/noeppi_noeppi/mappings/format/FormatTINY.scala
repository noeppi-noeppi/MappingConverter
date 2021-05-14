package io.github.noeppi_noeppi.mappings.format
import io.github.noeppi_noeppi.mappings.mappings.{Mapped, MappingBuilder, Mappings, MultiMappingBuilder, Names, Obfuscated, SRG}
import io.github.noeppi_noeppi.mappings.util.{CommonParsers, NamedField, NamedMethod, Unknown}

import java.io.{BufferedReader, BufferedWriter}

object FormatTINY extends MappingFormatText {
  
  override def required: Set[Names] = Set(Obfuscated, SRG)

  override def read(in: BufferedReader): Mappings = {
    val header = in.readLine()
    if (header.contains("named")) {
      val builder = new MultiMappingBuilder()
      in.lines().toArray.map(_.toString).flatMap(ParserTINYn.parseIt(ParserTINYn.stmt, _)).foreach(_(builder))
      builder.build()
    } else {
      val builder = new MappingBuilder(Obfuscated, SRG)
      in.lines().toArray.map(_.toString).flatMap(ParserTINYi.parseIt(ParserTINYi.stmt, _)).foreach(_(builder))
      builder.build()
    }
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("TINY")
    val hasNames = mappings.has(Mapped)
    if (hasNames) {
      out.write("v1\tofficial\tintermediary\tnamed\n")
    } else {
      out.write("v1\tofficial\tintermediary\n")
    }
    mappings.classMappings.foreach(m => {
      if (hasNames) {
        out.write("CLASS\t" + m.name(Obfuscated).internalString + "\t" + m.name(SRG).internalString + "\t" + m.name(Mapped).internalString + "\n")
      } else {
        out.write("CLASS\t" + m.name(Obfuscated).internalString + "\t" + m.name(SRG).internalString + "\n")
      }
    })
    mappings.fieldMappings.foreach(m => {
      if (hasNames) {
        out.write("FIELD\t" + m.name(Obfuscated).cls.internalString + "\t" + m.name(Obfuscated).requireElement("TINY").string + "\t" + m.name(Obfuscated).name + "\t" + m.name(SRG).name + "\t" + m.name(Mapped).name + "\n")
      } else {
        out.write("FIELD\t" + m.name(Obfuscated).cls.internalString + "\t" + m.name(Obfuscated).requireElement("TINY").string + "\t" + m.name(Obfuscated).name + "\t" + m.name(SRG).name + "\n")
      }
    })
    mappings.methodMappings.foreach(m => {
      if (hasNames) {
        out.write("METHOD\t" + m.name(Obfuscated).cls.internalString + "\t" + m.name(Obfuscated).sig.toString + "\t" + m.name(Obfuscated).name + "\t" + m.name(SRG).name + "\t" + m.name(Mapped).name + "\n")
      } else {
        out.write("METHOD\t" + m.name(Obfuscated).cls.internalString + "\t" + m.name(Obfuscated).sig.toString + "\t" + m.name(Obfuscated).name + "\t" + m.name(SRG).name + "\n")
      }
    })
    out.flush()
  }
}

object ParserTINYi extends CommonParsers {
  
  type X = MappingBuilder => Unit
  
  def stmt: Parser[X] = stmt_comment | stmt_cl | stmt_fd | stmt_md | failure("TINY Intermediary statement expected")
  
  def stmt_comment: Parser[X] = "#" ~> ".*".r ^^ (_ => _=> {})
  def stmt_cl: Parser[X] = "CLASS" ~> class_internal ~ class_internal ^^ { case obf ~ cls => builder => builder.map(obf, cls) }
  def stmt_fd: Parser[X] = "FIELD" ~> class_internal ~ type_entry_nv ~ ident ~ ident ^^ { case cls ~ element ~ obf ~ name => builder => builder.map(NamedField(cls, obf, Some(element), Unknown), name) }
  def stmt_md: Parser[X] = "METHOD" ~> class_internal ~ msig ~ ident ~ ident ^^ { case cls ~ sig  ~ obf ~ name => builder => builder.map(NamedMethod(cls, obf, sig, Unknown), name) }
}

object ParserTINYn extends CommonParsers {
  
  type X = MultiMappingBuilder => Unit
  
  def stmt: Parser[X] = stmt_comment | stmt_cl | stmt_fd | stmt_md | failure("TINY Named statement expected")
  
  def stmt_comment: Parser[X] = "#" ~> ".*".r ^^ (_ => _=> {})
  def stmt_cl: Parser[X] = "CLASS" ~> class_internal ~ class_internal ~ class_internal ^^ { case obf ~ cls ~ named => builder => builder.map(obf, cls, named) }
  def stmt_fd: Parser[X] = "FIELD" ~> class_internal ~ type_entry_nv ~ ident ~ ident ~ ident ^^ { case cls ~ element ~ obf ~ intermediary ~ name => builder => builder.map(NamedField(cls, obf, Some(element), Unknown), intermediary, name) }
  def stmt_md: Parser[X] = "METHOD" ~> class_internal ~ msig ~ ident ~ ident ~ ident ^^ { case cls ~ sig  ~ obf ~ intermediary ~ name => builder => builder.map(NamedMethod(cls, obf, sig, Unknown), intermediary, name) }
}