package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings.{Mappings, Names, PseudoMappingBuilder, SRG}
import io.github.noeppi_noeppi.mappings.util.{CommonParsers, Unknown}

import java.io.{BufferedReader, BufferedWriter}

// We just parse this for constructor arguments, not exceptions or access.
object FormatEXC extends MappingFormatText {
  
  override def required: Set[Names] = Set(SRG)

  override def read(in: BufferedReader): Mappings = {
    val builder = new PseudoMappingBuilder(SRG)
    in.lines().toArray.map(_.toString).map(str => if (str.contains("#")) { str.substring(0, str.indexOf('#')) } else { str }).flatMap(ParserEXC.parseIt(ParserEXC.stmt, _)).foreach(_(builder))
    builder.build()
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("EXC")
    val maxIdx = mappings.constructorMappings.map(_.name(SRG)).flatMap(_.name.toIntOption).max + 1
    out.write("max_constructor_index=" + maxIdx + "\n")
    for (entry <- mappings.constructorMappings) {
      out.write(entry.name(SRG).cls.internalString + ".<init>" + entry.name(SRG).sig.toString + "=|" + entry.name(SRG).sig.args.indices.map(idx => "p_i" + entry.name(SRG).name + "_" + idx + "_").mkString(",") + "\n")
    }
  }
}

object ParserEXC extends CommonParsers {
  
  type X = PseudoMappingBuilder => Unit
  
  def stmt: Parser[X] = stmt_exc_meta | stmt_exc_ign | stmt_exc | failure("EXC Statement expected")
  
  def stmt_exc_meta: Parser[X] = "max_constructor_index" ~ "=" ~ wholeNumber ^^ (_ => _ => {})
  def stmt_exc: Parser[X] = class_internal ~ "." ~ ("<init>" | ident) ~ msig ~ "=" ~ repsep(class_internal, ",") ~ "|" ~ repsep(ident, ",") ^^ { case cls ~ _ ~ name ~ sig ~ _ ~ _ ~ _ ~ params => builder => if (name == "<init>") { SrgUtil.mapCtorExc(builder, cls, sig, params, Unknown) }}
  def stmt_exc_ign: Parser[X] = class_internal ~ "." ~ ("<init>" | ident) ~ msig ~ "-" ~ ".*".r ^^ (_ => _ => {})
}

