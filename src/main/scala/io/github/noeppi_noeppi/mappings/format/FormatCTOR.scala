package io.github.noeppi_noeppi.mappings.format
import io.github.noeppi_noeppi.mappings.mappings.{Mappings, Names, PseudoMappingBuilder, SRG}
import io.github.noeppi_noeppi.mappings.util.{CommonParsers, Unknown}

import java.io.{BufferedReader, BufferedWriter}

object FormatCTOR extends MappingFormatText {
  
  override def required: Set[Names] = Set(SRG)

  override def read(in: BufferedReader): Mappings = {
    val builder = new PseudoMappingBuilder(SRG)
    in.lines().toArray.map(_.toString).flatMap(ParserCTOR.parseIt(ParserCTOR.stmt, _)).foreach(_(builder))
    builder.build()
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("SRG-CTOR")
    for (entry <- mappings.constructorMappings) {
      out.write(entry.name(SRG).name + " " + entry.name(SRG).cls.internalString + " " + entry.name(SRG).sig.toString + "\n")
    }
  }
}

object ParserCTOR extends CommonParsers {
  
  type X = PseudoMappingBuilder => Unit
  
  def stmt: Parser[X] = stmt_ctor | failure("SRG-CTOR Statement expected")
  def stmt_ctor: Parser[X] = wholeNumber ~ class_internal ~ msig ^^ { case num ~ cls ~ sig => builder => SrgUtil.mapCtor(builder, cls, num, sig, Unknown) }
}

