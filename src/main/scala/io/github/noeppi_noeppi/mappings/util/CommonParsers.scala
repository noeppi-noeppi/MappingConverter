package io.github.noeppi_noeppi.mappings.util

import org.apache.commons.text.StringEscapeUtils

import scala.util.parsing.combinator.JavaTokenParsers

class CommonParsers extends JavaTokenParsers {

  def parseIt[T](parser: Parser[T], line: String): Option[T] = {
    if (line.trim.isEmpty) {
      None
    } else {
      parseAll(parser, line) match {
        case Success(x, _) => Some(x)
        case NoSuccess(msg, _) => throw new IllegalStateException(msg + ": " + line)
      }
    }
  }
  
  def type_entry: Parser[TypeEntry] = primitive_entry | void_entry | class_entry | array_entry | failure("Type entry expected.")
  def type_entry_nv: Parser[TypeEntry] = primitive_entry | class_entry | array_entry | failure("Non-void type entry expected.")
  def class_internal: Parser[ClassEntry] = rep1sep(ident, "/") ^^ (x => ClassEntry(x.last, x.init))
  def class_plain: Parser[ClassEntry] = rep1sep(ident, ".") ^^ (x => ClassEntry(x.last, x.init))
  def msig: Parser[MethodSignature] = "(" ~> rep(type_entry_nv) ~ ")" ~ type_entry ^^ { case args ~ _ ~ ret => MethodSignature(ret, args) }
  
  def class_internal_ident: Parser[(String, ClassEntry)] = rep1sep(ident, "/") ^^ (x => (x.last, ClassEntry(x.init.last, x.init.init)))
  
  private def primitive_entry: Parser[PrimitiveEntry] = ("Z" | "B" | "C" | "D" | "F" | "I" | "J" | "S") ^^ (x => PrimitiveEntry(x))
  private def void_entry: Parser[PrimitiveEntry] = "V" ^^ (x => PrimitiveEntry(x))
  private def class_entry: Parser[ClassEntry] = "L" ~> class_internal <~ ";"
  private def array_entry: Parser[ArrayEntry] = "[" ~> type_entry_nv ^^ (x => ArrayEntry(x))
  
  private def plain_primitive_boolean: Parser[PrimitiveEntry] = "boolean" ^^ (_ => PrimitiveEntry("Z"))
  private def plain_primitive_byte: Parser[PrimitiveEntry] = "byte" ^^ (_ => PrimitiveEntry("B"))
  private def plain_primitive_char: Parser[PrimitiveEntry] = "char" ^^ (_ => PrimitiveEntry("C"))
  private def plain_primitive_double: Parser[PrimitiveEntry] = "double" ^^ (_ => PrimitiveEntry("D"))
  private def plain_primitive_float: Parser[PrimitiveEntry] = "float" ^^ (_ => PrimitiveEntry("F"))
  private def plain_primitive_int: Parser[PrimitiveEntry] = "int" ^^ (_ => PrimitiveEntry("I"))
  private def plain_primitive_long: Parser[PrimitiveEntry] = "long" ^^ (_ => PrimitiveEntry("J"))
  private def plain_primitive_short: Parser[PrimitiveEntry] = "short" ^^ (_ => PrimitiveEntry("S"))
  private def plain_primitive_void: Parser[PrimitiveEntry] = "void" ^^ (_ => PrimitiveEntry("V"))
  
  private def plain_primitive_nv: Parser[PrimitiveEntry] = plain_primitive_boolean | plain_primitive_byte | plain_primitive_char | plain_primitive_double | plain_primitive_float | plain_primitive_int | plain_primitive_long | plain_primitive_short
  private def plain_primitive: Parser[PrimitiveEntry] = plain_primitive_nv | plain_primitive_void
  
  private def plain_type_nav: Parser[TypeEntry] = plain_primitive_nv | class_plain
  private def plain_type_na: Parser[TypeEntry] = plain_primitive | class_plain
  
  def type_plain_nv: Parser[TypeEntry] = plain_type_nav ~ rep("[]") ^^ { case element ~ rep => rep.foldLeft(element)((e, _) => ArrayEntry(e)) }
  def type_plain: Parser[TypeEntry] = plain_type_na ~ rep("[]") ^^ { case element ~ rep => rep.foldLeft(element)((e, _) => ArrayEntry(e)) }
  
  def escapedStringLiteral: Parser[String] = stringLiteral ^^ (x => unquote(x))
  
  private def unquote(quoted: String): String = {
    val raw = if (quoted.startsWith("\"") && quoted.endsWith("\"")) {
      quoted.substring(1, quoted.length - 1)
    } else {
      quoted
    }
    StringEscapeUtils.unescapeJava(raw)
  }
}
