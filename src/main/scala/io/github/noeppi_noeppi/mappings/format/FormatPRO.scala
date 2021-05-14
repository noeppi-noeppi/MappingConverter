package io.github.noeppi_noeppi.mappings.format
import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util._

import java.io.{BufferedReader, BufferedWriter}
import scala.collection.mutable

object FormatPRO extends MappingFormatText {
  
  override def required: Set[Names] = Set(Obfuscated, Mapped)

  override def read(in: BufferedReader): Mappings = {
    val builder = new PROMappingBuilder(Mapped, Obfuscated)
    in.lines().toArray.map(_.toString).map(str => if (str.contains("#")) { str.substring(0, str.indexOf('#')) } else { str }).flatMap[PROMappingBuilder => Unit] {
      case x if x.startsWith("\t") || x.startsWith("    ") => ParserPRO.parseIt(ParserPRO.stmt_ct, x)
      case x if x.contains("package-info") || x.contains("module-info") =>Some(_ => {})
      case x => ParserPRO.parseIt(ParserPRO.stmt_cl, x)
    }.foreach(_(builder))
    builder.build()
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("ProGuard")
    val classMap = mutable.Map[ClassEntry, (mutable.Set[Mapping[NamedField]], mutable.Set[Mapping[NamedMethod]])]()
    mappings.classMappings.foreach(m => {
      classMap.put(m.name(Mapped), (mutable.Set(), mutable.Set()))
    })
    mappings.fieldMappings.foreach(m => {
      val id = m.name(Mapped).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set()))
      }
      classMap(id)._1.addOne(m)
    })
    mappings.methodMappings.foreach(m => {
      val id = m.name(Mapped).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set()))
      }
      classMap(id)._2.addOne(m)
    })
    for ((cls, (fields, methods)) <- classMap) {
      out.write(cls.plainString + " -> " + mappings.map(Mapped, Obfuscated, cls).plainString + ":\n")
      for (field <- fields) {
        out.write("    " + field.name(Mapped).requireElement("ProGuard") + " " + field.name(Mapped).name + " -> " + field.name(Obfuscated).name + "\n")
      }
      for (method <- methods) {
        // ProGuard has line numbers that are not important for remapping. So we just set dummy values.
        var nextLine = 1
        out.write("    :" + nextLine + ":" + nextLine + ":" + method.name(Mapped).sig.ret.plainString + " " + method.name(Mapped).name + method.name(Mapped).sig.args.map(_.plainString).mkString("(", ",", ")") + " -> " + method.name(Obfuscated).name + "\n")
        nextLine += 1
      }
    }
    out.flush()
  }
}

object ParserPRO extends CommonParsers {
  
  type X = PROMappingBuilder => Unit
  
  def stmt_cl: ParserPRO.Parser[X] = stmt_cl_r | failure("ProGuard class statement expected")
  def stmt_cl_r: Parser[X] = class_plain ~ "->" ~ class_plain <~ ":" ^^ { case n ~ _ ~ o => builder => builder.map(n, o) }
  
  def stmt_ct: Parser[X] = stmt_init | stmt_fd | stmt_md | failure("ProGuard member statement expected")
  // Ignore constructor and class init remaps as they are useless.
  def stmt_init: Parser[X] = wholeNumber ~ ":" ~> wholeNumber ~ ":" ~> type_plain ~ ("<init>" | "<clinit>") ~ "(" ~ repsep(type_plain_nv, ",") ~ ")" ~ "->" ~ (ident | "<init>" | "<clinit>") ^^ (_ => _ => {})
  def stmt_fd: Parser[X] = type_plain_nv ~ ident ~ "->" ~ ident ^^ { case element ~ name ~ _ ~ obf => builder => builder.map(NamedField(builder.last, name, Some(element), Unknown), obf) }
  def stmt_md: Parser[X] = opt(wholeNumber ~ ":" ~ wholeNumber ~ ":") ~> msig_plain_name ~ "->" ~ ident ^^ { case sig ~ _ ~ obf => builder => builder.map(NamedMethod(builder.last, sig._2, sig._1, Unknown), obf) }
  
  private def msig_plain_name: Parser[(MethodSignature, String)] = type_plain ~ ident ~ "(" ~ repsep(type_plain_nv, ",") <~ ")" ^^ { case ret ~ name ~ _ ~ args => (MethodSignature(ret, args), name) }
}

class PROMappingBuilder(from: Names, to: Names) extends MappingBuilder(from, to) {

  private[this] var lastClass: ClassEntry = _

  override def map(k: ClassEntry, v: ClassEntry): Unit = {
    super.map(k, v)
    lastClass = k
  }

  def last: ClassEntry = {
    if (lastClass == null) {
      throw new IllegalStateException("Invalid ProGuard: field or method mapping before a class mapping.")
    }
    lastClass
  }
}
