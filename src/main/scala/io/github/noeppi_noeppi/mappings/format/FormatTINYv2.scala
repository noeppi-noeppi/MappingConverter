package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings._
import io.github.noeppi_noeppi.mappings.util._

import java.io.{BufferedReader, BufferedWriter}
import scala.collection.mutable

object FormatTINYv2 extends MappingFormatText {
  
  override def required: Set[Names] = Set(Obfuscated, SRG)

  override def read(in: BufferedReader): Mappings = {
    val header = in.readLine()
    if (header.contains("named")) {
      val builder = new TINYv2nMappingBuilder()
      in.lines().toArray.map(_.toString).flatMap(ParserTINYv2n.parseIt(ParserTINYv2n.stmt, _)).foreach(_(builder))
      builder.build()
    } else {
      val builder = new TINYv2iMappingBuilder(Obfuscated, SRG)
      in.lines().toArray.map(_.toString).flatMap(ParserTINYv2i.parseIt(ParserTINYv2i.stmt, _)).foreach(_(builder))
      builder.build()
    }
  }

  override def write(out: BufferedWriter, mappings: Mappings): Unit = {
    mappings.warnUnique("TINYv2")
    val classMap = mutable.Map[ClassEntry, (mutable.Set[Mapping[NamedField]], mutable.Set[Mapping[NamedConstructor]], mutable.Set[Mapping[NamedMethod]])]()
    mappings.classMappings.foreach(m => {
      classMap.put(m.name(Obfuscated), (mutable.Set(), mutable.Set(), mutable.Set()))
    })
    mappings.fieldMappings.foreach(m => {
      val id = m.name(Obfuscated).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set(), mutable.Set()))
      }
      classMap(id)._1.addOne(m)
    })
    mappings.constructorMappings.foreach(m => {
      val id = m.name(Obfuscated).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set(), mutable.Set()))
      }
      classMap(id)._2.addOne(m)
    })
    mappings.methodMappings.foreach(m => {
      val id = m.name(Obfuscated).cls
      if (!classMap.contains(id)) {
        classMap.put(id, (mutable.Set(), mutable.Set(), mutable.Set()))
      }
      classMap(id)._3.addOne(m)
    })
    val hasNames = mappings.has(Mapped)
    if (hasNames) {
      out.write("tiny\t2\t0\tofficial\tintermediary\tnamed\n")
    } else {
      out.write("tiny\t2\t0\tofficial\tintermediary\n")
    }
    for ((cls, (fields, ctors, methods)) <- classMap) {
      if (hasNames) {
        out.write("c\t" + cls.internalString + "\t" + mappings.map(Obfuscated, SRG, cls).internalString + "\t" + mappings.map(Obfuscated, Mapped, cls).internalString + "\n")
      } else {
        out.write("c\t" + cls.internalString + "\t" + mappings.map(Obfuscated, SRG, cls).internalString + "\n")
      }
      for (field <- fields) {
        if (hasNames) {
          out.write("\tf\t" + field.name(Obfuscated).requireElement("TINYv2").string + "\t" + field.name(Obfuscated).name + "\t" + field.name(SRG).name + "\t" + field.name(Mapped).name + "\n")
        } else {
          out.write("\tf\t" + field.name(Obfuscated).requireElement("TINYv2").string + "\t" + field.name(Obfuscated).name + "\t" + field.name(SRG).name + "\n")
        }
      }
      for (ctor <- ctors) {
        if (hasNames) {
          out.write("\tm\t" + ctor.name(Obfuscated).sig.toString + "\t<init>\t<init>\t<init>\n")
        } else {
          out.write("\tm\t" + ctor.name(Obfuscated).sig.toString + "\t<init>\t<init>\n")
        }
        if (hasNames) {
          val params = ctor.name(Mapped).params
          for (idx <- params.indices) {
            params(idx) match {
              case Some(pname) => out.write("\t\tp\t" + idx + "\t\t\t" + pname + "\n")
              case None =>
            }
          }
        }
      }
      for (method <- methods) {
        if (hasNames) {
          out.write("\tm\t" + method.name(Obfuscated).sig.toString + "\t" + method.name(Obfuscated).name + "\t" + method.name(SRG).name + "\t" + method.name(Mapped).name + "\n")
        } else {
          out.write("\tm\t" + method.name(Obfuscated).sig.toString + "\t" + method.name(Obfuscated).name + "\t" + method.name(SRG).name + "\n")
        }
        if (hasNames) {
          val params = method.name(Mapped).params
          for (idx <- params.indices) {
            params(idx) match {
              case Some(pname) => out.write("\t\tp\t" + idx + "\t\t\t" + pname + "\n")
              case None =>
            }
          }
        }
      }
    }
    out.flush()
  }
}

object ParserTINYv2i extends CommonParsers {
  
  type X = TINYv2iMappingBuilder => Unit
  
  def stmt: Parser[X] = stmt_comment | stmt_cl | stmt_fd | stmt_ct | stmt_md | stmt_pd | failure("TINYv2 Intermediary statement expected")
  
  def stmt_comment: Parser[X] = "#" ~> ".*".r ^^ (_ => _=> {})
  def stmt_cl: Parser[X] = "c" ~> class_internal ~ class_internal ^^ { case obf ~ cls => builder => builder.map(obf, cls) }
  def stmt_fd: Parser[X] = "f" ~> type_entry_nv ~ ident ~ ident ^^ { case element ~ obf ~ name => builder => builder.map(NamedField(builder.last, obf, Some(element), Unknown), name) }
  def stmt_md: Parser[X] = "m" ~> msig ~ ident ~ ident ^^ { case sig  ~ obf ~ name => builder => builder.map(NamedMethod(builder.last, obf, sig, Unknown), name) }
  def stmt_ct: Parser[X] = "m" ~> msig <~ "<init>" <~ "<init>" ^^ (x => builder => builder.map(NamedConstructor(builder.last, "<init>", x, Unknown), "<init>"))
  // Ignore parameters as they have no intermediary names
  def stmt_pd: Parser[X] = "p" ~> wholeNumber ~ ".*".r ^^ (_ => _ => {})
}

object ParserTINYv2n extends CommonParsers {

  type X = TINYv2nMappingBuilder => Unit

  def stmt: Parser[X] = stmt_comment | stmt_cl | stmt_fd | stmt_ct | stmt_md | stmt_pd | failure("TINYv2 Intermediary statement expected")

  def stmt_comment: Parser[X] = "#" ~> ".*".r ^^ (_ => _=> {})
  def stmt_cl: Parser[X] = "c" ~> class_internal ~ class_internal ~ class_internal ^^ { case obf ~ i ~ n => builder => builder.map(obf, i, n) }
  def stmt_fd: Parser[X] = "f" ~> type_entry_nv ~ ident ~ ident ~ ident ^^ { case element ~ obf ~ i ~ n => builder => builder.map(NamedField(builder.last, obf, Some(element), Unknown), i, n) }
  def stmt_md: Parser[X] = "m" ~> msig ~ ident ~ ident ~ ident ^^ { case sig  ~ obf ~ i ~ n => builder => builder.map(NamedMethod(builder.last, obf, sig, Unknown), i, n) }
  def stmt_ct: Parser[X] = "m" ~> msig <~ "<init>" <~ "<init>" <~ "<init>" ^^ (x => builder => builder.map(NamedConstructor(builder.last, "<init>", x, Unknown), "<init>", "<init>"))
  def stmt_pd: Parser[X] = "p" ~> wholeNumber ~ ident ^^ { case idx ~ name => builder => builder.lastParam(idx.toInt, name) }
}

class TINYv2iMappingBuilder(from: Names, to: Names) extends MappingBuilder(from, to) {

  private[this] var lastClass: ClassEntry = _
  private[this] var lastMethod: Either[NamedMethod, NamedConstructor] = _

  override def map(k: ClassEntry, v: ClassEntry): Unit = {
    super.map(k, v)
    lastClass = k
  }
  
  override def map(k: NamedMethod, v: String): Unit = {
    super.map(k, v)
    lastMethod = Left(k)
  }

  override def map(k: NamedConstructor, v: String): Unit = {
    super.map(k, v)
    lastMethod = Right(k)
  }

  def last: ClassEntry = {
    if (lastClass == null) {
      throw new IllegalStateException("Invalid TINYv2: field or method mapping before a class mapping.")
    }
    lastClass
  }
  
  def lastParam(idx: Int, n: String): Unit = {
    if (lastMethod == null) {
      throw new IllegalStateException("Invalid TINYv2: parameter mapping before a method/constructor mapping.")
    }
    lastMethod match {
      case Left(x) => param(x, idx, n)
      case Right(x) => param(x, idx, n)
    }
  }
}

class TINYv2nMappingBuilder extends MultiMappingBuilder {

  private[this] var lastClass: ClassEntry = _
  private[this] var lastMethod: Either[NamedMethod, NamedConstructor] = _

  override def map(k: ClassEntry, s: ClassEntry, v: ClassEntry): Unit = {
    super.map(k, s, v)
    lastClass = k
  }
  
  override def map(k: NamedMethod, i: String, v: String): Unit = {
    super.map(k, i, v)
    lastMethod = Left(k)
  }

  override def map(k: NamedConstructor, i: String, v: String): Unit = {
    super.map(k, i, v)
    lastMethod = Right(k)
  }

  def last: ClassEntry = {
    if (lastClass == null) {
      throw new IllegalStateException("Invalid TINYv2: field or method mapping before a class mapping.")
    }
    lastClass
  }
  
  def lastParam(idx: Int, n: String): Unit = {
    if (lastMethod == null) {
      throw new IllegalStateException("Invalid TINYv2: parameter mapping before a method/constructor mapping.")
    }
    lastMethod match {
      case Left(x) => param(x, idx, x.name + "_" + idx, n)
      case Right(x) => param(x, idx, x.name + "_" + idx, n)
    }
  }
}