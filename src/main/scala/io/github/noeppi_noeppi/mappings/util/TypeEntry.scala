package io.github.noeppi_noeppi.mappings.util

import io.github.noeppi_noeppi.mappings.remapper.ClassRemapper

import scala.annotation.tailrec

sealed trait TypeEntry extends Remappable {
  
  def string: String
  def plainString: String
  def isVoid: Boolean
  def remap(remapper: ClassRemapper): TypeEntry
}

case class PrimitiveEntry(string: String) extends TypeEntry {

  val plainString: String = string match {
    case "Z" => "boolean"
    case "B" => "byte"
    case "C" => "char"
    case "D" => "double"
    case "F" => "float"
    case "I" => "int"
    case "J" => "long"
    case "S" => "short"
    case "V" => "void"
    case x => throw new IllegalStateException(s"Invalid primitive type: '$x''")
  }
  
  override def isVoid: Boolean = string == "V"
  override def remap(remapper: ClassRemapper): PrimitiveEntry = this
}

case class ClassEntry(name: String, pkg: List[String], side: Side) extends TypeEntry {
  
  override def string: String = "L" + pkg.map(_ + "/").mkString + name + ";"
  override def isVoid: Boolean = false
  override def remap(remapper: ClassRemapper): ClassEntry = remapper.remap(this)
  def internalString: String = pkg.map(_ + "/").mkString + name
  def plainString: String = pkg.map(_ + ".").mkString + name
  
  def known(newSide: Side): ClassEntry = if (side == Unknown) forceKnown(newSide) else this
  def forceKnown(newSide: Side): ClassEntry = ClassEntry(name, pkg, newSide)
}

object ClassEntry {
  
  def apply(name: String, pkg: List[String]): ClassEntry = ClassEntry(name, pkg, Unknown)
}

case class ArrayEntry(element: TypeEntry) extends TypeEntry {
  
  override def string: String = "[" + element.string
  override def plainString: String = element.plainString + "[]"
  override def isVoid: Boolean = false
  override def remap(remapper: ClassRemapper): ArrayEntry = ArrayEntry(element.remap(remapper))
}

object TypeEntry {
  
  private val primitives = Map[String, PrimitiveEntry](
    "Z" -> PrimitiveEntry("Z"),
    "B" -> PrimitiveEntry("B"),
    "C" -> PrimitiveEntry("C"),
    "D" -> PrimitiveEntry("D"),
    "F" -> PrimitiveEntry("F"),
    "I" -> PrimitiveEntry("I"),
    "J" -> PrimitiveEntry("J"),
    "S" -> PrimitiveEntry("S"),
    "V" -> PrimitiveEntry("V")
  )
  
  def parse(cls: String): TypeEntry = {
    if (primitives.contains(cls.trim)) {
      primitives(cls.trim)
    } else if (cls.trim.startsWith("[")) {
      ArrayEntry(parse(cls.trim.substring(1)))
    } else {
      parseClass(cls)
    }
  }
  
  def parseClass(cls: String): ClassEntry = {
    val parts = cls.split('/').map(_.trim)
    if (parts.isEmpty) {
      throw new IllegalStateException(s"Empty class entry: '$cls'")
    }
    if (parts.exists(s => !Character.isJavaIdentifierStart(s.head) || s.tail.exists(!Character.isJavaIdentifierPart(_)))) {
      throw new IllegalStateException(s"Invalid class entry identifier: '$cls'")
    }
    ClassEntry(parts.last, parts.init.toList)
  }

  //noinspection TypeCheckCanBeMatch
  @tailrec
  def dup(t1: TypeEntry, t2: TypeEntry): Boolean = {
    if (t1 == t2) {
      true
    } else if (t1.isInstanceOf[ClassEntry] && t2.isInstanceOf[ClassEntry]) {
      val c1 = t1.asInstanceOf[ClassEntry]
      val c2 = t2.asInstanceOf[ClassEntry]
      c1.name == c2.name && c1.pkg == c2.pkg
    } else if (t1.isInstanceOf[ArrayEntry] && t2.isInstanceOf[ArrayEntry]) {
      dup(t1.asInstanceOf[ArrayEntry].element, t2.asInstanceOf[ArrayEntry].element)
    } else {
      false
    }
  }
  
  def distinct(x: ClassEntry): ClassEntry = ClassEntry(x.name, x.pkg, Unknown)
  
  def distinct(t: TypeEntry): TypeEntry = t match {
    case x: ClassEntry => ClassEntry(x.name, x.pkg, Unknown)
    case x: ArrayEntry => ArrayEntry(distinct(x.element))
    case x => x
  }
}