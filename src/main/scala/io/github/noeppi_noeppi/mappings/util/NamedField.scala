package io.github.noeppi_noeppi.mappings.util

case class NamedField(cls: ClassEntry, name: String, element: Option[TypeEntry], side: Side, javadoc: String) {

  def known(newSide: Side): NamedField = if (side == Unknown) forceKnown(newSide) else this
  def forceKnown(newSide: Side): NamedField = NamedField(cls, name, element, newSide, javadoc)
  def withElement(e: TypeEntry): NamedField = NamedField(cls, name, Some(e), side, javadoc)
  def renamed(newName: String): NamedField = NamedField(cls, newName, element, side, javadoc)
  
  def requireElement(action: String): TypeEntry = element.getOrElse(throw new IllegalStateException(action + " can not be used with mapping lacking field types."))
}

object NamedField {
  
  def apply(cls: ClassEntry, name: String, element: Option[TypeEntry], side: Side): NamedField = NamedField(cls, name, element, side, "")
  def dup(f1: NamedField, f2: NamedField): Boolean = TypeEntry.dup(f1.cls, f2.cls) && f1.name == f2.name
  def distinct(f: NamedField): NamedField = NamedField(TypeEntry.distinct(f.cls), f.name, None, Unknown, "")
}