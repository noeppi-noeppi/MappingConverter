package io.github.noeppi_noeppi.mappings.util

// We need this as mappings can contain names for constructor arguments and side information
// name is a virtual name used by the mappings. For example SRG asigns every constructor a name
case class NamedConstructor(cls: ClassEntry, name: String, sig: MethodSignature, params: List[Option[String]], side: Side, javadoc: String) {
  
  if (params.size != sig.args.size) {
    throw new IllegalStateException("Invalid named constructor: parameter count mismatch")
  }

  def known(newSide: Side): NamedConstructor = if (side == Unknown) forceKnown(newSide) else this
  def forceKnown(newSide: Side): NamedConstructor = NamedConstructor(cls, name, sig, params, newSide, javadoc)
  def updatedParams(newParams: List[Option[String]]): NamedConstructor = if (newParams == params) this else NamedConstructor(cls, name, sig, newParams, side, javadoc)
}

object NamedConstructor {
  
  def apply(cls: ClassEntry, name: String, sig: MethodSignature, side: Side): NamedConstructor = NamedConstructor(cls, name, sig, List.fill(sig.args.size)(None), side, "")
  def apply(cls: ClassEntry, name: String, sig: MethodSignature, side: Side, javadoc: String): NamedConstructor = NamedConstructor(cls, name, sig, List.fill(sig.args.size)(None), side, javadoc)
  // As constructors have no real names, name is ignored here
  def dup(c1: NamedConstructor, c2: NamedConstructor): Boolean = TypeEntry.dup(c1.cls, c2.cls) && MethodSignature.dup(c1.sig, c2.sig)
  def distinct(f: NamedConstructor): NamedConstructor = NamedConstructor(TypeEntry.distinct(f.cls), "<init>", MethodSignature.distinct(f.sig), Unknown)
}