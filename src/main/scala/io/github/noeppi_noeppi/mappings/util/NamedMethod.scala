package io.github.noeppi_noeppi.mappings.util

case class NamedMethod(cls: ClassEntry, name: String, sig: MethodSignature, params: List[Option[String]], side: Side, javadoc: String) {

  if (params.size != sig.args.size) {
    throw new IllegalStateException("Invalid named method: parameter count mismatch")
  }
  
  def known(newSide: Side): NamedMethod = if (side == Unknown) forceKnown(newSide) else this
  def forceKnown(newSide: Side): NamedMethod = NamedMethod(cls, name, sig, params, newSide, javadoc)
  def renamed(newName: String): NamedMethod = if (name == newName) this else NamedMethod(cls, newName, sig, params, side, javadoc)
  def updatedParams(newParams: List[Option[String]]): NamedMethod = if (newParams == params) this else NamedMethod(cls, name, sig, newParams, side, javadoc)
}


object NamedMethod {
  
  def apply(cls: ClassEntry, name: String, sig: MethodSignature, side: Side): NamedMethod = NamedMethod(cls, name, sig, List.fill(sig.args.size)(None), side, "")
  def apply(cls: ClassEntry, name: String, sig: MethodSignature, side: Side, javadoc: String): NamedMethod = NamedMethod(cls, name, sig, List.fill(sig.args.size)(None), side, javadoc)
  def dup(m1: NamedMethod, m2: NamedMethod): Boolean = TypeEntry.dup(m1.cls, m2.cls) && m1.name == m2.name && MethodSignature.dup(m1.sig, m2.sig)
  def distinct(f: NamedMethod): NamedMethod = NamedMethod(TypeEntry.distinct(f.cls), f.name, MethodSignature.distinct(f.sig), Unknown)
}