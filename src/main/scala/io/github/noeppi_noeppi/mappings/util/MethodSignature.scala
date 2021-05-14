package io.github.noeppi_noeppi.mappings.util
import io.github.noeppi_noeppi.mappings.remapper.ClassRemapper

case class MethodSignature(ret: TypeEntry, args: List[TypeEntry]) extends Remappable {
  
  if (args.exists(_.isVoid)) {
    throw new IllegalStateException("A method can't take void types as argument: " + this.toString)
  }
  
  override def toString: String = args.map(_.string).mkString("(", ",", ")") + ret.string
  override def remap(remapper: ClassRemapper): MethodSignature = MethodSignature(ret.remap(remapper), args.map(_.remap(remapper)))
}

object MethodSignature {
  
  def dup(ms1: MethodSignature, ms2: MethodSignature): Boolean = {
    TypeEntry.dup(ms1.ret, ms2.ret) && ms1.args.size == ms2.args.size && (ms1.args zip ms2.args).forall(entry => TypeEntry.dup(entry._1, entry._2))
  }
  def distinct(ms: MethodSignature): MethodSignature = MethodSignature(TypeEntry.distinct(ms.ret), ms.args.map(TypeEntry.distinct))
}
