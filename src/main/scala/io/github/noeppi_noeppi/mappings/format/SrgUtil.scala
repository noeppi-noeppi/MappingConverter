package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings.{MappingBuilder, PseudoMappingBuilder}
import io.github.noeppi_noeppi.mappings.util.{ClassEntry, MethodSignature, NamedConstructor, NamedMethod, Side, Unknown}

import scala.util.matching.Regex

object SrgUtil {
  
  val FUNC_REGEX: Regex = """func_(\d+)_[a-z]*""".r
  val ANYPARAM_REGEX: Regex = """p_i?(\d+)_(\d+)_""".r
  
  def paramNames(name: String, sig: MethodSignature, ctor: Boolean): List[Option[String]] = name match {
    case FUNC_REGEX(id) => sig.args.indices.map(idx => Some((if (ctor) "p_i" else "p_") + id + "_" + idx + "_")).toList;
    case _ => List.fill(sig.args.size)(None)
  }
  
  def mapMethod(builder: MappingBuilder, m: NamedMethod, name: String): Unit = {
    builder.map(m, name)
    val params = paramNames(name, m.sig, ctor = false)
    for (i <- params.indices if params(i).isDefined) {
      builder.param(m, i, params(i).get)
    }
  }
  
  def mapCtor(builder: MappingBuilder, m: NamedConstructor, name: String): Unit = {
    builder.map(m, name)
    val params = paramNames(name, m.sig, ctor = true)
    for (i <- params.indices if params(i).isDefined) {
      builder.param(m, i, params(i).get)
    }
  }
  
  def mapCtor(builder: PseudoMappingBuilder, cls: ClassEntry, name: String, sig: MethodSignature, side: Side): Unit = {
    val params = paramNames(name, sig, ctor = true)
    builder.map(NamedConstructor(cls, name, sig, params, side, ""))
  }
  
  def mapCtorExc(builder: PseudoMappingBuilder, cls: ClassEntry, sig: MethodSignature, params: List[String], side: Side): Unit = {
    // EXC has no real constructor ids but we'll try to infer it from the parameters.
    val id = inferCtorFromParams(params)
    // If param amount matches the param number fom the signature
    // We take the params given. If not we use classic SRG style named params
    // if we could infer and id.
    if (sig.args.size == params.size) {
      builder.map(NamedConstructor(cls, id.getOrElse("<init>"), sig, params.map(Some(_)), side, ""))
    } else if (id.isDefined) {
      mapCtor(builder, cls, id.get, sig, side)
    }
  }
  
  private def inferCtorFromParams(params: List[String]): Option[String] = {
    var inferred: String = null
    for (param <- params) param match {
      case ANYPARAM_REGEX(id, idx) if inferred == null || inferred == id => inferred = id
      case ANYPARAM_REGEX(id, idx) => return None
      case _ => 
    }
    Option(inferred)
  }
}
