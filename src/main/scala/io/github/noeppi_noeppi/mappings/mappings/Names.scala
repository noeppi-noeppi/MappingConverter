package io.github.noeppi_noeppi.mappings.mappings

sealed trait Names

object Obfuscated extends Names {
  override def toString: String = "obfuscated"
}
object SRG extends Names{
  override def toString: String = "srg/intermediary"
}
object Mapped extends Names{
  override def toString: String = "named"
}