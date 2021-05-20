package io.github.noeppi_noeppi.mappings.util

sealed abstract class Side(val name: String, val client: Boolean, val server: Boolean, val mcpIdx: Int) {
  
  def test(requiresClient: Boolean, requiresServer: Boolean): Boolean = (!requiresClient || client) && (!requiresServer || server)
}

object Unknown extends Side("unknown", true, true, 2)
object Client extends Side("client", true, false, 0)
object Server extends Side("dedicated_server", false, true, 1)
object Both extends Side("both", true, true, 2)

object Side {
  
  def merge(s1: Side, s2: Side): Side = {
    if (s1 == Unknown) {
      s2
    } else {
      s1
    }
  }
  
  def join(s1: Side, s2: Side): Side = {
    if (s1 == Unknown) {
      s2
    } else if (s2 == Unknown) {
      s1
    } else if (s1 == s2) {
      s1
    } else {
      Both
    }
  }
  
  def byMcpIdx(idx: String): Side = {
    try {
      byMcpIdx(idx.toInt)
    } catch {
      case _: NumberFormatException => Both
    }
  }
    
  def byMcpIdx(idx: Int): Side = idx match {
    case 0 => Client
    case 1 => Server
    case _ => Both
  }
}