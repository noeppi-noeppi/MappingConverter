package io.github.noeppi_noeppi.mappings.version

case class YarnVersion(mc: MinecraftVersion, build: Int) extends MappingVersion {
  
  // Pre releases before 1.14.2 had a different name format. That's why toMavenString exists.
  override def toString: String = mc.toString + "+build." + build
  def toMavenString: String = mc match {
    case v: MinecraftPreRelease if v <= MinecraftRelease(1, 14, 2) => v.release.toString + " Pre-Release " + v.build + "+build." + build
    case _ => toString
  }
}