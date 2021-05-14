package io.github.noeppi_noeppi.mappings.version

sealed trait MinecraftVersion extends MappingVersion {
  
  def < (other: (MinecraftVersionComparable, MinecraftSnapshot)): Boolean = this match {
    case v: MinecraftVersionComparable => v < other._1
    case v: MinecraftSnapshot => v < other._2
  }
  
  def > (other: (MinecraftVersionComparable, MinecraftSnapshot)): Boolean = this match {
    case v: MinecraftVersionComparable => v > other._1
    case v: MinecraftSnapshot => v > other._2
  }
}

sealed trait MinecraftVersionComparable extends MinecraftVersion {
  def < (other: MinecraftVersionComparable): Boolean = {
    val (releaseThis, typeThis, buildThis) = this match {
      case x: MinecraftRelease => (x, 2, 0)
      case x: MinecraftReleaseCandidate => (x.release, 1, x.build)
      case x: MinecraftPreRelease => (x.release, 0, x.build)
    }
    val (releaseOther, typeOther, buildOther) = other match {
      case x: MinecraftRelease => (x, 2, 0)
      case x: MinecraftReleaseCandidate => (x.release, 1, x.build)
      case x: MinecraftPreRelease => (x.release, 0, x.build)
    }
    if (releaseThis.major == releaseOther.major) {
      if (releaseThis.minor == releaseOther.minor) {
        if (releaseThis.release == releaseOther.release) {
          if (typeThis == typeOther) {
            buildThis < buildOther
          } else {
            typeThis < typeOther
          }
        } else {
          releaseThis.release < releaseOther.release
        }
      } else {
        releaseThis.minor < releaseOther.minor
      }
    } else {
      releaseThis.major < releaseOther.major
    }
  }
  
  def > (other: MinecraftVersionComparable): Boolean = other < this
  def <= (other: MinecraftVersionComparable): Boolean = this == other || this < other
  def >= (other: MinecraftVersionComparable): Boolean = this == other || this > other
}

case class MinecraftRelease(major: Int, minor: Int, release: Int) extends MinecraftVersionComparable {
  override def toString: String = if (release == 0) "" + major + "." + minor else "" + major + "." + minor + "." + release
}

case class MinecraftSnapshot(year: Int, version: Int, build: String) extends MinecraftVersion {
  override def toString: String = "" + year + "w" + version + build
  
  def < (other: MinecraftSnapshot): Boolean = {
    if (this.year == other.year) {
      if (this.version == other.version) {
        this.build.toLowerCase < other.build.toLowerCase
      } else {
        this.version < other.version
      }
    } else {
      this.year < other.year
    }
  }
  
  def > (other: MinecraftSnapshot): Boolean = other < this
  def <= (other: MinecraftSnapshot): Boolean = this == other || this < other
  def >= (other: MinecraftSnapshot): Boolean = this == other || this > other
}

case class MinecraftReleaseCandidate(release: MinecraftRelease, build: Int) extends MinecraftVersionComparable {
  override def toString: String = release.toString + "-rc" + build
}

case class MinecraftPreRelease(release: MinecraftRelease, build: Int) extends MinecraftVersionComparable {
  override def toString: String = release.toString + "-pre" + build
}