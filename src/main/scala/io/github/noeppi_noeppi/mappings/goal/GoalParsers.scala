package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.format.{FormatCSRG, FormatCTOR, FormatEXC, FormatMCP, FormatMCPC, FormatPRO, FormatSRG, FormatTINY, FormatTINYv2, FormatTSRG, MappingFormat}
import io.github.noeppi_noeppi.mappings.mappings.{Mapped, Names, Obfuscated, SRG}
import io.github.noeppi_noeppi.mappings.provider.{IntermediaryMappingProvider, MCPMappingProvider, MCPPMappingProvider, OfficialMappingProvider, SRGMappingProvider, YarnMappingProvider}
import io.github.noeppi_noeppi.mappings.util.{Client, CommonParsers, Server, Side}
import io.github.noeppi_noeppi.mappings.version.{McpVersion, MinecraftPreRelease, MinecraftRelease, MinecraftReleaseCandidate, MinecraftSnapshot, MinecraftVersion, YarnVersion}

import java.nio.file.{Path, Paths}

object GoalParsers extends CommonParsers {

  private val PATH_REGEX = """[A-Za-z0-9/\\._$\-]+""".r
  
  def stmt: GoalParsers.Parser[(Goal, String)] = stmt_assign | stmt_noassign
  def stmt_assign: Parser[(Goal, String)] = ident ~ "=" ~ goal ^^ { case vname ~ _ ~ goal => (goal, vname) }
  def stmt_noassign: Parser[(Goal, String)] = goal ^^ (x => (x, null))
  
  def goal: Parser[Goal] = goal_merge | goal_safe_merge | goal_transform | goal_reverse | goal_mreverse | goal_apply | goal_partial | goal_obfmap | goal_sided | goal_force | goal_filter | goal_ctor | goal_ftypes | goal_prefix | goal_param_regex | goal_p_mcpp | goal_p_mcp | goal_p_yarn | goal_p_srg | goal_p_intermediary | goal_p_official | goal_output | goal_input | goal_pvar | goal_var | failure("Goal expected")
  def goal_output: Parser[Goal] = "write" ~> "(" ~> goal ~ "," ~ format ~ "," ~ path <~ ")" ^^ { case goal ~ _ ~ format ~ _ ~ path => new OutputGoal(goal, format, path) }
  def goal_input: Parser[Goal] = "read" ~> "(" ~> format ~ "," ~ path <~ ")" ^^ { case format ~ _ ~ path => new ProviderGoalFile(format, path) }
  def goal_var: Parser[Goal] = ident ^^ (x => new ProviderGoalVar(x))
  def goal_pvar: Parser[Goal] = "%" ~> wholeNumber ^^ (x => new ProviderGoalVar("%" + x))
  
  def goal_p_official: Parser[Goal] = "mc" ~> "(" ~> mc_version <~ ")" ^^ (x => new ProviderGoalSpecial(OfficialMappingProvider, x))
  def goal_p_srg: Parser[Goal] = "srg" ~> "(" ~> mc_version <~ ")" ^^ (x => new ProviderGoalSpecial(SRGMappingProvider, x))
  def goal_p_intermediary: Parser[Goal] = "intermediary" ~> "(" ~> mc_version <~ ")" ^^ (x => new ProviderGoalSpecial(IntermediaryMappingProvider, x))
  def goal_p_mcp: Parser[Goal] = "mcp" ~> "(" ~> mcp_version <~ ")" ^^ (x => new ProviderGoalSpecial(MCPMappingProvider, x))
  def goal_p_mcpp: Parser[Goal] = "mcpp" ~> "(" ~> mcp_version <~ ")" ^^ (x => new ProviderGoalSpecial(MCPPMappingProvider, x))
  def goal_p_yarn: Parser[Goal] = "yarn" ~> "(" ~> yarn_version <~ ")" ^^ (x => new ProviderGoalSpecial(YarnMappingProvider, x))

  def goal_merge: Parser[Goal] = "merge" ~> "(" ~> names ~ "," ~ rep1sep(goal, ",") <~ ")" ^^ { case toward ~ _ ~ goals => new MergeGoal(toward, goals: _*) }
  def goal_safe_merge: Parser[Goal] = ("safe_merge" | "safe") ~> "(" ~> names ~ "," ~ names ~ "," ~ goal ~ "," ~ goal <~ ")" ^^ { case toward ~ _ ~ safe ~ _ ~ goal1 ~ _ ~ goal2 => new SafeMergeGoal(toward, safe, goal1, goal2) }
  def goal_transform: Parser[Goal] = "transform" ~> "(" ~> goal ~ "," ~ rep1sep(transformation, ",") <~ ")" ^^ { case goal ~ _ ~ transformations => new TransformGoal(goal, transformations: _*) }
  def goal_reverse: Parser[Goal] = ("reverse" | "rev") ~> "(" ~> goal <~ ")" ^^ (x => new TransformGoal(x, Obfuscated -> SRG, SRG -> Obfuscated))
  def goal_mreverse: Parser[Goal] = ("mapping_reverse" | "mreverse" | "mrev") ~> "(" ~> goal <~ ")" ^^ (x => new TransformGoal(x, SRG -> Mapped, Mapped -> SRG))
  def goal_apply: Parser[Goal] = "apply" ~> "(" ~> goal ~ "," ~ goal <~ ")" ^^ { case srg ~ _ ~ map => new ApplyGoal(srg, map) }
  def goal_partial: Parser[Goal] = ("partial_apply" | "partial") ~> "(" ~> goal ~ "," ~ goal <~ ")" ^^ { case srg ~ _ ~ map => new ApplyPartialGoal(srg, map) }
  def goal_obfmap: Parser[Goal] = ("obf_apply" | "obfmap") ~> "(" ~> goal ~ "," ~ goal <~ ")" ^^ { case srg ~ _ ~ map => new ApplyObfGoal(srg, map) }
  def goal_sided: Parser[Goal] = "sided" ~> "(" ~> goal ~ "," ~ side <~ ")" ^^ { case goal ~ _ ~ side => new SidedGoal(side, goal) }
  def goal_force: Parser[Goal] = ("force_sided" | "force") ~> "(" ~> goal ~ "," ~ side <~ ")" ^^ { case goal ~ _ ~ side => new SidedForceGoal(side, goal) }
  def goal_filter: Parser[Goal] = ("filter_sided" | "filter") ~> "(" ~> goal ~ "," ~ bool ~ "," ~ bool <~ ")" ^^ { case goal ~ _ ~ client ~ _ ~ server => new FilterGoal(client, server, goal) }
  def goal_ctor: Parser[Goal] = ("contructor_apply" | "ctor_apply" | "ctor") ~> "(" ~> names ~ "," ~ goal ~ "," ~ goal <~ ")" ^^ { case common ~ _ ~ mappings ~ _ ~ ctors => new ConstructorGoal(common, mappings, ctors) }
  def goal_ftypes: Parser[Goal] = ("field_types_apply" | "ftypes_apply" | "ftypes") ~> "(" ~> names ~ "," ~ goal ~ "," ~ goal <~ ")" ^^ { case common ~ _ ~ mappings ~ _ ~ ftypes => new FieldTypeGoal(common, mappings, ftypes) }
  def goal_prefix: Parser[Goal] = "prefix" ~> "(" ~> names ~ "," ~ c_string ~ "," ~ goal <~ ")" ^^ { case target ~ _ ~ prefix ~ _ ~ goal => new PrefixGoal(target, prefix, goal) }
  def goal_param_regex: Parser[Goal] = ("param_replace" | "prep") ~> "(" ~> names ~ "," ~ c_string ~ "," ~ c_string ~ "," ~ goal <~ ")" ^^ { case target ~ _ ~ pattern ~ _ ~ replacement ~ _ ~ goal => new RegexParamsGoal(target, pattern, replacement, goal) }
  
  def bool: GoalParsers.Parser[Boolean] = ("true" | "false") ^^ (x => x.toBoolean)
  
  def names: Parser[Names] = names_obf | names_srg | names_mapped
  def names_obf: Parser[Names] = ("obf" | "obfuscated") ^^ (_ => Obfuscated)
  def names_srg: Parser[Names] = ("srg" | "intermediary") ^^ (_ => SRG)
  def names_mapped: Parser[Names] = ("named" | "mapped") ^^ (_ => Mapped)
  
  def transformation: Parser[(Names, Names)] = names ~ "->" ~ names ^^ { case from ~ _ ~ to => if (from == to) throw new IllegalStateException("Identity transformation.") else from -> to }
  
  def side: Parser[Side] = side_client | side_server | side_both
  def side_client: Parser[Side] = "client" ^^ (_ => Client)
  def side_server: Parser[Side] = "server" ^^ (_ => Server)
  def side_both: Parser[Side] = "both" ^^ (_ => Server)
  
  def path: GoalParsers.Parser[Path] = path_str | path_simple
  def path_simple: Parser[Path] = PATH_REGEX ^^ (x => Paths.get(x))
  def path_str: Parser[Path] = escapedStringLiteral ^^ (x => Paths.get(x))
  
  def c_string: GoalParsers.Parser[String] = PATH_REGEX | escapedStringLiteral
  
  def format: Parser[MappingFormat] = format_csrg | format_ctor | format_exc | format_mcpc | format_mcp | format_pro | format_srg | format_tinyv2 | format_tiny | format_tsrg
  def format_csrg: Parser[MappingFormat] = "csrg" ^^ (_ => FormatCSRG)
  def format_ctor: Parser[MappingFormat] = "ctor" ^^ (_ => FormatCTOR)
  def format_exc: Parser[MappingFormat] = "exc" ^^ (_ => FormatEXC)
  def format_mcp: Parser[MappingFormat] = "mcp" ^^ (_ => FormatMCP)
  def format_mcpc: Parser[MappingFormat] = "mcpc" ^^ (_ => FormatMCPC)
  def format_pro: Parser[MappingFormat] = ("proguard" | "pro") ^^ (_ => FormatPRO)
  def format_srg: Parser[MappingFormat] = "srg" ^^ (_ => FormatSRG)
  def format_tiny: Parser[MappingFormat] = "tiny" ^^ (_ => FormatTINY)
  def format_tinyv2: Parser[MappingFormat] = ("tinyv2" | "v2") ^^ (_ => FormatTINYv2)
  def format_tsrg: Parser[MappingFormat] = "tsrg" ^^ (_ => FormatTSRG)
  
  def mc_version: Parser[MinecraftVersion] = mc_rc | mc_pre | mc_release | mc_snapshot
  def mc_release: Parser[MinecraftRelease] = wholeNumber ~ "." ~ wholeNumber ~ opt("." ~ wholeNumber)  ^^ {
    case major ~ _ ~ minor ~ Some(_ ~ release) => MinecraftRelease(major.toInt, minor.toInt, release.toInt)
    case major ~ _ ~ minor ~ None => MinecraftRelease(major.toInt, minor.toInt, 0)
  }
  def mc_rc: Parser[MinecraftReleaseCandidate] = mc_release ~ "-rc" ~ wholeNumber ^^ { case mcv ~ _ ~ build => MinecraftReleaseCandidate(mcv, build.toInt) }
  def mc_pre: Parser[MinecraftPreRelease] = mc_release ~ "-pre" ~ wholeNumber ^^ { case mcv ~ _ ~ build => MinecraftPreRelease(mcv, build.toInt) }
  def mc_snapshot: Parser[MinecraftSnapshot] = wholeNumber ~ "w" ~ wholeNumber ~ ident ^^ { case year ~ _ ~ id ~ build => MinecraftSnapshot(year.toInt, id.toInt, build) }
  
  def mcp_version: Parser[McpVersion] = ("stable" | "snapshot") ~ "_" ~ wholeNumber ~ "-" ~ mc_version ^^ { case channel ~ _ ~ version ~ _ ~ mc => McpVersion(channel, version.toInt, mc) }
  def yarn_version: Parser[YarnVersion] = mc_version ~ "+build." ~ wholeNumber ^^ { case mc ~ _ ~ build => YarnVersion(mc, build.toInt) }
}
