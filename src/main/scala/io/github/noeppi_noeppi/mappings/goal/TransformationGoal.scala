package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.mappings.{Mappings, Names}
import io.github.noeppi_noeppi.mappings.util.Side

class MergeGoal(toward: Names, goals: Goal*) extends Goal {
  override def perform(env: MappingEnv): Mappings = {
    val mappings = goals.map(_.perform(env))
    Mappings.merge(toward, None, mappings: _*)
  }
}

class SafeMergeGoal(toward: Names, safe: Names, goal1: Goal, goal2: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = {
    Mappings.merge(toward, Some(safe), goal1.perform(env), goal2.perform(env))
  }
}

class TransformGoal(goal: Goal, transformations: (Names, Names)*) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.transform(goal.perform(env), transformations: _*)
}

class ApplyGoal(srg: Goal, mappings: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.apply(srg.perform(env), mappings.perform(env))
}

class ApplyPartialGoal(srg: Goal, mappings: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.applyPartial(srg.perform(env), mappings.perform(env))
}

class ApplyObfGoal(srg: Goal, mappings: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.applyObf(srg.perform(env), mappings.perform(env))
}

class SidedGoal(side: Side, goal: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.known(goal.perform(env), side)
}

class SidedForceGoal(side: Side, goal: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.forceKnown(goal.perform(env), side)
}

class FilterGoal(client: Boolean, server: Boolean, goal: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.filterSide(goal.perform(env), None, client, server)
}

class ConstructorGoal(from: Names, mappings: Goal, ctors: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.applyConstructors(from, mappings.perform(env), ctors.perform(env))
}

class FieldTypeGoal(from: Names, mappings: Goal, ftypes: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.applyFieldTypes(from, mappings.perform(env), ftypes.perform(env))
}

class PrefixGoal(target: Names, prefix: String, mappings: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.prefix(target, prefix, mappings.perform(env))
}

class RegexParamsGoal(target: Names, pattern: String, replacement: String, mappings: Goal) extends Goal {
  override def perform(env: MappingEnv): Mappings = Mappings.regexParams(target, pattern.r, replacement, mappings.perform(env))
}