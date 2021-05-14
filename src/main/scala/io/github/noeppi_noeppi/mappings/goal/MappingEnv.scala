package io.github.noeppi_noeppi.mappings.goal

import io.github.noeppi_noeppi.mappings.mappings.Mappings

import scala.collection.mutable

class MappingEnv {

  private val vars = mutable.Map[String, Mappings]()
  private var nextId = 0
  
  def variable(name: String): Mappings = vars.getOrElseUpdate(name, throw new IllegalStateException("Variable not found: '" + name + "'"))
  
  def run(line: String): Boolean = {
    GoalParsers.parseAll(GoalParsers.stmt, line) match {
      case GoalParsers.Success(x, _) => run(x._1, x._2)
      case GoalParsers.NoSuccess(x, _) => println("Failed to parse statement: " + x); false
    }
  }
  
  def run(goal: Goal, v: String): Boolean = {
    try {
      val mappings = goal.perform(this)
      if (v != null) {
        vars.put(v, mappings)
      }
      vars.put("%" + nextId, mappings)
      nextId += 1
      true
    } catch {
      case e: Exception => println("Failed: " + e.getMessage); e.printStackTrace(); false
    }
  }
}
