package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.util.{ClassEntry, NamedConstructor, NamedField, NamedMethod, Side}

import scala.collection.mutable

class MappingBuilder(val from: Names, val to: Names) {
  
  private val classes = mutable.Map[ClassEntry, ClassEntry]()
  private val mappedClasses = mutable.Set[ClassEntry]()
  private val fields = mutable.Map[NamedField, (String, String)]()
  
  private val methods = mutable.Map[NamedMethod, (String, String)]()
  private val parameters = mutable.Map[NamedMethod, mutable.Map[Int, String]]()
  private val uniqueParameters = mutable.Map[String, mutable.Map[Int, String]]()
  
  private val constructors = mutable.Map[NamedConstructor, (String, String)]()
  private val cparameters = mutable.Map[NamedConstructor, mutable.Map[Int, String]]()
  private val cuniqueParameters = mutable.Map[String, mutable.Map[Int, String]]()
  
  private val uniqueFields = mutable.Map[String, (String, Side, String)]()
  private val uniqueMethods = mutable.Map[String, (String, Map[Int, String], Side, String)]()
  private val uniqueConstructors = mutable.Map[String, (String, Map[Int, String], Side, String)]()
  
  def map(k: ClassEntry, v: ClassEntry): Unit = {
    if (classes.contains(k)) {
      throw new IllegalStateException("Duplicate class key: " + k)
    } else if (mappedClasses.contains(v)) {
      throw new IllegalStateException("Duplicate class value: " + v)
    } else {
      classes.put(k, v)
      mappedClasses.addOne(v)
    }
  }
  
  def map(k: NamedField, v: String): Unit = map(k, v, "")

  def map(k: NamedField, v: String, javadoc: String): Unit = {
    if (fields.contains(k)) {
      throw new IllegalStateException("Duplicate field: " + k)
    } else {
      fields.put(k, (v, javadoc))
    }
  }
  
  def map(k: NamedMethod, v: String): Unit = map(k, v, "")
  
  def map(k: NamedMethod, v: String, javadoc: String): Unit = {
    if (methods.contains(k)) {
      throw new IllegalStateException("Duplicate method: " + k)
    } else {
      methods.put(k, (v, javadoc))
    }
  }

  def map(k: NamedConstructor, v: String): Unit = map(k, v, "")
  
  def map(k: NamedConstructor, v: String, javadoc: String): Unit = {
    if (constructors.contains(k)) {
      throw new IllegalStateException("Duplicate constructor: " + k)
    } else {
      constructors.put(k, (v, javadoc))
    }
  }
  
  def param(k: NamedMethod, idx: Integer, v: String): Unit = {
    if (!parameters.contains(k)) {
      parameters.put(k, mutable.Map())
    }
    parameters(k).put(idx, v)
  }
  
  def paramM(k: String, idx: Integer, v: String): Unit = {
    if (!uniqueParameters.contains(k)) {
      uniqueParameters.put(k, mutable.Map())
    }
    uniqueParameters(k).put(idx, v)
  }
  
  def param(k: NamedConstructor, idx: Integer, v: String): Unit = {
    if (!cparameters.contains(k)) {
      cparameters.put(k, mutable.Map())
    }
    cparameters(k).put(idx, v)
  }
  
  def paramC(k: String, idx: Integer, v: String): Unit = {
    if (!cuniqueParameters.contains(k)) {
      cuniqueParameters.put(k, mutable.Map())
    }
    cuniqueParameters(k).put(idx, v)
  }
  
  def uniqueF(k: String, v: String, s: Side, javadoc: String): Unit = {
    if (uniqueFields.contains(k)) {
      throw new IllegalStateException("Duplicate unique field mapping: '" + k + "'")
    }
    uniqueFields.put(k, (v, s, javadoc))
  }
  
  def uniqueM(k: String, v: String, p: Map[Int, String], s: Side, javadoc: String): Unit = {
    if (uniqueMethods.contains(k)) {
      throw new IllegalStateException("Duplicate unique method mapping: '" + k + "'")
    }
    uniqueMethods.put(k, (v, p, s, javadoc))
  }
  
  def uniqueC(k: String, v: String, p: Map[Int, String], s: Side, javadoc: String): Unit = {
    if (uniqueConstructors.contains(k)) {
      throw new IllegalStateException("Duplicate unique constructor mapping: '" + k + "'")
    }
    uniqueConstructors.put(k, (v, p, s, javadoc))
  }
  
  def build(): Mappings = {
    val builtMethods = methods.toMap.map(entry => {
      val nm = entry._1
      val mappedName = entry._2._1
      val javadoc = entry._2._2
      val paramMap: collection.Map[Int, String] = if (parameters.contains(nm)) {
        parameters(nm)
      } else if (uniqueParameters.contains(nm.name)) {
        uniqueParameters(nm.name)
      } else {
        Map()
      }
      val params = nm.sig.args.indices.map(paramMap.get).toList
      (nm, (mappedName, params, javadoc))
    })
    val builtConstructors = constructors.toMap.map(entry => {
      val nm = entry._1
      val mappedName = entry._2._1
      val javadoc = entry._2._2
      val paramMap: collection.Map[Int, String] = if (cparameters.contains(nm)) {
        cparameters(nm)
      } else if (cuniqueParameters.contains(nm.name)) {
        cuniqueParameters(nm.name)
      } else {
        Map()
      }
      val params = nm.sig.args.indices.map(paramMap.get).toList
      (nm, (mappedName, params, javadoc))
    })
    Mappings.create(from, to, classes.toMap, fields.toMap, builtMethods, builtConstructors, uniqueFields.toMap, uniqueMethods.toMap, uniqueConstructors.toMap)
  }
}
