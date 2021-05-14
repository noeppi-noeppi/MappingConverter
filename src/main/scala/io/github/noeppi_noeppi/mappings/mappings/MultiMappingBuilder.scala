package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.util.{ClassEntry, NamedConstructor, NamedField, NamedMethod}

import scala.collection.mutable

class MultiMappingBuilder {
  
  private val classes = mutable.Map[ClassEntry, (ClassEntry, ClassEntry)]()
  private val srgClasses = mutable.Set[ClassEntry]()
  private val mappedClasses = mutable.Set[ClassEntry]()
  private val fields = mutable.Map[NamedField, (String, String)]()
  
  private val methods = mutable.Map[NamedMethod, (String, String)]()
  private val parameters = mutable.Map[NamedMethod, mutable.Map[Int, (String, String)]]()
  private val uniqueParameters = mutable.Map[String, mutable.Map[Int, (String, String)]]()
  
  private val constructors = mutable.Map[NamedConstructor, (String, String)]()
  private val cparameters = mutable.Map[NamedConstructor, mutable.Map[Int, (String, String)]]()
  private val cuniqueParameters = mutable.Map[String, mutable.Map[Int, (String, String)]]()
  
  def map(k: ClassEntry, i: ClassEntry, v: ClassEntry): Unit = {
    if (classes.contains(k)) {
      throw new IllegalStateException("Duplicate class key: " + k)
    } else if (srgClasses.contains(i)) {
      throw new IllegalStateException("Duplicate class SRG/Intermediary: " + i)
    } else if (srgClasses.contains(v)) {
      throw new IllegalStateException("Duplicate class value: " + v)
    } else {
      classes.put(k, (i, v))
      srgClasses.addOne(i)
      mappedClasses.addOne(v)
    }
  }
  
  def map(k: NamedField, i: String, v: String): Unit = {
    if (fields.contains(k)) {
      throw new IllegalStateException("Duplicate field: " + k)
    } else {
      fields.put(k, (i, v))
    }
  }
  
  def map(k: NamedMethod, i: String, v: String): Unit = {
    if (methods.contains(k)) {
      throw new IllegalStateException("Duplicate method: " + k)
    } else {
      methods.put(k, (i, v))
    }
  }
  
  def map(k: NamedConstructor, i: String, v: String): Unit = {
    if (constructors.contains(k)) {
      throw new IllegalStateException("Duplicate constructor: " + k)
    } else {
      constructors.put(k, (i, v))
    }
  }
  
  def param(k: NamedMethod, idx: Integer, i: String, v: String): Unit = {
    if (!parameters.contains(k)) {
      parameters.put(k, mutable.Map())
    }
    parameters(k).put(idx, (i, v))
  }
  
  def paramM(k: String, idx: Integer, i: String, v: String): Unit = {
    if (!uniqueParameters.contains(k)) {
      uniqueParameters.put(k, mutable.Map())
    }
    uniqueParameters(k).put(idx, (i, v))
  }
  
  def param(k: NamedConstructor, idx: Integer, i: String, v: String): Unit = {
    if (!cparameters.contains(k)) {
      cparameters.put(k, mutable.Map())
    }
    cparameters(k).put(idx, (i, v))
  }
  
  def paramC(k: String, idx: Integer, i: String, v: String): Unit = {
    if (!cuniqueParameters.contains(k)) {
      cuniqueParameters.put(k, mutable.Map())
    }
    cuniqueParameters(k).put(idx, (i, v))
  }
  
  def build(): Mappings = {
    val builtMethods = methods.toMap.map(entry => {
      val nm = entry._1
      val srgName = entry._2._1
      val mappedName = entry._2._2
      val paramMap: collection.Map[Int, (String, String)] = if (parameters.contains(nm)) {
        parameters(nm)
      } else if (uniqueParameters.contains(nm.name)) {
        uniqueParameters(nm.name)
      } else {
        Map()
      }
      val srgParams = nm.sig.args.indices.map(idx => paramMap.get(idx).map(_._1)).toList
      val mappedParams = nm.sig.args.indices.map(idx => paramMap.get(idx).map(_._2)).toList
      (nm, ((srgName, srgParams), (mappedName, mappedParams)))
    })
    val builtConstructors = constructors.toMap.map(entry => {
      val nm = entry._1
      val srgName = entry._2._1
      val mappedName = entry._2._2
      val paramMap: collection.Map[Int, (String, String)] = if (cparameters.contains(nm)) {
        cparameters(nm)
      } else if (uniqueParameters.contains(nm.name)) {
        cuniqueParameters(nm.name)
      } else {
        Map()
      }
      val srgParams = nm.sig.args.indices.map(idx => paramMap.get(idx).map(_._1)).toList
      val mappedParams = nm.sig.args.indices.map(idx => paramMap.get(idx).map(_._2)).toList
      (nm, ((srgName, srgParams), (mappedName, mappedParams)))
    })
    Mappings.createMulti(classes.toMap, fields.toMap, builtMethods, builtConstructors)
  }
}
