package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.util._

import java.io.BufferedWriter
import scala.collection.mutable

object MappingDiff {

  // Writes a mapping difference report of two mappings.
  def writeDiffReport(names: Names, from: Mappings, to: Mappings, out: BufferedWriter): Unit = {
    // keys must be distinct
    val diffs = mutable.Map[ClassEntry, DiffReport]()
    
    // Added classes
    for (m <- to.classMappings; cls = m.name(names) if !from.canMap(names, cls)) {
      diffs.put(TypeEntry.distinct(cls), new DiffReport(true, false, cls))
    }
    
    // Removed classes
    for (m <- from.classMappings; cls = m.name(names) if !to.canMap(names, cls)) {
      diffs.put(TypeEntry.distinct(cls), new DiffReport(false, true, cls))
    }
    
    // utility to get diff report for a class
    def diff(cls: ClassEntry): DiffReport = diffs.getOrElseUpdate(TypeEntry.distinct(cls), new DiffReport(false, false, cls))
    
    // Changed fields
    val (usedFOld, usedFNew, fchanges) = findChanges[NamedField, TypeEntry](from.fieldMappings.map(_.name(names)), to.fieldMappings.map(_.name(names)), _.name, NamedField.distinct, _.element)
    for ((o, n) <- fchanges) {
      diff(o.cls).signatureFields.addOne(o -> n)
    }
    // Added fields
    for (m <- to.fieldMappings; f = m.name(names) if !usedFNew.contains(NamedField.distinct(f)) && !from.canMap(names, f)) {
      diff(f.cls).addedFields.add(f)
    }
    // Removed fields
    for (m <- from.fieldMappings; f = m.name(names) if !usedFOld.contains(NamedField.distinct(f)) && !to.canMap(names, f)) {
      diff(f.cls).removedFields.add(f)
    }
    
    // Changed constructors
    val (usedCOld, usedCNew, cchanges) = findChanges[NamedConstructor, MethodSignature](from.constructorMappings.map(_.name(names)), to.constructorMappings.map(_.name(names)), _.name, NamedConstructor.distinct, e => Some(e.sig))
    for ((o, n) <- cchanges) {
      diff(o.cls).signatureCtors.addOne(o -> n)
    }
    // Added constructors
    for (m <- to.constructorMappings; c = m.name(names) if !usedCNew.contains(NamedConstructor.distinct(c)) && !from.canMap(names, c)) {
      diff(c.cls).addedCtors.add(c)
    }
    // Removed constructors
    for (m <- from.constructorMappings; c = m.name(names) if !usedCOld.contains(NamedConstructor.distinct(c)) && !to.canMap(names, c)) {
      diff(c.cls).removedCtors.add(c)
    }
    
    // Changed methods
    val (usedMOld, usedMNew, mchanges) = findChanges[NamedMethod, MethodSignature](from.methodMappings.map(_.name(names)), to.methodMappings.map(_.name(names)), _.name, NamedMethod.distinct, e => Some(e.sig))
    for ((o, n) <- mchanges) {
      diff(o.cls).signatureMethods.addOne(o -> n)
    }
    // Added methods
    for (m <- to.methodMappings; c = m.name(names) if !usedMNew.contains(NamedMethod.distinct(c)) && !from.canMap(names, c)) {
      diff(c.cls).addedMethods.add(c)
    }
    // Removed methods
    for (m <- from.methodMappings; c = m.name(names) if !usedMOld.contains(NamedMethod.distinct(c)) && !to.canMap(names, c)) {
      diff(c.cls).removedMethods.add(c)
    }
    
    if (diffs.isEmpty) {
      out.write("Mappings are identical")
    } else {
      for (diff <- diffs.values.toSeq.filter(_.added).sortBy(_.cls.internalString)) {
        diff.write(out)
      }
      for (diff <- diffs.values.toSeq.filter(x => !x.added && x.removed).sortBy(_.cls.internalString)) {
        diff.write(out)
      }
      for (diff <- diffs.values.toSeq.filter(x => !x.added && !x.removed).sortBy(_.cls.internalString)) {
        diff.write(out)
      }
    }
  }
  
  def findChanges[T, U](o: List[T], n: List[T], nameExtractor: T => String, distinct: T => T, targetExtractor: T => Option[U]): (Set[T], Set[T], Map[T, U]) = {
    val names = n.map(t => (nameExtractor(t), t)).toVector
    val nameMapBuilder = mutable.Map[String, T]()
    val blockedNames = mutable.Set[String]()
    for ((k, v) <- names) {
      if (blockedNames.contains(k) || nameMapBuilder.contains(k)) {
        blockedNames.add(k)
        nameMapBuilder.remove(k)
      } else {
        nameMapBuilder.put(k, v)
      }
    }
    val nameMap = nameMapBuilder.toMap
    names.filter(e => names.count(_._1 == e._1) == 1).toMap
    val changeList = for (t <- o; name = nameExtractor(t) if nameMap.contains(name); q = nameMap(name) if distinct(t) != distinct(q))
      yield t -> q
    (changeList.map(_._1).map(distinct).toSet, changeList.map(_._2).map(distinct).toSet, changeList.flatMap(e => targetExtractor(e._2).map(x => (e._1, x))).toMap)
  }
  
  private def sideStr(side: Side): String = {
    if (side != Unknown) {
      " @ " + side.name + "(" + side.mcpIdx + ")"
    } else {
      ""
    }
  }
  
  private def ctorName(name: String): String = {
    if (name == "<init>") {
      "<init>"
    } else {
      "<init>#" + name
    }
  }

  class DiffReport(val added: Boolean, val removed: Boolean, val cls: ClassEntry) {

    val addedFields: mutable.Set[NamedField] = mutable.Set[NamedField]()
    val removedFields: mutable.Set[NamedField] = mutable.Set[NamedField]()
    val signatureFields: mutable.Map[NamedField, TypeEntry] = mutable.Map[NamedField, TypeEntry]()

    val addedCtors: mutable.Set[NamedConstructor] = mutable.Set[NamedConstructor]()
    val removedCtors: mutable.Set[NamedConstructor] = mutable.Set[NamedConstructor]()
    val signatureCtors: mutable.Map[NamedConstructor, MethodSignature] = mutable.Map[NamedConstructor, MethodSignature]()

    val addedMethods: mutable.Set[NamedMethod] = mutable.Set[NamedMethod]()
    val removedMethods: mutable.Set[NamedMethod] = mutable.Set[NamedMethod]()
    val signatureMethods: mutable.Map[NamedMethod, MethodSignature] = mutable.Map[NamedMethod, MethodSignature]()

    def write(out: BufferedWriter): Unit = {
      if (added) {
        out.write("+ " + cls.internalString + sideStr(cls.side) + "\n")
      } else if (removed) {
        out.write("- " + cls.internalString + sideStr(cls.side) + "\n")
      } else {
        out.write("* " + cls.internalString + sideStr(cls.side) + "\n")
      }
      
      for (f <- addedFields.toSeq.sortBy(_.name)) {
        out.write("  + " + f.name + " " + f.element.map(e => " " + e.string).getOrElse("") + sideStr(cls.side) + "\n")
      }
      for (f <- addedCtors.toSeq.sortBy(_.name)) {
        out.write("  + " + ctorName(f.name) + " " + f.sig.toString + sideStr(cls.side) + "\n")
      }
      for (f <- addedMethods.toSeq.sortBy(_.name)) {
        out.write("  + " + f.name + " " + f.sig.toString + sideStr(cls.side) + "\n")
      }
      
      
      for (f <- removedFields.toSeq.sortBy(_.name)) {
        out.write("  - " + f.name + " " + f.element.map(e => " " + e.string).getOrElse("") + sideStr(cls.side) + "\n")
      }
      for (f <- removedCtors.toSeq.sortBy(_.name)) {
        out.write("  - " + ctorName(f.name) + " " + f.sig.toString + sideStr(cls.side) + "\n")
      }
      for (f <- removedMethods.toSeq.sortBy(_.name)) {
        out.write("  - " + f.name + " " + f.sig.toString + sideStr(cls.side) + "\n")
      }
      
      for ((f, nn) <- signatureFields.toSeq.sortBy(_._1.name)) {
        out.write("  * " + f.name + " " + f.element.map(e => " " + e.string).getOrElse("") + sideStr(cls.side) + " -> " + nn + "\n")
      }
      for ((f, nn) <- signatureCtors.toSeq.sortBy(_._1.name)) {
        out.write("  * " + ctorName(f.name) + " " + f.sig.toString + sideStr(cls.side) + " -> " + nn.toString + "\n")
      }
      for ((f, nn) <- signatureMethods.toSeq.sortBy(_._1.name)) {
        out.write("  * " + f.name + " " + f.sig.toString + sideStr(cls.side) + " -> " + nn.toString + "\n")
      }
      out.write("\n")
    }
  }
}
