package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.mappings.MappingMerger.{mergeMappingLists, mergeUnique, mergeUniqueParam}
import io.github.noeppi_noeppi.mappings.remapper.ClassRemapper
import io.github.noeppi_noeppi.mappings.util.{ClassEntry, MethodSignature, NamedConstructor, NamedField, NamedMethod, Side, TypeEntry, Unknown}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

class Mappings private (
                         private val names: Set[Names],
                         val classMappings: List[Mapping[ClassEntry]],
                         val fieldMappings: List[Mapping[NamedField]],
                         val methodMappings: List[Mapping[NamedMethod]],
                         val constructorMappings: List[Mapping[NamedConstructor]],
                         val uniqueFieldNames: Map[String, (String, Side, String)],
                         val uniqueMethodNames: Map[String, (String, Map[Int, String], Side, String)],
                         val uniqueConstructorNames: Map[String, (String, Map[Int, String], Side, String)]
                       ) {
  
  private[this] val remappers = mutable.Map[(Names, Names), ClassRemapper]()
  private[this] val lookupMapC = mutable.Map[Names, Map[ClassEntry, Mapping[ClassEntry]]]()
  private[this] val lookupMapF = mutable.Map[Names, Map[NamedField, Mapping[NamedField]]]()
  private[this] val lookupMapM = mutable.Map[Names, Map[NamedMethod, Mapping[NamedMethod]]]()
  private[this] val lookupMapI = mutable.Map[Names, Map[NamedConstructor, Mapping[NamedConstructor]]]()
  
  private def remapper(from: Names, to: Names): ClassRemapper = {
    if (!remappers.contains((from, to))) {
      if (!has(from) || !has(to)) {
        return ClassRemapper.NOOP
      } else {
        remappers.put((from, to), new ClassRemapper(classMappings.map(m => (m.name(from), m.name(to))).toMap))
      }
    }
    remappers((from, to))
  }
  
  def hasUniqueMappings: Boolean = uniqueFieldNames.nonEmpty || uniqueMethodNames.nonEmpty || uniqueConstructorNames.nonEmpty
  def warnUnique(fmt: String): Unit = if (hasUniqueMappings) println(fmt + " does not support unique mappings. They'll be skipped. To retains them, apply the unique mappings to other mappings first.")
  
  def has(name: Names): Boolean = names.contains(name)
  
  def canMap(from: Names, key: ClassEntry): Boolean = lookupC(from).contains(TypeEntry.distinct(key))
  def canMap(from: Names, key: NamedField): Boolean = lookupF(from).contains(NamedField.distinct(key)) || uniqueFieldNames.contains(key.name)
  def canMap(from: Names, key: NamedMethod): Boolean = lookupM(from).contains(NamedMethod.distinct(key)) || uniqueMethodNames.contains(key.name)
  def canMap(from: Names, key: NamedConstructor): Boolean = lookupI(from).contains(NamedConstructor.distinct(key)) || uniqueConstructorNames.contains(key.name)
  
  def map(from: Names, to: Names, key: ClassEntry): ClassEntry = lookupC(from).get(TypeEntry.distinct(key)).map(_.name(to)).getOrElse(key)
  def map(from: Names, to: Names, key: NamedField): NamedField = lookupF(from).get(NamedField.distinct(key)).map(_.name(to)).orElse(uniqueFieldNames.get(key.name).map(n => NamedField(key.cls, n._1, None, Side.merge(key.side, n._2), n._3))).getOrElse(key)
  def map(from: Names, to: Names, key: NamedMethod): NamedMethod = lookupM(from).get(NamedMethod.distinct(key)).map(_.name(to)).orElse(uniqueMethodNames.get(key.name).map(n => NamedMethod(key.cls, n._1, key.sig.remap(remapper(from, to)), buildParamsByUnique(key.sig, key.params, n._2), Side.merge(key.side, n._3), n._4))).getOrElse(key)
  def map(from: Names, to: Names, key: NamedConstructor): NamedConstructor = lookupI(from).get(NamedConstructor.distinct(key)).map(_.name(to)).orElse(uniqueConstructorNames.get(key.name).map(n => NamedConstructor(key.cls, n._1, key.sig.remap(remapper(from, to)), buildParamsByUnique(key.sig, key.params, n._2), Side.merge(key.side, n._3), n._4))).getOrElse(key)
  
  def distinctKeysC(from: Names): Set[ClassEntry] = lookupC(from).keySet
  def distinctKeysF(from: Names): Set[NamedField] = lookupF(from).keySet
  def distinctKeysM(from: Names): Set[NamedMethod] = lookupM(from).keySet
  def distinctKeysI(from: Names): Set[NamedConstructor] = lookupI(from).keySet
  
  private def buildParamsByUnique(m: MethodSignature, a: List[Option[String]], p: Map[Int, String]): List[Option[String]] = {
    m.args.indices.map(idx => p.get(idx).orElse(a(idx))).toList
  }
  
  def lookupC(from: Names): Map[ClassEntry, Mapping[ClassEntry]] = lookupMapC.getOrElseUpdate(from, buildLookupMap(from, classMappings, TypeEntry.distinct))
  def lookupF(from: Names): Map[NamedField, Mapping[NamedField]] = lookupMapF.getOrElseUpdate(from, buildLookupMap(from, fieldMappings, NamedField.distinct))
  def lookupM(from: Names): Map[NamedMethod, Mapping[NamedMethod]] = lookupMapM.getOrElseUpdate(from, buildLookupMap(from, methodMappings, NamedMethod.distinct))
  def lookupI(from: Names): Map[NamedConstructor, Mapping[NamedConstructor]] = lookupMapI.getOrElseUpdate(from, buildLookupMap(from, constructorMappings, NamedConstructor.distinct))
  
  private def buildLookupMap[T](from: Names, entries: List[Mapping[T]], distinct: T => T): Map[T, Mapping[T]] = {
    val builder = Map.newBuilder[T, Mapping[T]]
    for (m <- entries) {
      builder.addOne(distinct(m.name(from)) ->  m)
    }
    builder.result()
  }
}

class Mapping[T](val values: Map[Names, T]) {
  
  def has(names: Names): Boolean = values.contains(names)
  def name(names: Names): T = values.getOrElse(names, throw new IllegalStateException("Mapping is not defined for name: " + names))
  def names: Set[Names] = values.keySet
  
  private[mappings] def transform(transformations: Map[Names, Option[Names]]): Mapping[T] = new Mapping[T](values.flatMap(entry => transformations.getOrElse(entry._1, Some(entry._1)).map(value => value -> entry._2)))
  def transform(transformer: T => T): Mapping[T] = new Mapping[T](values.map(entry => (entry._1, transformer(entry._2))))
  def transform(target: Names, transformer: T => T): Mapping[T] = if (values.contains(target)) { new Mapping[T](values.updated(target, transformer(values(target)))) } else { this }
  def testSide(extractor: T => Side, primary: Option[Names], requiresClient: Boolean, requiresServer: Boolean): Boolean = {
    if (primary.isDefined && values.contains(primary.get)) {
      extractor(values(primary.get)).test(requiresClient, requiresServer)
    } else {
      values.values.map(extractor).fold(Unknown)(Side.join).test(requiresClient, requiresServer)
    }
  }
}

object Mappings {
  
  def createPseudo(names: Names, constructors: Set[NamedConstructor]): Mappings = {
    val constructorMappings = constructors.map(entry => new Mapping[NamedConstructor](Map(names -> entry)))
    new Mappings(Set(names), Nil, Nil, Nil, constructorMappings.toList, Map(), Map(), Map())
  }
  
  def create(from: Names, to: Names, classes: Map[ClassEntry, ClassEntry], fields: Map[NamedField, (String, String)], methods: Map[NamedMethod, (String, List[Option[String]], String)], constructors: Map[NamedConstructor, (String, List[Option[String]], String)]): Mappings = {
    create(from, to, classes, fields, methods, constructors, Map(), Map(), Map())
  }
  
  def create(from: Names, to: Names, classes: Map[ClassEntry, ClassEntry], fields: Map[NamedField, (String, String)], methods: Map[NamedMethod, (String, List[Option[String]], String)], constructors: Map[NamedConstructor, (String, List[Option[String]], String)], uniqueFields: Map[String, (String, Side, String)], uniqueMethods: Map[String, (String, Map[Int, String], Side, String)], uniqueConstructors: Map[String, (String, Map[Int, String], Side, String)]): Mappings = {
    val remapper = new ClassRemapper(classes)
    val names = Set(from, to) 
    val classMappings = classes.map(entry => new Mapping[ClassEntry](Map(from -> entry._1, to -> entry._2)))
    val fieldMappings = fields.map(entry => new Mapping[NamedField](Map(from -> entry._1, to -> NamedField(remapper.remap(entry._1.cls), entry._2._1, entry._1.element.map(remapper.remap), entry._1.side, entry._2._2))))
    val methodMappings = methods.map(entry => new Mapping[NamedMethod](Map(from -> entry._1, to -> NamedMethod(remapper.remap(entry._1.cls), entry._2._1, entry._1.sig.remap(remapper), entry._2._2, entry._1.side, entry._2._3))))
    val constructorMappings = constructors.map(entry => new Mapping[NamedConstructor](Map(from -> entry._1, to -> NamedConstructor(remapper.remap(entry._1.cls), entry._2._1, entry._1.sig.remap(remapper), entry._2._2, entry._1.side, entry._2._3))))
    new Mappings(names, classMappings.toList, fieldMappings.toList, methodMappings.toList, constructorMappings.toList, uniqueFields, uniqueMethods, uniqueConstructors)
  }
  
  def createMulti(classes: Map[ClassEntry, (ClassEntry, ClassEntry)], fields: Map[NamedField, (String, String)], methods: Map[NamedMethod, ((String, List[Option[String]]), (String, List[Option[String]]))], constructors: Map[NamedConstructor, ((String, List[Option[String]]), (String, List[Option[String]]))]): Mappings = {
    val remapperSrg = new ClassRemapper(classes.map(entry => (entry._1, entry._2._1)))
    val remapperMap = new ClassRemapper(classes.map(entry => (entry._1, entry._2._2)))
    val names = Set(Obfuscated, SRG, Mapped) 
    val classMappings = classes.map(entry => new Mapping[ClassEntry](Map(Obfuscated -> entry._1, SRG -> entry._2._1, Mapped -> entry._2._2)))
    val fieldMappings = fields.map(entry => new Mapping[NamedField](Map(Obfuscated -> entry._1, SRG -> NamedField(remapperSrg.remap(entry._1.cls), entry._2._1, entry._1.element.map(remapperSrg.remap), entry._1.side, ""), Mapped -> NamedField(remapperMap.remap(entry._1.cls), entry._2._2, entry._1.element.map(remapperMap.remap), entry._1.side, ""))))
    val methodMappings = methods.map(entry => new Mapping[NamedMethod](Map(Obfuscated -> entry._1, SRG -> NamedMethod(remapperSrg.remap(entry._1.cls), entry._2._1._1, entry._1.sig.remap(remapperSrg), entry._2._1._2, entry._1.side, ""), Mapped -> NamedMethod(remapperMap.remap(entry._1.cls), entry._2._1._1, entry._1.sig.remap(remapperMap), entry._2._1._2, entry._1.side, ""))))
    val constructorMappings = constructors.map(entry => new Mapping[NamedConstructor](Map(Obfuscated -> entry._1, SRG -> NamedConstructor(remapperSrg.remap(entry._1.cls), entry._2._1._1, entry._1.sig.remap(remapperSrg), entry._2._1._2, entry._1.side, ""), Mapped -> NamedConstructor(remapperMap.remap(entry._1.cls), entry._2._1._1, entry._1.sig.remap(remapperMap), entry._2._1._2, entry._1.side, ""))))
    new Mappings(names, classMappings.toList, fieldMappings.toList, methodMappings.toList, constructorMappings.toList, Map(), Map(), Map())
  }
  
  // Leave SRG names if mappings have no name
  def apply(srg: Mappings, map: Mappings): Mappings = {
    if (!srg.has(Obfuscated)) {
      throw new IllegalStateException("Can't apply mappings together: First mappings are missing obfuscated names.")
    }
    if (!srg.has(SRG)) {
      throw new IllegalStateException("Can't apply mappings together: First mappings are missing SRG/Intermediary names.")
    }
    if (!map.has(SRG)) {
      throw new IllegalStateException("Can't apply mappings together: First mappings are missing SRG/Intermediary names.")
    }
    if (!map.has(Mapped)) {
      throw new IllegalStateException("Can't apply mappings together: First mappings are missing deobfuscated names.")
    }
    val names = Set(Obfuscated, SRG, Mapped)
    val classMappings = srg.classMappings.map(m => new Mapping[ClassEntry](m.values.updated(Mapped, map.map(SRG, Mapped, m.name(SRG)))))
    val remappers = lazyRemappers(classMappings)
    val fieldMappings = srg.fieldMappings.map(m => applyField(m, Mapped, map.map(SRG, Mapped, m.name(SRG)), remappers))
    val methodMappings = srg.methodMappings.map(m => new Mapping[NamedMethod](m.values.updated(Mapped, map.map(SRG, Mapped, m.name(SRG)))))
    val constructorMappings = srg.constructorMappings.map(m => new Mapping[NamedConstructor](m.values.updated(Mapped, map.map(SRG, Mapped, m.name(SRG)))))
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, Map(), Map(), Map())
  }
  
  // Drop mappings completely if mappings have no name
  def applyPartial(srg: Mappings, map: Mappings): Mappings = {
    if (!srg.has(Obfuscated)) {
      throw new IllegalStateException("Can't partially apply mappings together: First mappings are missing obfuscated names.")
    }
    if (!srg.has(SRG)) {
      throw new IllegalStateException("Can't partially apply mappings together: First mappings are missing SRG/Intermediary names.")
    }
    if (!map.has(SRG)) {
      throw new IllegalStateException("Can't partially apply mappings together: First mappings are missing SRG/Intermediary names.")
    }
    if (!map.has(Mapped)) {
      throw new IllegalStateException("Can't partially apply mappings together: First mappings are missing deobfuscated names.")
    }
    val names = Set(Obfuscated, SRG, Mapped)
    // class mappings are not dropped as this breaks things
    val classMappings = srg.classMappings.map(m => new Mapping[ClassEntry](m.values.updated(Mapped, map.map(SRG, Mapped, m.name(SRG)))))
    val remappers = lazyRemappers(classMappings)
    val fieldMappings = srg.fieldMappings.flatMap(m => if (map.canMap(SRG, m.name(SRG))) { Some(applyField(m, Mapped, map.map(SRG, Mapped, m.name(SRG)), remappers)) } else { None })
    val methodMappings = srg.methodMappings.flatMap(m => if (map.canMap(SRG, m.name(SRG))) { Some(new Mapping[NamedMethod](m.values.updated(Mapped, map.map(SRG, Mapped, m.name(SRG))))) } else { None })
    val constructorMappings = srg.constructorMappings.flatMap(m => if (map.canMap(SRG, m.name(SRG))) { Some(new Mapping[NamedConstructor](m.values.updated(Mapped, map.map(SRG, Mapped, m.name(SRG))))) } else { None })
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, Map(), Map(), Map())
  }

  def applyObf(srg: Mappings, map: Mappings): Mappings = {
    if (!srg.has(Obfuscated)) {
      throw new IllegalStateException("Can't obf-apply mappings together: First mappings are missing obfuscated names.")
    }
    if (!srg.has(SRG)) {
      throw new IllegalStateException("Can't obf-apply mappings together: First mappings are missing SRG/Intermediary names.")
    }
    if (!map.has(Obfuscated)) {
      throw new IllegalStateException("Can't obf-apply mappings together: First mappings are missing obfuscated names.")
    }
    if (!map.has(Mapped)) {
      throw new IllegalStateException("Can't obf-apply mappings together: First mappings are missing deobfuscated names.")
    }
    val names = Set(Obfuscated, SRG, Mapped)
    val classMappings = srg.classMappings.map(m => new Mapping[ClassEntry](m.values.updated(Mapped, map.map(Obfuscated, Mapped, m.name(Obfuscated)))))
    val remappers = lazyRemappers(classMappings)
    val fieldMappings = srg.fieldMappings.map(m => applyField(m, Mapped, map.map(Obfuscated, Mapped, m.name(Obfuscated)), remappers))
    val methodMappings = srg.methodMappings.map(m => new Mapping[NamedMethod](m.values.updated(Mapped, map.map(Obfuscated, Mapped, m.name(Obfuscated)))))
    val constructorMappings = srg.constructorMappings.map(m => new Mapping[NamedConstructor](m.values.updated(Mapped, map.map(Obfuscated, Mapped, m.name(Obfuscated)))))
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, Map(), Map(), Map())
  }
  
  def known(mappings: Mappings, side: Side): Mappings = {
    val names = mappings.names
    val classMappings = mappings.classMappings.map(m => m.transform(_.known(side)))
    val fieldMappings = mappings.fieldMappings.map(m => m.transform(_.known(side)))
    val methodMappings = mappings.methodMappings.map(m => m.transform(_.known(side)))
    val constructorMappings = mappings.constructorMappings.map(m => m.transform(_.known(side)))
    val uniqueFieldsNames = mappings.uniqueFieldNames.map(entry => if (entry._2._2 == Unknown) { (entry._1, (entry._2._1, side, entry._2._3)) } else { entry })
    val uniqueMethodNames = mappings.uniqueMethodNames.map(entry => if (entry._2._3 == Unknown) { (entry._1, (entry._2._1, entry._2._2, side, entry._2._4)) } else { entry })
    val uniqueConstructorNames = mappings.uniqueConstructorNames.map(entry => if (entry._2._3 == Unknown) { (entry._1, (entry._2._1, entry._2._2, side, entry._2._4)) } else { entry })
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, uniqueFieldsNames, uniqueMethodNames, uniqueConstructorNames)
  }
  
  def forceKnown(mappings: Mappings, side: Side): Mappings = {
    val names = mappings.names
    val classMappings = mappings.classMappings.map(m => m.transform(_.forceKnown(side)))
    val fieldMappings = mappings.fieldMappings.map(m => m.transform(_.forceKnown(side)))
    val methodMappings = mappings.methodMappings.map(m => m.transform(_.forceKnown(side)))
    val constructorMappings = mappings.constructorMappings.map(m => m.transform(_.forceKnown(side)))
    val uniqueFieldsNames = mappings.uniqueFieldNames.map(entry => (entry._1, (entry._2._1, side, entry._2._3)))
    val uniqueMethodNames = mappings.uniqueMethodNames.map(entry => (entry._1, (entry._2._1, entry._2._2, side, entry._2._4)))
    val uniqueConstructorNames = mappings.uniqueConstructorNames.map(entry => (entry._1, (entry._2._1, entry._2._2, side, entry._2._4)))
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, uniqueFieldsNames, uniqueMethodNames, uniqueConstructorNames)
  }
  
  def filterSide(mappings: Mappings, primary: Option[Names], requiresClient: Boolean, requiresServer: Boolean): Mappings = {
    val names = mappings.names
    val classMappings = mappings.classMappings.filter(m => m.testSide(_.side, primary, requiresClient, requiresServer))
    val fieldMappings = mappings.fieldMappings.filter(m => m.testSide(_.side, primary, requiresClient, requiresServer))
    val methodMappings = mappings.methodMappings.filter(m => m.testSide(_.side, primary, requiresClient, requiresServer))
    val constructorMappings = mappings.constructorMappings.filter(m => m.testSide(_.side, primary, requiresClient, requiresServer))
    val uniqueFieldsNames = mappings.uniqueFieldNames.filter(entry => entry._2._2.test(requiresClient, requiresServer))
    val uniqueMethodNames = mappings.uniqueMethodNames.filter(entry => entry._2._3.test(requiresClient, requiresServer))
    val uniqueConstructorNames = mappings.uniqueConstructorNames.filter(entry => entry._2._3.test(requiresClient, requiresServer))
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, uniqueFieldsNames, uniqueMethodNames, uniqueConstructorNames)
  }

  def merge(toward: Names, safe: Option[Names], m: Mappings*): Mappings = {
    if (m.size == 1) {
      return m.head
    } else if (safe.isDefined && m.size != 2) {
      throw new IllegalStateException("Safe merging only works with exaclty two mappings.")
    }
    val names = m.map(_.names).reduce(_ intersect _)
    val classMappings = mergeMappingLists[ClassEntry](m.map(_.classMappings), m.map(_.lookupC(toward)), _.side, _.forceKnown(_), safe.map(names => e => m.head.canMap(names, e.name(names))))
    val fieldMappings = mergeMappingLists[NamedField](m.map(_.fieldMappings), m.map(_.lookupF(toward)), _.side, _.forceKnown(_), safe.map(names => e => m.head.canMap(names, e.name(names))))
    val methodMappings = mergeMappingLists[NamedMethod](m.map(_.methodMappings), m.map(_.lookupM(toward)), _.side, _.forceKnown(_), safe.map(names => e => m.head.canMap(names, e.name(names))))
    val constructorMappings = mergeMappingLists[NamedConstructor](m.map(_.constructorMappings), m.map(_.lookupI(toward)), _.side, _.forceKnown(_), safe.map(names => e => m.head.canMap(names, e.name(names))))
    val uniqueFieldsNames = mergeUnique(m.map(_.uniqueFieldNames))
    val uniqueMethodNames = mergeUniqueParam(m.map(_.uniqueMethodNames))
    val uniqueConstructorNames = mergeUniqueParam(m.map(_.uniqueConstructorNames))
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, uniqueFieldsNames, uniqueMethodNames, uniqueConstructorNames)
  }
  
  def transform(mappings: Mappings, transformations: (Names, Names)*): Mappings = transform(mappings, Map(transformations: _*))
  
  def transform(mappings: Mappings, transformations: Map[Names, Names]): Mappings = {
    val transformationMap = buildTransformationMap(transformations)
    val names = mappings.names.map(n => transformations.getOrElse(n, n))
    val classMappings = mappings.classMappings.map(_.transform(transformationMap))
    val fieldMappings = mappings.fieldMappings.map(_.transform(transformationMap))
    val methodMappings = mappings.methodMappings.map(_.transform(transformationMap))
    val constructorMappings = mappings.constructorMappings.map(_.transform(transformationMap))
    new Mappings(names, classMappings, fieldMappings, methodMappings, constructorMappings, Map(), Map(), Map())
  }
  
  // Will remove all constructor mappings from the old mappings. If they need to be retained as well,
  // Merge the old with the new mappings afterwards.
  // Pseudo-Names and param names of constructors are only kept for the names they were defined before.
  // Will also remove all unique constructor names.
  def applyConstructors(from: Names, mappings: Mappings, ctors: Mappings): Mappings = {
    val remappers = lazyRemappers(mappings.classMappings)
    val constructorMappings = ctors.constructorMappings.map(entry => fillConstructorMappingFrom(mappings.names, from, entry.name(from), remappers))
    new Mappings(mappings.names, mappings.classMappings, mappings.fieldMappings, mappings.methodMappings, constructorMappings, mappings.uniqueFieldNames, mappings.uniqueMethodNames, Map())
  }
  
  def applyFieldTypes(from: Names, mappings: Mappings, ftypes: Mappings): Mappings = {
    val remappers = lazyRemappers(mappings.classMappings)
    val fieldMappings = mappings.fieldMappings.map(entry => applyFieldType(from, ftypes, entry, remappers))
    new Mappings(mappings.names, mappings.classMappings, fieldMappings, mappings.methodMappings, mappings.constructorMappings, mappings.uniqueFieldNames, mappings.uniqueMethodNames, mappings.uniqueConstructorNames)
  }
  
  def prefix(target: Names, prefix: String, mappings: Mappings): Mappings = {
    val fieldMappings = mappings.fieldMappings.map(_.transform(target, elem => elem.renamed(prefix + elem.name)))
    val methodMappings = mappings.methodMappings.map(_.transform(target, elem => elem.renamed(prefix + elem.name)))
    val uniqueFields = if (target == Mapped) { mappings.uniqueFieldNames.map(entry => (entry._1, (prefix + entry._2._1, entry._2._2, entry._2._3))) } else { mappings.uniqueFieldNames }
    val uniqueMethods = if (target == Mapped) { mappings.uniqueMethodNames.map(entry => (entry._1, (prefix + entry._2._1, entry._2._2, entry._2._3, entry._2._4))) } else { mappings.uniqueMethodNames }
    new Mappings(mappings.names, mappings.classMappings, fieldMappings, methodMappings, mappings.constructorMappings, uniqueFields, uniqueMethods, mappings.uniqueConstructorNames)
  }
  
  def regexParams(target: Names, pattern: Regex, replacement: String, mappings: Mappings): Mappings = {
    val methodMappings = mappings.methodMappings.map(_.transform(target, entry => entry.updatedParams(replaceParams(entry.params, pattern, replacement))))
    val constructorMappings = mappings.constructorMappings.map(_.transform(target, entry => entry.updatedParams(replaceParams(entry.params, pattern, replacement))))
    val uniqueMethods = mappings.uniqueMethodNames.map(entry => (entry._1, (entry._2._1, replaceParams(entry._2._2, pattern, replacement), entry._2._3, entry._2._4)))
    val uniqueConstructors = mappings.uniqueConstructorNames.map(entry => (entry._1, (entry._2._1, replaceParams(entry._2._2, pattern, replacement), entry._2._3, entry._2._4)))
    new Mappings(mappings.names, mappings.classMappings, mappings.fieldMappings, methodMappings, constructorMappings, mappings.uniqueFieldNames, uniqueMethods, uniqueConstructors)
  }
  
  private def replaceParams(params: List[Option[String]], pattern: Regex, replacement: String): List[Option[String]] = {
    val usedNames = mutable.Set.from(params.flatten)
    val newParams = ListBuffer.from(params)
    for ((elemOption, idx) <- params.zipWithIndex if elemOption.isDefined; elem = elemOption.get) {
      val newName = replace(elem, pattern, replacement)
      if (!usedNames.contains(newName)) {
        newParams(idx) = Some(newName)
        usedNames.add(newName)
      }
    }
    newParams.toList
  }
  
  private def replaceParams(params: Map[Int, String], pattern: Regex, replacement: String): Map[Int, String] = {
    val usedNames = mutable.Set.from(params.values)
    val newParams = mutable.Map.from(params)
    for ((idx, elem) <- params) {
      val newName = replace(elem, pattern, replacement)
      if (!usedNames.contains(newName)) {
        newParams.put(idx, newName)
        usedNames.add(newName)
      }
    }
    newParams.toMap
  }
  
  private def replace(name: String, pattern: Regex, replacement: String): String = {
    name match {
      case pattern(groups @ _*) =>
        var newName = replacement
        for ((elem, idx) <- groups.zipWithIndex.reverse) {
          newName = newName.replace("#" + (idx + 1), elem)
        }
        newName = newName.replace("##", "#")
        newName
      case _ => name
    }
  }
  
  private def applyFieldType(from: Names, ftypes: Mappings, m: Mapping[NamedField], remappers: (Names, Names) => ClassRemapper): Mapping[NamedField] = {
    val elementFrom = ftypes.lookupF(from).get(NamedField.distinct(m.name(from)))
    elementFrom match {
      case Some(elementApply) =>
        val fieldType = elementApply.name(from).element
        val builder = Map.newBuilder[Names, NamedField]
        for (n <- m.names) {
          val elem = m.name(n)
          elem.element match {
            case Some(_) => builder.addOne(n -> elem)
            case None if fieldType.isDefined => builder.addOne(n -> elem.withElement(remappers(from, n).remap(fieldType.get)))
            case None => builder.addOne(n -> elem)
          }
        }
        new Mapping[NamedField](builder.result())
      case None => m
    }
  }
  
  private def fillConstructorMappingFrom(names: Set[Names], from: Names, ctor: NamedConstructor, remappers: (Names, Names) => ClassRemapper): Mapping[NamedConstructor] = {
    val builder = Map.newBuilder[Names, NamedConstructor]
    for (n <- names) {
      if (n == from) {
        builder.addOne(n -> ctor)
      } else {
        val remapper = remappers(from, n)
        builder.addOne(n -> NamedConstructor(remapper.remap(ctor.cls), "<init>", ctor.sig.remap(remapper), ctor.side))
      }
    }
    new Mapping[NamedConstructor](builder.result())
  }
  
  private def buildTransformationMap(transformations: Map[Names, Names]): Map[Names, Option[Names]] = {
    val drop = transformations.values.toSet.removedAll(transformations.keys)
    val builder = Map.newBuilder[Names, Option[Names]]
    builder.addAll(transformations.map(entry => entry._1 -> Some(entry._2)))
    builder.addAll(drop.map(n => n -> None))
    builder.result()
  }

  private def lazyRemappers(classMappings: List[Mapping[ClassEntry]]): (Names, Names) => ClassRemapper = {
    val map = mutable.Map[(Names, Names), ClassRemapper]()
    (from, to) => {
      if (!map.contains(from -> to)) {
        map.put(from -> to, new ClassRemapper(classMappings.map(m => m.name(from) -> m.name(to)).toMap))
      }
      map(from -> to)
    }
  }
  
  // Handles copying the field type whenever possible
  private def applyField(old: Mapping[NamedField], newKey: Names, newValue: NamedField, remappers: (Names, Names) => ClassRemapper): Mapping[NamedField] = {
    val oldHasType = old.values.values.forall(_.element.isDefined)
    val newHasType = newValue.element.isDefined
    if (oldHasType == newHasType) {
      new Mapping[NamedField](old.values.updated(newKey, newValue))
    } else if (oldHasType) {
      val (fieldType, names) = old.values.flatMap(e => e._2.element.map(x => (x, e._1))).head
      val remapper = remappers(names, newKey)
      new Mapping[NamedField](old.values.updated(newKey, newValue.withElement(remapper.remap(fieldType))))
    } else if (newHasType) {
      var map = old.values
      for ((names, elem) <- old.values) {
        if (elem.element.isEmpty) {
          val remapper = remappers(newKey, names)
          map = map.updated(names, elem.withElement(remapper.remap(newValue.element.get)))
        }
      }
      new Mapping[NamedField](map.updated(newKey, newValue))
    } else {
      new Mapping[NamedField](old.values.updated(newKey, newValue))
    }
  }
}

