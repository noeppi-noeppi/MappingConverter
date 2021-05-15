package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.util.{NamedField, Side}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MappingMerger {
  
  def mergeMappingLists[T](mappings: Seq[List[Mapping[T]]], lookup: Seq[Map[T, Mapping[T]]], sideExtractor: T => Side, sideApply: (T, Side) => T, safeMap: Option[Mapping[T] => Boolean]): List[Mapping[T]] = {
    val nonEmpty = mappings.filter(_.nonEmpty)
    if (nonEmpty.size == 1) {
      return nonEmpty.head
    }
    val list = ListBuffer[Mapping[T]]()
    val allElements = lookup.flatMap(_.keySet).toSet
    for (elem <- allElements) {
      val matches: Seq[Mapping[T]] = lookup.flatMap(_.get(elem))
      safeMap match {
        case Some(canMap) if matches.size == 1 || (matches.nonEmpty && matches.tail.exists(canMap)) => list.addOne(matches.head)
        case _ => list.addOne(merged(matches, sideExtractor, sideApply))
      }
    }
    list.toList
  }
  
  def merged[T](mappings: Seq[Mapping[T]], sideExtractor: T => Side, sideApply: (T, Side) => T): Mapping[T] = {
    if (mappings.size == 1) {
      return mappings.head
    }
    val map = mutable.Map[Names, T]()
    for (m <- mappings; n <- m.names) {
      if (!map.contains(n)) {
        map.put(n, m.name(n))
      } else {
        val oldElem = map(n)
        val newElem = m.name(n)
        val oldSide = sideExtractor(oldElem)
        val newSide = Side.join(oldSide, sideExtractor(newElem))
        var updated = sideApply(oldElem, newSide)
        //noinspection TypeCheckCanBeMatch
        if (oldElem.isInstanceOf[NamedField] && newElem.isInstanceOf[NamedField]
          && oldElem.asInstanceOf[NamedField].element.isEmpty && newElem.asInstanceOf[NamedField].element.isDefined) {
          updated = oldElem.asInstanceOf[NamedField].withElement(newElem.asInstanceOf[NamedField].element.get).asInstanceOf[T]
        }
        if (newSide != oldSide) {
          map.put(n, sideApply(oldElem, newSide))
        }
      }
    }
    new Mapping[T](map.toMap)
  }
  
  def mergeUnique(mappings: Seq[Map[String, (String, Side, String)]]): Map[String, (String, Side, String)] = {
    val map = mutable.Map[String, (String, Side, String)]()
    for (m <- mappings; entry <- m) {
      if (!map.contains(entry._1)) {
        map.put(entry._1, entry._2)
      } else {
        val oldElem = map(entry._1)
        val newElem = entry._2
        val newSide = Side.merge(oldElem._2, newElem._2)
        val newDoc = if (oldElem._3.isEmpty && newElem._3.nonEmpty) { newElem._3 } else { oldElem._3 }
        if (newSide != oldElem._2 || oldElem._3 != newDoc) {
          map.put(entry._1, (oldElem._1, newSide, newDoc))
        }
      }
    }
    map.toMap
  }
  
  def mergeUniqueParam(mappings: Seq[Map[String, (String, Map[Int, String], Side, String)]]): Map[String, (String, Map[Int, String], Side, String)] = {
    val map = mutable.Map[String, (String, Map[Int, String], Side, String)]()
    for (m <- mappings; entry <- m) {
      if (!map.contains(entry._1)) {
        map.put(entry._1, entry._2)
      } else {
        val oldElem = map(entry._1)
        val newElem = entry._2
        val newSide = Side.merge(oldElem._3, newElem._3)
        // oldElem params will override newElem params
        var newMap = newElem._2
        for (entry <- oldElem._2) {
          newMap = newMap.updated(entry._1, entry._2)
        }
        val newDoc = if (oldElem._4.isEmpty && newElem._4.nonEmpty) { newElem._4 } else { oldElem._4 }
        map.put(entry._1, (oldElem._1, newMap, newSide, newDoc))
      }
    }
    map.toMap
  }
}
