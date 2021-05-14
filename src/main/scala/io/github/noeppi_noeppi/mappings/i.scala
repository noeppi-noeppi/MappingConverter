package io.github.noeppi_noeppi.mappings

import com.google.gson.{JsonArray, JsonElement}

object i {

  implicit class ImplicitJsonArray(array: JsonArray) extends Iterable[JsonElement] {
    
    override def iterator: Iterator[JsonElement] = new Iterator[JsonElement]() {
      private var idx = 0
      override def hasNext: Boolean = array.size() > idx
      override def next(): JsonElement = {
        val elem = array.get(idx)
        idx += 1
        elem
      }
    }
  }
}
