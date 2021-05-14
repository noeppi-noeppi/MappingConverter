package io.github.noeppi_noeppi.mappings.mappings

import io.github.noeppi_noeppi.mappings.format.{MappingFormat, MappingFormatText}

import java.io.{BufferedReader, BufferedWriter, InputStream, OutputStream}

object MappingIO {
  
  def read(format: MappingFormat, in: InputStream): Mappings = {
    val mappings = format.read(in)
    format.required.find(n => !mappings.has(n)) match {
      case Some(x) => throw new IllegalStateException("Mapping format '" + format + "' did not read required names '" + x + "'")
      case None => mappings
    }
  }
  
  def read(format: MappingFormatText, in: BufferedReader): Mappings = {
    val mappings = format.read(in)
    format.required.find(n => !mappings.has(n)) match {
      case Some(x) => throw new IllegalStateException("Mapping format '" + format + "' did not read required names '" + x + "'")
      case None => mappings
    }
  }
  
  def write(format: MappingFormat, out: OutputStream, mappings: Mappings): Unit = {
    format.required.find(n => !mappings.has(n)) match {
      case Some(x) => throw new IllegalStateException("Can't write mapping with format '" + format + ": Missing required names '" + x + "'")
      case None => format.write(out, mappings)
    }
  }
  
  def write(format: MappingFormatText, out: BufferedWriter, mappings: Mappings): Unit = {
    format.required.find(n => !mappings.has(n)) match {
      case Some(x) => throw new IllegalStateException("Can't write mapping with format '" + format + ": Missing required names '" + x + "'")
      case None => format.write(out, mappings)
    }
  }
}
