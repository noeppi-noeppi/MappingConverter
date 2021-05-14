package io.github.noeppi_noeppi.mappings.format

import io.github.noeppi_noeppi.mappings.mappings.{Mappings, Names}

import java.io._

trait MappingFormat {

  def required: Set[Names]
  def read(in: InputStream): Mappings
  def write(out: OutputStream, mappings: Mappings): Unit
}

trait MappingFormatText extends MappingFormat {
  
  def read(in: BufferedReader): Mappings
  def write(out: BufferedWriter, mappings: Mappings): Unit

  override def read(in: InputStream): Mappings = read(new BufferedReader(new InputStreamReader(in)))
  override def write(out: OutputStream, mappings: Mappings): Unit = {
    val w = new BufferedWriter(new OutputStreamWriter(out))
    write(w, mappings)
    w.flush()
  }
}