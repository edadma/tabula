package com.vinctus.scandas

abstract class Datatype:
  def parse(s: String): Option[Any]

case object Int extends Datatype:
  def parse(s: String): Option[Any] = s.toIntOption

case object Float extends Datatype:
  def parse(s: String): Option[Any] = s.toDoubleOption

case object String extends Datatype:
  def parse(s: String): Option[Any] = Some(s)

//todo Timestamp
//todo Date
