package com.vinctus.scandas

object Datatype:
  def from(dtype: String): Datatype =
    dtype match
      case "Infer" | "infer"   => Infer
      case "Mixed" | "mixed"   => Mixed
      case "Int" | "int"       => Int
      case "Float" | "float"   => Float
      case "String" | "string" => String

abstract class Datatype:
  def parse(s: String): Option[Any]

case object Infer extends Datatype:
  def parse(s: String): Option[Any] = sys.error("can't parse")

case object Mixed extends Datatype:
  def parse(s: String): Option[Any] = sys.error("can't parse")

case object Int extends Datatype:
  def parse(s: String): Option[Any] = s.toIntOption

case object Float extends Datatype:
  def parse(s: String): Option[Any] = s.toDoubleOption

case object String extends Datatype:
  def parse(s: String): Option[Any] = Some(s)

//todo Timestamp
//todo Date
