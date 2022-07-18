package com.vinctus.scandas

object Type:
  def from(dtype: String): Type =
    dtype match
      case "Infer" | "infer"   => InferType
      case "Mixed" | "mixed"   => MixedType
      case "Int" | "int"       => IntType
      case "Float" | "float"   => FloatType
      case "String" | "string" => StringType
      case "Bool" | "bool"     => BoolType

abstract class Type:
  def convert(a: Any): Option[Any]

case object InferType extends Type:
  def convert(a: Any): Option[Any] = sys.error("InferType.convert()")

case object MixedType extends Datatype:
  def convert(a: Any): Option[Any] = sys.error("MixedType.convert()")

case object IntType extends Type:
  def convert(a: Any): Option[Any] =
    a match
      case n: Int    => Some(n.toLong)
      case _: Long   => Some(a)
      case n: Double => if LongMinDouble <= n && n <= LongMaxDouble then Some(n.toLong) else None
      case s: String => s.toLongOption
      case _         => None

case object FloatType extends Type:
  def convert(a: Any): Option[Any] =
    a match
      case n: Int    => Some(n.toDouble)
      case n: Long   => Some(n.toDouble)
      case _: Double => Some(a)
      case s: String => s.toDoubleOption
      case _         => None

case object BoolType extends Type:
  def convert(a: Any): Option[Any] =
    a match
      case "t" | "T" | "true" | "True"   => Some(true)
      case "f" | "F" | "false" | "False" => Some(false)
      case _                             => None

case object StringType extends Type:
  def convert(a: Any): Option[Any] = Some(a.toString)

// todo TimestampType
// todo DateType
