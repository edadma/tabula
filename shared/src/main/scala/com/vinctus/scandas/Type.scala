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

abstract class Type(val name: String):
  def convert(a: Any): Option[Any]

case object InferType extends Type("infer"):
  def convert(a: Any): Option[Any] = sys.error("InferType.convert()")

case object MixedType extends Type("mixed"):
  def convert(a: Any): Option[Any] = sys.error("MixedType.convert()")

case object UnknownType extends Type("unknown"):
  def convert(a: Any): Option[Any] = sys.error("UnknownType.convert()")

case object IntType extends Type("int"):
  private val LongMinDouble = Long.MinValue.toDouble
  private val LongMaxDouble = Long.MaxValue.toDouble

  def convert(a: Any): Option[Any] =
    a match
      case n: Int    => Some(n.toLong)
      case _: Long   => Some(a)
      case n: Double => if LongMinDouble <= n && n <= LongMaxDouble then Some(n.toLong) else None
      case s: String => s.toLongOption
      case _         => None

case object FloatType extends Type("float"):
  def convert(a: Any): Option[Any] =
    a match
      case n: Int    => Some(n.toDouble)
      case n: Long   => Some(n.toDouble)
      case _: Double => Some(a)
      case s: String => s.toDoubleOption
      case _         => None

case object BoolType extends Type("bool"):
  def convert(a: Any): Option[Any] =
    a match
      case "t" | "T" | "true" | "True"   => Some(true)
      case "f" | "F" | "false" | "False" => Some(false)
      case _                             => None

case object StringType extends Type("string"):
  def convert(a: Any): Option[Any] = Some(a.toString)

// todo TimestampType
// todo DateType
