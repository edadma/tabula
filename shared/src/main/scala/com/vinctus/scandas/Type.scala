package com.vinctus.scandas

import java.time.{Instant, LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.util.{Try, Success, Failure}

object Type:
  def from(dtype: String): Type =
    dtype match
      case "Infer" | "infer" => InferType
//      case "Mixed" | "mixed"   => MixedType
      case "Int" | "int"       => IntType
      case "Float" | "float"   => FloatType
      case "String" | "string" => StringType
      case "Bool" | "bool"     => BoolType

abstract class Type(val name: String, val numerical: Boolean):
  def convert(a: Any, coerse: Boolean = false): Option[Any]

case object InferType extends Type("infer", false):
  def convert(a: Any, coerse: Boolean): Option[Any] = sys.error("InferType.convert()")

//case object MixedType extends Type("mixed", false):
//  def convert(a: Any, coerse: Boolean): Option[Any] = sys.error("MixedType.convert()")

case object UnknownType extends Type("unknown", false):
  def convert(a: Any, coerse: Boolean): Option[Any] = sys.error("UnknownType.convert()")

case object IntType extends Type("int", true):
  private val LongMinDouble = Long.MinValue.toDouble
  private val LongMaxDouble = Long.MaxValue.toDouble

  def convert(a: Any, coerse: Boolean = false): Option[Any] =
    a match
      case null                             => Some(null)
      case n: Int                           => Some(n.toLong)
      case _: Long                          => Some(a)
      case n: Double if coerse || n.isWhole => if LongMinDouble <= n && n <= LongMaxDouble then Some(n.toLong) else None
      case s: String => s.toLongOption orElse (if coerse then s.toDoubleOption.map(_.toLong) else None)
      case _         => None

case object FloatType extends Type("float", true):
  def convert(a: Any, coerse: Boolean = false): Option[Any] =
    a match
      case null      => Some(null)
      case n: Int    => Some(n.toDouble)
      case n: Long   => Some(n.toDouble)
      case _: Double => Some(a)
      case s: String => s.toDoubleOption
      case _         => None

case object BoolType extends Type("bool", false):
  def convert(a: Any, coerse: Boolean = false): Option[Any] =
    a match
      case null                          => Some(null)
      case b: Boolean                    => Some(b)
      case "t" | "T" | "true" | "True"   => Some(true)
      case "f" | "F" | "false" | "False" => Some(false)
      case _                             => None

case object StringType extends Type("string", false):
  def convert(a: Any, coerse: Boolean = false): Option[Any] = Some(if a == null then null else a.toString)

case object TimestampType extends Type("timestamp", false):
  def convert(a: Any, coerse: Boolean = false): Option[Any] =
    a match
      case null       => Some(null)
      case t: Instant => Some(t)
      case s: String =>
        Try(ZonedDateTime.parse(s)) match
          case Success(t) => Some(t.toInstant)
          case _ =>
            Try(LocalDateTime.parse(s)) match
              case Success(t) => Some(t.toInstant(ZoneOffset.UTC))
              case _          => None
      case _ => None
