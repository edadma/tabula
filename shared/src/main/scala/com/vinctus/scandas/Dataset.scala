package com.vinctus.scandas

import io.github.edadma.csv.CSVRead
import io.github.edadma.matrix.Matrix
import io.github.edadma.table.{ASCII, TextTable}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class Dataset(
    columns: Seq[String],
    data: collection.Seq[collection.Seq[Any]],
    types: Seq[Type] = Seq(InferType),
):
  private val columnNameArray = ArrayBuffer from columns
  private val columnNameMap = new mutable.HashMap[String, Int]
  private val columnTypeArray = ArrayBuffer from types
  private val dataArray = data map (_ to ArrayBuffer) to ArrayBuffer
  private val nonNullCounts = ArrayBuffer.fill(cols)(0)

  require(columnNameArray.nonEmpty, "a dataset needs at least one column")
  require(dataArray.head.length == cols, "the number of data columns should be equal to the number of column names")
  require(
    columnTypeArray.length == 1 || columnTypeArray.length == cols,
    "there should be one type or the same number of types as there are columns",
  )

  if columnTypeArray.length == 1 && cols > 1 then
    for (i <- 2 to cols)
      columnTypeArray += columnTypeArray.head

  private val LongMinDouble = Long.MinValue.toDouble
  private val LongMaxDouble = Long.MaxValue.toDouble

  private def convertError(a: Any, to: String, r: Int, c: Int) =
    sys.error(s"conversion error [$r, $c]: '$a' cannot be converted to type '$to'")

  for (c <- columnNameArray.indices) {
    var tempType = columnTypeArray(c)
    val tempValues = new ArrayBuffer[Any](rows)

    for (r <- dataArray.indices) {
      val d = dataArray(r)(c)
      val prevTempType = tempType

      columnTypeArray(c) match
        case InferType | MixedType =>
          tempValues(r) = IntType.convert(d) match
            case None =>
              FloatType.convert(d) match
                case None =>
                  BoolType.convert(d) match
                    case None =>
                      tempType = StringType
                      d.toString
                    case Some(v) =>
                      tempType = BoolType
                      v
                case Some(v) =>
                  tempType = FloatType
                  v
            case Some(v) =>
              tempType = IntType
              v
        case t => tempValues(r) = t.convert(d) getOrElse convertError(d, t, r, c)

      columnTypeArray(c) match
        case InferType =>
          if prevTempType != InferType then
            tempType = (prevTempType, tempType) match
              case (IntType, t @ (IntType | FloatType)) => t
              case (FloatType, FloatType | IntType)     => FloatType
              case (BoolType, BoolType)                 => BoolType
              case _                                    => StringType
        case MixedType => // todo
    }

    if columnTypeArray(c) == InferType then
      columnTypeArray(c) = tempType

      for (r <- dataArray.indices)
        tempValues(r) = tempType.convert(tempValues(r))

    for (r <- dataArray.indices)
      dataArray(r)(c) = tempValues(r)
  }

  def col(name: String): Seq[Any] = col(columnNameMap(name))

  def col(cidx: Int): Seq[Any] = data map (_(cidx)) to ArraySeq

  def mean(cidx: Int): Double =
    col(cidx) match
      case c: Seq[Double] => c.sum / c.length

  def rows: Int = dataArray.length

  def cols: Int = columnNameArray.length

  def getRows(fidx: Int, tidx: Int): String =
    new TextTable() {
      headerSeq("" +: columnNameArray)

      for (i <- fidx to tidx)
        rowSeq(i +: dataArray(i))

      1 to cols foreach rightAlignment
    }.toString

  def head: String = getRows(0, 9 min (rows - 1))

  def tail: String = getRows(rows - 10 max 0, rows - 1)

  def info(): Unit =
    println(columnNameArray)
    println(columnTypeArray)
    println(getClass.getName)
    println(s"$rows rows; $cols columns")
    println(
      new TextTable() {
        header("#", "Column", "Non-Null Count", "Datatype")

        for ((((n, t), c), i) <- columnNameArray zip columnTypeArray zip nonNullCounts zipWithIndex)
          row(i, n, c, t)

        rightAlignment(1)
        rightAlignment(3)
      },
    )

  override def toString: String = head

object Dataset:

//  def apply(columns: collection.Seq[String], data: Matrix[Double]): Dataset =
//    new Dataset(columns, data)
//
//  def apply(columns: collection.Seq[String], data: Seq[Seq[Any]]): Dataset =
//    new Dataset(columns, Matrix.fromArray(data map (_ map (_.asInstanceOf[Number].doubleValue) toArray) toArray))

  def fromString(s: String): Dataset =
    val csv = CSVRead.fromFile(s).get
    val (header, data) = (csv.head, csv drop 1)

    Dataset(header, data)

  def fromCSV(file: String, columns: Seq[String] = null): Dataset =
    val csv = CSVRead.fromFile(file).get
    val (header, data) =
      if (columns eq null) (csv.head, csv drop 1)
      else (columns, csv)

    Dataset(header, data map (_ map (_.toDouble)))

// todo: columns: Seq[String] | Int // so that you can specify starting column number
