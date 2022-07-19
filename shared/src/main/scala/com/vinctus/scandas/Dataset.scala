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
  private[scandas] val dataArray = data map (_ to ArrayBuffer) to ArrayBuffer

  require(columnNameArray.nonEmpty, "a dataset needs at least one column")
  require(
    isEmpty || dataArray.head.length == cols,
    "the number of data columns should be equal to the number of column names",
  )
  require(
    columnTypeArray.length == 1 || columnTypeArray.length == cols,
    "there should be one type or the same number of types as there are columns",
  )

  if columnTypeArray.length == 1 && cols > 1 then
    for (_ <- 2 to cols)
      columnTypeArray += columnTypeArray.head

  private def convertError(a: Any, to: String, r: Int, c: Int) =
    sys.error(s"conversion error [${r + 1}, ${c + 1}]: '$a' cannot be converted to type '$to'")

  for (c <- columnNameArray.indices) {
    var tempType = columnTypeArray(c)
    var changed = false
    val tempValues = new ArrayBuffer[Any](rows)

    for (r <- dataArray.indices) {
      val d = dataArray(r)(c)
      val prevTempType = tempType

      columnTypeArray(c) match
        case InferType if d == null => tempValues += null
        case InferType =>
          val (t, v) = IntType.convert(d) match
            case None =>
              FloatType.convert(d) match
                case None =>
                  BoolType.convert(d) match
                    case None    => (StringType, String.valueOf(d))
                    case Some(c) => (BoolType, c)
                case Some(c) => (FloatType, c)
            case Some(c) => (IntType, c)

          tempType = t
          tempValues += v
        case t => tempValues += t.convert(d, true) getOrElse convertError(d, t.name, r, c)

      columnTypeArray(c) match
        case InferType =>
          if prevTempType != InferType then
            tempType = (prevTempType, tempType) match
              case (IntType, t @ (IntType | FloatType)) => t
              case (FloatType, FloatType | IntType)     => FloatType
              case (BoolType, BoolType)                 => BoolType
              case _                                    => StringType
            if prevTempType != tempType then changed = true
//        case MixedType => // todo
        case _ =>
    }

    if columnTypeArray(c) == InferType then
      if isEmpty then columnTypeArray(c) = UnknownType
      else
        columnTypeArray(c) = tempType

        if changed || tempType == StringType then
          for (r <- dataArray.indices)
            tempValues(r) =
              tempType.convert(dataArray(r)(c), true) getOrElse convertError(dataArray(r)(c), tempType.name, r, c)
        else
          for (r <- dataArray.indices)
            tempValues(r) =
              tempType.convert(tempValues(r), true) getOrElse convertError(tempValues(r), tempType.name, r, c)

    for (r <- dataArray.indices)
      dataArray(r)(c) = tempValues(r)
  }

  def mean(cidx: Int): Double =
    apply(cidx) match
      case c: Seq[Double] => c.sum / c.length

  def rows: Int = dataArray.length

  def isEmpty: Boolean = rows == 0

  def cols: Int = columnNameArray.length

  def numericalColumnIndices: Seq[Int] = 0 until cols filter (c => columnTypeArray(c).numerical)

  def getRows(fidx: Int, tidx: Int): String =
    new TextTable() {
      headerSeq("" +: columnNameArray)

      for (i <- fidx to tidx)
        rowSeq(i +: dataArray(i))

      1 +: numericalColumnIndices.map(_ + 2) foreach rightAlignment
    }.toString

  def head: String = getRows(0, 9 min (rows - 1))

  def tail: String = getRows(rows - 10 max 0, rows - 1)

  def info(): Unit =
    println(s"<class ${getClass.getName}>")
    println(s"$rows rows; $cols columns")
    println(
      new TextTable() {
        header("#", "Column", "Non-Null Count", "Datatype")

        for (((n, t), i) <- columnNameArray zip columnTypeArray zipWithIndex)
          this.row(i, n, apply(i).count(_ != null), t.name)

        rightAlignment(1)
        rightAlignment(3)
      },
    )

  def shape: (Int, Int) = (rows, cols)

  def row(ridx: Int): IndexedSeq[Any] = dataArray(ridx).toIndexedSeq

  def apply(cname: String): IndexedSeq[Any] = apply(columnNameMap(cname))

  def apply(cidx: Int): IndexedSeq[Any] = dataArray map (_(cidx)) to ArraySeq

  def iterator: Iterator[IndexedSeq[Any]] = dataArray.iterator map (_.toIndexedSeq)

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
