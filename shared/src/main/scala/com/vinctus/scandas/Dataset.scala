package com.vinctus.scandas

import io.github.edadma.csv.CSVRead
import io.github.edadma.matrix.Matrix
import io.github.edadma.table.{ASCII, TextTable}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class Dataset private (
    private val columnNameArray: ArrayBuffer[String],
    private val columnNameMap: mutable.HashMap[String, Int],
    private val dataArray: ArrayBuffer[ArrayBuffer[Any]],
    private val columnTypeArray: ArrayBuffer[Type],
    private val rowIndexArray: ArrayBuffer[Any],
):
  def min(cidx: Int): Double = columnNonNullNumericalIterator(cidx).min

  def max(cidx: Int): Double = columnNonNullNumericalIterator(cidx).max

  def mean(cidx: Int): Double = columnNonNullNumericalIterator(cidx).sum / count(cidx)

  def count(cidx: Int): Int = columnNonNullIterator[Any](cidx).count(_ => true)

  def std(cidx: Int): Double =
    val m = mean(cidx)

    math.sqrt((columnNonNullNumericalIterator(cidx) map (a => (a - m) * (a - m)) sum) / (rows - 1))

  def rows: Int = dataArray.length

  def isEmpty: Boolean = rows == 0

  def cols: Int = columnNameArray.length

  def numericalColumnIndices: Seq[Int] = 0 until cols filter (c => columnTypeArray(c).numerical)

  def table(fidx: Int, tidx: Int): String =
    new TextTable() {
      headerSeq("" +: columnNameArray)

      for (i <- fidx to tidx)
        rowSeq(rowIndexArray(i) +: dataArray(i))

      1 +: numericalColumnIndices.map(_ + 2) foreach rightAlignment
    }.toString

  def head: String = table(0, 9 min (rows - 1))

  def tail: String = table(rows - 10 max 0, rows - 1)

  def print(): Unit = println(table(0, rows - 1))

  def info(): Unit =
    println(s"<class ${getClass.getName}>")
    println(s"$rows rows; $cols columns")
    println(
      new TextTable() {
        header("#", "Column", "Non-Null Count", "Datatype")

        for (((n, t), i) <- columnNameArray zip columnTypeArray zipWithIndex)
          this.row(i, n, count(i), t.name)

        rightAlignment(1)
        rightAlignment(3)
      },
    )

  // https://statisticsbyjim.com/basics/percentiles/
  def percentile(cidx: Int, percent: Int): Double =
    val data = columnNonNullNumericalIterator(cidx).toIndexedSeq.sorted
    val p = percent / 100d
    val rank = p * (data.length + 1)
    val upperIndex = rank.toInt
    val lowerIndex = upperIndex - 1
    val lower = data(lowerIndex)

    if rank.isWhole then lower
    else (data(upperIndex) - lower) * (rank - rank.toInt) + lower

  def q1(cidx: Int): Double = percentile(cidx, 25)

  def q2(cidx: Int): Double = percentile(cidx, 50)

  def q3(cidx: Int): Double = percentile(cidx, 75)

  def describe: Dataset =
    val fs = Seq(count, mean, std, min, q1, q2, q3, max)
    val cs = numericalColumnIndices
    val data = fs map (f => cs map f)
    val ds = Dataset(cs map columnNameArray, data)

    ds.index(Seq("count", "mean", "std", "min", "q1", "q2", "q3", "max"))
    ds

//  def sample(n: Int): Dataset =
//    require(n >= 0, "number of samples must be non-negative")
//
//    val indices = new mutable.HashSet[Int]
//
//    while indices.size < n do indices += Random.nextInt(rows)

  def shape: (Int, Int) = (rows, cols)

  def row(ridx: Int): IndexedSeq[Any] = dataArray(ridx).toIndexedSeq

  def apply(cname: String): IndexedSeq[Any] = apply(columnNameMap(cname))

  def apply(cidx: Int): IndexedSeq[Any] = dataArray map (_(cidx)) to ArraySeq

  def columnNonNullIterator[T](cidx: Int): Iterator[T] =
    (dataArray.iterator map (_(cidx)) filter (_ != null)).asInstanceOf[Iterator[T]]

  def columnNonNullNumericalIterator(cidx: Int): Iterator[Double] =
    columnNonNullIterator[Number](cidx) map (_.doubleValue)

  def iterator: Iterator[IndexedSeq[Any]] = dataArray.iterator map (_.toIndexedSeq)

  def index(s: Seq[Any]): Unit =
    require(s.length == rows, "sequence of indices should be the same length as the number of rows")
    s.zipWithIndex foreach { case (v, i) => rowIndexArray(i) = v }

  override def toString: String = head

object Dataset:

//  def apply(columns: collection.Seq[String], data: Matrix[Double]): Dataset =
//    new Dataset(columns, data)

  def apply(
      columns: collection.Seq[String],
      data: Seq[Seq[Any]],
      types: Seq[Type] = Seq(InferType),
      indices: Seq[Any] = Nil,
  ): Dataset =
    val columnNameArray = ArrayBuffer from columns
    val columnNameMap = columnNameArray.zipWithIndex to mutable.HashMap
    val dataArray = data map (_ to ArrayBuffer) to ArrayBuffer
    val columnTypeArray = ArrayBuffer from types
    val rowIndexArray: ArrayBuffer[Any] = dataArray.indices to ArrayBuffer

    require(columnNameArray.nonEmpty, "a dataset needs at least one column")
    require(
      dataArray.isEmpty || dataArray.head.length == columnNameArray.length,
      "the number of data columns should be equal to the number of column names",
    )
    require(
      columnTypeArray.length == 1 || columnTypeArray.length == columnNameArray.length,
      "there should be one type or the same number of types as there are columns",
    )

    if columnTypeArray.length == 1 && columnNameArray.length > 1 then
      for (_ <- 2 to columnNameArray.length)
        columnTypeArray += columnTypeArray.head

    def convertError(a: Any, to: String, r: Int, c: Int) =
      sys.error(s"conversion error [${r + 1}, ${c + 1}]: '$a' cannot be converted to type '$to'")

    for (c <- columnNameArray.indices) {
      var tempType = columnTypeArray(c)
      var changed = false
      val tempValues = new ArrayBuffer[Any](dataArray.length)

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
        if dataArray.isEmpty then columnTypeArray(c) = UnknownType
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

    new Dataset(
      columnNameArray,
      columnNameMap,
      dataArray,
      columnTypeArray,
      rowIndexArray,
    )

  def fromString(s: String): Dataset =
    val csv = CSVRead.fromFile(s).get
    val (header, data) = (csv.head, csv drop 1)

    Dataset(header, data)

  def fromCSV(file: String, columns: Seq[String] = null): Dataset =
    val csv = CSVRead.fromFile(file).get
    val (header, data) =
      if (columns eq null) (csv.head, csv drop 1)
      else (columns, csv)

    Dataset(header, data)
