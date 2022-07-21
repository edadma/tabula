package com.vinctus.scandas

import io.github.edadma.csv.CSVRead
import io.github.edadma.matrix.Matrix
import io.github.edadma.table.{ASCII, TextTable}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class Dataset protected (
    private val columnNameMap: Map[String, Int],
    val columnNames: Vector[String],
    private val dataArray: Vector[Vector[Any]],
    val columnTypes: Vector[Type],
):
  lazy val columnNamesSet: Set[String] = columnNames.toSet

  def min(cidx: Int): Double = columnNonNullNumericalIterator(cidx).min

  def max(cidx: Int): Double = columnNonNullNumericalIterator(cidx).max

  def mean(cidx: Int): Double = columnNonNullNumericalIterator(cidx).sum / count(cidx)

  def count(cidx: Int): Int = columnNonNullIterator(cidx).length

  def std(cidx: Int): Double =
    val m = mean(cidx)

    math.sqrt((columnNonNullNumericalIterator(cidx) map (a => (a - m) * (a - m)) sum) / (rows - 1))

  def rows: Int = dataArray.length

  def isEmpty: Boolean = rows == 0

  def cols: Int = columnNames.length

  def numericalColumnIndices: Seq[Int] = 0 until cols filter (c => columnTypes(c).numerical)

  def table(from: Int, until: Int): String =
    new TextTable() {
      headerSeq("" +: columnNames)

      for (i <- from until until)
        rowSeq(dataArray(i).map {
          case v: Double => f"$v%.3f"
          case v         => v
        })

      1 +: numericalColumnIndices.map(_ + 2) foreach rightAlignment
    }.toString

  def head(n: Int = 10): Dataset = slice(0, n min rows)

  def tail(n: Int = 10): Dataset = slice(rows - n max 0, rows)

  def print(): Unit = println(table(0, rows))

  def info(): Unit =
    println(s"<class ${getClass.getName}>")
    println(s"$rows rows; $cols columns")
    println(
      new TextTable() {
        header("#", "Column", "Non-Null Count", "Datatype")

        for (((n, t), i) <- columnNames zip columnTypes zipWithIndex)
          this.row(i, n, count(i), t.name)

        rightAlignment(1)
        rightAlignment(3)
      },
    )

  // https://statisticsbyjim.com/basics/percentiles/
  def percentile(cidx: Int, percent: Int): Double =
    columnIndexCheck(cidx)
    val data = columnNonNullNumericalIterator(cidx).toIndexedSeq.sorted

    if data.isEmpty then Double.NaN
    else if data.length < 3 then data.head
    else
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

    if cs.isEmpty then Dataset(Seq("EMPTY"), Nil) // todo: pandas.describe() when there are no numeric columns
    else
      val data = fs map (f => cs map f)
      val ds =
        Dataset(cs map columnNames, data, indices = Seq("count", "mean", "std", "min", "25%", "50%", "75%", "max"))

      ds

  def slice(from: Int, until: Int): Dataset =
    new Dataset(
      columnNameMap,
      columnNames,
      dataArray.slice(from, until),
      columnTypes,
    )

  protected def removeElement[T](idx: Int, vec: Vector[T]): Vector[T] =
    val (left, right) = vec.splitAt(idx)

    left ++ right.drop(1)

  def remove(cidx: Int): Dataset =
    columnIndexCheck(cidx)

    new Dataset(
      columnNameMap.removed(columnNames(cidx)),
      removeElement(cidx, columnNames),
      dataArray map (r => removeElement(cidx + 1, r)),
      removeElement(cidx, columnTypes),
    )

  protected def insertElement[T](idx: Int, elems: Vector[T], vec: Vector[T]): Vector[T] =
    val (left, right) = vec.splitAt(idx)

    left ++ elems ++ right

  def insert(cidx: Int, ds: Dataset): Dataset =
    columnIndexCheck(cidx)

    if columnNamesSet.intersect(ds.columnNamesSet).nonEmpty then sys.error("insert: duplicate column name")

    new Dataset(
      columnNameMap map { case (k, v) =>
        if v >= cidx then (k, v + ds.cols)
        else (k, v)
      },
      (columnNames take cidx) ++ ds.columnNames ++ (columnNames drop cidx),
      dataArray zip ds.dataArray map { case (thisr, thatr) =>
        (thisr take cidx + 1) ++ thatr.tail ++ (thisr drop cidx + 1)
      },
      (columnTypes take cidx) ++ ds.columnTypes ++ (columnTypes drop cidx),
    )

  def sample(n: Int): Dataset =
    require(n >= 0, "number of samples must be non-negative")

    val indicesSet = new mutable.HashSet[Int]
    val count = n min rows

    while indicesSet.size < count do indicesSet += Random.nextInt(rows)

    new Dataset(
      columnNameMap,
      columnNames,
      indicesSet.toVector.iterator map dataArray toVector,
      columnTypes,
    )

  def shape: (Int, Int) = (rows, cols)

  def row(ridx: Int): IndexedSeq[Any] = dataArray(ridx).tail

  def apply(cname: String): IndexedSeq[Any] = apply(columnNameMap(cname))

  protected def columnIndexCheck(cidx: Int): Unit =
    require(0 <= cidx && cidx < cols, "column index ranges from 0 to number of columns - 1")

  def apply(cidx: Int): IndexedSeq[Any] =
    columnIndexCheck(cidx)
    dataArray map (_(cidx + 1)) to ArraySeq

  def columnNonNullIterator[T](cidx: Int): Iterator[T] =
    columnIndexCheck(cidx)
    (dataArray.iterator map (_(cidx + 1)) filter (_ != null)).asInstanceOf[Iterator[T]]

  def columnNonNullNumericalIterator(cidx: Int): Iterator[Double] =
    columnIndexCheck(cidx)
    columnNonNullIterator[Number](cidx) map (_.doubleValue)

  def iterator: Iterator[IndexedSeq[Any]] = dataArray.iterator map (_ drop 1)

  def index(indices: Seq[Any]): Dataset =
    require(indices.length == rows, "sequence of indices should be the same length as the number of rows")
    new Dataset(
      columnNameMap,
      columnNames,
      dataArray zip indices map { case (r, i) => i +: r.tail },
      columnTypes,
    )

  def toArray: ArraySeq[ArraySeq[Any]] = iterator map (_ to ArraySeq) to ArraySeq

  override def toString: String =
    val n = 10

    table(0, n min rows)

object Dataset:

  def apply(
      columns: collection.Seq[String],
      data: Seq[Seq[Any]],
      types: Seq[Type] = Seq(InferType),
      indices: Seq[Any] = Nil,
  ): Dataset =
    val columnNameMap = columns.zipWithIndex.toMap
    val columnNameArray = Vector from columns
    val dataArray = data map (_ to ArrayBuffer) to ArrayBuffer
    val columnTypeArray = ArrayBuffer from types
    val rowIndexArray = (if indices.isEmpty then dataArray.indices else indices).toVector

    require(columnNameArray.nonEmpty, "a dataset needs at least one column")
    require(columnNameArray.distinct.length == columnNameArray.length, "column names must be distinct")
    require(
      dataArray.isEmpty || dataArray.forall(_.length == columnNameArray.length),
      "the number of data columns should be equal to the number of column names",
    )
    require(
      columnTypeArray.length == 1 || columnTypeArray.length == columnNameArray.length,
      "there should be one type or the same number of types as there are columns",
    )
    require(
      rowIndexArray.length == dataArray.length,
      "there should be no indices or the same number of indices as there are rows",
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
                      case None =>
                        TimestampType.convert(d) match
                          case None    => (StringType, String.valueOf(d))
                          case Some(c) => (TimestampType, c)
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
      columnNameMap,
      columnNameArray,
      dataArray zip rowIndexArray map { case (r, i) => (i +: r).toVector } toVector,
      columnTypeArray.toVector,
    )

  def fromString(s: String): Dataset =
    val csv = CSVRead.fromString(s).get
    val (header, data) = (csv.head, csv drop 1)

    Dataset(header, data)

  def fromCSV(file: String, columns: Seq[String] = null): Dataset =
    val csv = CSVRead.fromFile(file).get
    val (header, data) =
      if (columns eq null) (csv.head, csv drop 1)
      else (columns, csv)

    Dataset(header, data)
