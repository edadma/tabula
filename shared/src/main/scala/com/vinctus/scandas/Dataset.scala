package com.vinctus.scandas

import io.github.edadma.csv.CSVRead
import io.github.edadma.importer.{Importer, Table}
import io.github.edadma.json.DefaultJSONReader
import io.github.edadma.matrix.Matrix
import io.github.edadma.table.{ASCII, TextTable}

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.{dynamics, postfixOps}
import scala.util.Random
import math.*

class Dataset protected (
    val columnNameMap: Map[String, Int],
    val columnNames: Vector[String],
    val dataArray: Vector[Vector[Any]],
    val columnTypes: Vector[Type],
) extends collection.immutable.AbstractSeq[Vector[Any]]
    with Dynamic:
  lazy val columnSet: Set[String] = columnNames.toSet

  protected val LIMIT = 5

  protected def columnData(data: Vector[Any], typ: Type): Dataset =
    new Dataset(
      Map("0" -> 0),
      Vector("0"),
      data.zipWithIndex map { case (v, i) => Vector(i, v) },
      Vector(typ),
    )

  def columnType(cname: String): Type =
    columnNameCheck(cname)
    columnTypes(columnNameMap(cname))

  def all: Dataset = columnData(dataArray map (r => r.tail forall (_.asInstanceOf[Boolean])), BoolType)

  def any: Dataset = columnData(dataArray map (r => r.tail exists (_.asInstanceOf[Boolean])), BoolType)

  protected def booleanData(data: Vector[Vector[Any]]): Dataset =
    new Dataset(
      columnNameMap,
      columnNames,
      data,
      Vector.fill(cols)(BoolType),
    )

  protected def transformSameType(
      fl: Option[Long => Long],
      fd: Option[Double => Double],
      fi: Option[Instant => Instant],
      fs: Option[String => String],
      fb: Option[Boolean => Boolean],
  ): Vector[Vector[Any]] =
    dataArray map (r =>
      r.head +: (r.tail map {
        case l: Long if fl.isDefined    => fl.get(l)
        case d: Double if fd.isDefined  => fd.get(d)
        case i: Instant if fi.isDefined => fi.get(i)
        case s: String if fs.isDefined  => fs.get(s)
        case b: Boolean if fb.isDefined => fb.get(b)
        case x                          => sys.error(s"type error during transform: $x of class ${x.getClass}")
      })
    )

  protected def transformDifferentType(f: Any => Any): Vector[Vector[Any]] =
    dataArray map (r => r.head +: (r.tail map f))

  protected def operator[A, B](ds: Dataset, f: (A, A) => B): Vector[Vector[Any]] =
    shapeCheck(ds)

    val tupled = f.tupled.asInstanceOf[((Any, Any)) => Any]

    dataArray zip ds.dataArray map { case (r, d) => r.head +: (r.tail zip d.tail map tupled) }

  protected def combine[A, B](ds: Dataset, f: (A, A) => B): Vector[Vector[Any]] =
    shapeCheck(ds)

    val tupled = f.tupled.asInstanceOf[((Any, Any)) => Any]

    dataArray zip ds.dataArray map { case (r, d) => r.tail zip d.tail map tupled }

  protected def predicate[T](p: T => Boolean): Dataset = booleanData(transformDifferentType(p.asInstanceOf[Any => Any]))

  def >(a: Number): Dataset = predicate[Number](_.doubleValue > a.doubleValue)

  def >(a: Dataset): Dataset = comparison(_.doubleValue > _.doubleValue, a)

  def >=(a: Number): Dataset = predicate[Number](_.doubleValue >= a.doubleValue)

  def <(a: Number): Dataset = predicate[Number](_.doubleValue < a.doubleValue)

  def <(a: Dataset): Dataset = comparison(_.doubleValue < _.doubleValue, a)

  def <=(a: Number): Dataset = predicate[Number](_.doubleValue <= a.doubleValue)

  def ==(a: Long): Dataset = predicate[Number](_.longValue == a)

  def !=(a: Long): Dataset = predicate[Number](_.longValue != a)

  def ==(a: Double): Dataset = predicate[Double](_.doubleValue == a.doubleValue)

  def !=(a: Double): Dataset = predicate[Double](_.doubleValue != a.doubleValue)

  def ==(a: Boolean): Dataset = predicate[Boolean](_ == a)

  def !=(a: Boolean): Dataset = predicate[Boolean](_ != a)

  def ==(a: String): Dataset = predicate[String](_ == a)

  def !=(a: String): Dataset = predicate[String](_ != a)

  def >(a: Instant): Dataset = predicate[Instant](_ isAfter a)

  def >=(a: Instant): Dataset = predicate[Instant](t => !(t isBefore a))

  def <(a: Instant): Dataset = predicate[Instant](_ isBefore a)

  def <=(a: Instant): Dataset = predicate[Instant](t => !(t isAfter a))

  def ==(a: Instant): Dataset = predicate[Instant](_ == a)

  def !=(a: Instant): Dataset = predicate[Instant](_ != a)

  def &&(ds: Dataset): Dataset = connective(_ && _, ds)

  def ||(ds: Dataset): Dataset = connective(_ || _, ds)

  def unary_! : Dataset = predicate[Boolean](!_)

  protected def comparison(pred: (Number, Number) => Boolean, ds: Dataset): Dataset = booleanData(
    operator[Number, Boolean](ds, pred),
  )

  protected def connective(op: (Boolean, Boolean) => Boolean, ds: Dataset): Dataset = booleanData(
    operator[Boolean, Boolean](ds, op),
  )

  def operation(
      fl: Option[Long => Long],
      fd: Option[Double => Double],
      fi: Option[Instant => Instant],
      fs: Option[String => String],
      fb: Option[Boolean => Boolean],
  ): Dataset = dataset(transformSameType(fl, fd, fi, fs, fb))

  def apply(f: Seq[Any] => Seq[Any]): Dataset =
    val columns = for (c <- 0 until cols) yield f(columnNonNull(c))
    val data = (for (r <- 0 until rows) yield dataArray(r).head +: columns.map(_(r)).toVector).toVector

    dataset(data)

  protected def columnSample(cidx: Int, f: Seq[Any] => Any): Any = f(columnNonNull(cidx))

  def applyColumn(f: Seq[Any] => Any): Dataset =
    val columns = for (c <- 0 until cols) yield columnSample(c, f)
    val data = Vector(0 +: columns.toVector)

    dataset(data)

  def +(a: Long): Dataset = operation(
    Some(_ + a),
    Some(_ + a),
    Some(_.plus(a, ChronoUnit.MILLIS)),
    Some(_.toLongOption map (l => (l + a).toString) getOrElse sys.error("not an integer")),
    None,
  )

  //  def -(a: Double): Dataset = operation(_ - a)

  protected def ni: Nothing = sys.error("not implemented (yet)")

  protected def shapeCheck(ds: Dataset): Unit =
    require(shape == ds.shape, s"datasets don't have the same shape: $shape, ${ds.shape}")

  def -(ds: Dataset): Dataset =
    require(cols == 1, "can only subtract a single column dataset (for now)")
    shapeCheck(ds)

    (columnTypes.head, ds.columnTypes.head) match
      case (IntType, IntType)                                                   => ni
      case (FloatType, IntType) | (IntType, FloatType) | (FloatType, FloatType) => ni
      case (TimestampType, TimestampType) =>
        Dataset(combine[Instant, Long](ds, (t1: Instant, t2: Instant) => Duration.between(t1, t2).toSeconds))
      case _ => sys.error(s"type mismatch: ${columnTypes.head}, ${ds.columnTypes.head}")

  def *(a: Long): Dataset = operation(
    Some(_ * a),
    Some(_ * a),
    None,
    Some(_.toLongOption map (l => (l * a).toString) getOrElse sys.error("not an integer")),
    None,
  )

  def *(a: Double): Dataset = operation(
    Some(d => (d * a).toLong),
    Some(_ * a),
    None,
    Some(_.toDoubleOption map (l => (l * a).toString) getOrElse sys.error("not a number")),
    None,
  )

  //  def /(a: Double): Dataset = operation(_ / a)
  //
  //  def abs: Dataset = operation(_.abs)
  //
  //  def ceil: Dataset = operation(_.ceil)
  //
  //  def floor: Dataset = operation(_.floor)
  //
  //  def round: Dataset = operation(math.rint)

  def min: Dataset = applyColumn(Sample.min)

  def max: Dataset = applyColumn(Sample.max)

  def mean: Dataset = applyColumn(Sample.mean)

  def count(axis: Axis = Axis.COLUMN): Dataset = applyColumn(Sample.count)

  def std: Dataset = applyColumn(Sample.std)

  def sem: Dataset = applyColumn(Sample.sem)

  def s2: Dataset = applyColumn(Sample.s2)

  def zcode: Dataset = apply(Sample.zcode)

  def rows: Int = dataArray.length

  def cols: Int = columnNames.length

  def numericalColumnIndices: Seq[Int] = 0 until cols filter (c => columnTypes(c).numerical)

  def table(from: Int, until: Int, limit: Int = LIMIT): String =
    new TextTable() {
      def tableRows(f: Int, u: Int): Unit =
        for (i <- f until u)
          rowSeq(dataArray(i).map {
            case v: Double => f"$v%.4f"
            case v         => v
          })

      headerSeq("" +: columnNames)

      if until - from > 2 * limit then
        tableRows(from, from + limit)
        rowSeq(".." +: Seq.fill(cols)("..."))
        tableRows(until - limit, until)
      else tableRows(from, until)

      1 +: numericalColumnIndices.map(_ + 2) foreach rightAlignment
    }.toString

  def head(n: Int = 5): Dataset = rowSlice(0, n min rows)

  def tail(n: Int = 5): Dataset = rowSlice(rows - n max 0, rows)

  def print(): Unit = println(table(0, rows, rows))

  def info(): Unit =
    println(s"<class ${getClass.getName}>")
    println(s"$rows rows; $cols columns")

    val t =
      new TextTable() {
        header("#", "Column", "Non-Null Count", "Datatype")

        for (((n, t), i) <- columnNames zip columnTypes zipWithIndex)
          this.row(i, n, columnNonNullCount(i), t.name)

        rightAlignment(1)
        rightAlignment(3)
      }

    println(t)

  def describe: Dataset =
    val fs = Seq(Sample.count, Sample.mean, Sample.std, Sample.min, Sample.q1, Sample.q2, Sample.q3, Sample.max)
    val cs = numericalColumnIndices

    if cs.isEmpty then Dataset(Seq("EMPTY"), Nil) // todo: pandas.describe() when there are no numeric columns
    else
      val data = fs map (f => cs map (c => columnSample(c, f)))
      val ds =
        Dataset(cs map columnNames, data, indices = Seq("count", "mean", "std", "min", "25%", "50%", "75%", "max"))

      ds

  def rowSlice(from: Int, until: Int): Dataset = dataset(dataArray.slice(from, until))

  protected def removeElement[T](idx: Int, vec: Vector[T]): Vector[T] =
    val (left, right) = vec.splitAt(idx)

    left ++ right.drop(1)

  def dropColumn(cidx: Int): Dataset =
    columnIndexCheck(cidx)

    new Dataset(
      columnNameMap.removed(columnNames(cidx)).view.mapValues(v => if v >= cidx then v - 1 else v).toMap,
      removeElement(cidx, columnNames),
      dataArray map (r => removeElement(cidx + 1, r)),
      removeElement(cidx, columnTypes),
    )

  def drop(cnames: String*): Dataset =
    cnames foreach columnNameCheck

    @tailrec
    def drop(cnames: List[String], ds: Dataset): Dataset =
      cnames match
        case Nil    => ds
        case h :: t => drop(t, ds.dropColumn(ds.columnNameMap(h)))

    drop(cnames.toList, this)

  protected def insertElement[T](idx: Int, elems: Vector[T], vec: Vector[T]): Vector[T] =
    val (left, right) = vec.splitAt(idx)

    left ++ elems ++ right

  def insert(cidx: Int, ds: Dataset): Dataset =
    columnIndexCheck(cidx, true)

    if columnSet.intersect(ds.columnSet).nonEmpty then sys.error("insert: duplicate column name")

    new Dataset(
      (columnNameMap.view.mapValues(v =>
        if v >= cidx then v + ds.cols
        else v,
      ) ++ ds.columnNameMap.view.mapValues(v => v + cidx)).toMap,
      (columnNames take cidx) ++ ds.columnNames ++ (columnNames drop cidx),
      dataArray zip ds.dataArray map { case (thisr, thatr) =>
        (thisr take cidx + 1) ++ thatr.tail ++ (thisr drop cidx + 1)
      },
      (columnTypes take cidx) ++ ds.columnTypes ++ (columnTypes drop cidx),
    )

  def append(ds: Dataset): Dataset = insert(cols, ds)

  def rename(cname: String): Dataset =
    require(cols == 1, "dataset can only have one column")

    new Dataset(Map(cname -> 0), Vector(cname), dataArray, columnTypes)

  def iloc(index: Int): Dataset = iloc(Vector(index))

  def iloc(indices: collection.Seq[Int]): Dataset = dataset(indices.toVector map dataArray)

  def loc(index: Any): Dataset = loc(Vector(index))

  def loc(indices: collection.Seq[Any]): Dataset =
    val buf = new ArrayBuffer[Vector[Any]]

    for (i <- indices)
      dataArray find (_.head == i) match
        case None    => sys.error(s"index label '$i' not found")
        case Some(r) => buf += r

    dataset(buf.toVector)

  def valuesInt(cname: String): Seq[Long] =
    columnNonNull(cname).distinct.asInstanceOf[Seq[Long]].sorted

  def valuesTimestamp(cname: String): Seq[Instant] =
    columnNonNull(cname).distinct.asInstanceOf[Seq[Instant]].sorted

  def valuesFloat(cname: String): Seq[Double] =
    columnNonNull(cname).distinct.asInstanceOf[Seq[Double]].sorted

  def valuesString(cname: String): Seq[String] =
    columnNonNull(cname).distinct.asInstanceOf[Seq[String]].sorted

  def counts(cname: String): Map[Any, Int] =
    (columnNonNull(cname) groupBy identity).view.mapValues(_.length).toMap.withDefaultValue(0)

  def countsNormalize(cname: String): Map[Any, Double] =
    columnNameCheck(cname)

    val rowCount = columnNonNullCount(columnNameMap(cname))

    counts(cname).view.mapValues(c => if rowCount == 0 then 0 else c.toDouble / rowCount).toMap.withDefaultValue(0)

  def sort(cname: String): Dataset =
    columnNameCheck(cname)

    val col = columnNameMap(cname)

    columnTypes(col) match
      case IntType   => dataset(dataArray.sortBy(_(col + 1).asInstanceOf[Long]))
      case FloatType => dataset(dataArray.sortBy(_(col + 1).asInstanceOf[Double]))

  def sampleWithReplacement(n: Int = rows): Dataset = iloc(Vector.fill(n)(Random.nextInt(rows)))

  def sample(n: Int): Dataset =
    require(n >= 0, "number of samples must be non-negative")

    val indicesSet = new mutable.HashSet[Int]
    val count = n min rows

    while indicesSet.size < count do indicesSet += Random.nextInt(rows)

    iloc(indicesSet.toVector)

  def shuffle: Dataset = dataset(Random.shuffle(dataArray))

  def split(splits: Int*): Vector[Dataset] =
    require(splits.nonEmpty, "need at least one split")
    require(splits.sum == 100, s"splits should sum to 100; got ${splits.sum}")

    var shuffled = Random.shuffle(dataArray)
    var remaining = dataArray.length
    val sections =
      for i <- 0 to splits.length - 2 yield
        val len = (splits(i) / 100.0 * dataArray.length).toInt
        val (section, rest) = shuffled.splitAt(len)

        shuffled = rest
        remaining -= len
        dataset(section)

    sections.toVector :+ dataset(shuffled)

  def shape: (Int, Int) = (rows, cols)

  def row(ridx: Int): Map[String, Any] =
    rowIndexCheck(ridx)
    columnNames zip dataArray(ridx).tail toMap

  protected def columnNameCheck(cname: String): Unit =
    require(columnNameMap contains cname, s"column name '$cname' not found")

  def apply(cname: String): Dataset =
    columnNameCheck(cname)

    val cidx = columnNameMap(cname)

    new Dataset(
      Map(cname -> 0),
      Vector(cname),
      dataArray map (r => Vector(r.head, r(cidx + 1))),
      Vector(columnTypes(cidx)),
    )

  protected def dataset(data: Vector[Vector[Any]]): Dataset =
    new Dataset(
      columnNameMap,
      columnNames,
      data,
      columnTypes,
    )

  protected def columnIndexCheck(cidx: Int, includeAfterEnd: Boolean = false): Unit =
    require(
      0 <= cidx && (includeAfterEnd && cidx <= cols || !includeAfterEnd && cidx < cols),
      s"column index ranges from 0 to number of columns${if includeAfterEnd then "" else " - 1"}",
    )

  protected def rowIndexCheck(ridx: Int, includeAfterEnd: Boolean = false): Unit =
    require(
      0 <= ridx && (includeAfterEnd && ridx <= rows || !includeAfterEnd && ridx < rows),
      s"row index ranges from 0 to number of rows${if includeAfterEnd then "" else " - 1"}",
    )

  protected def rowsCheck(length: Int): Unit = require(length == rows, "number of rows don't match")

  def apply(ds: Dataset): Dataset =
    require(
      ds.rows == rows && ds.cols == 1 && ds.columnTypes.head == BoolType,
      "dataset should have the same number of rows and one boolean typed column",
    )
    apply(ds map (_.head.asInstanceOf[Boolean]))

  def apply(s: Seq[Boolean]): Dataset =
    rowsCheck(s.length)
    dataset(dataArray zip s flatMap { case (d, s) => if s then List(d) else Nil })

  def apply(idx: Int): Vector[Any] = dataArray(idx).tail

  def length: Int = rows

  def selectDynamic(cname: String): Dataset = apply(cname)

  def columnNonNull(cname: String): ArraySeq[Any] =
    columnNameCheck(cname)
    columnNonNull(columnNameMap(cname))

  def columnNonNull(cidx: Int): ArraySeq[Any] =
    columnIndexCheck(cidx)
    dataArray.iterator filter (_(cidx + 1) != null) map (_(cidx + 1)) to ArraySeq

  def columnNonNullCount(cidx: Int): Int =
    columnIndexCheck(cidx)
    dataArray count (_(cidx + 1) != null)

  override def iterator: Iterator[Vector[Any]] = dataArray.iterator map (_ drop 1)

  def index(indices: Seq[Any]): Dataset =
    require(indices.length == rows, "sequence of indices should be the same length as the number of rows")
    new Dataset(
      columnNameMap,
      columnNames,
      dataArray zip indices map { case (r, i) => i +: r.tail },
      columnTypes,
    )

  def toArray: ArraySeq[ArraySeq[Any]] = iterator map (_ to ArraySeq) to ArraySeq

  override def toString: String = table(0, rows)

  protected def residualSumOfSquares(predictions: String, target: String): Double =
    (for i <- 0 until rows yield
      val row = iloc(i)
      val prediction = row(predictions).head.head.asInstanceOf[Number].doubleValue
      val diff = prediction - row(target).head.head.asInstanceOf[Number].doubleValue

      diff * diff
    ).sum

  protected def totalSumOfSquares(target: String): Double =
    val mean = Sample.mean(columnNonNull(target))

    (for i <- 0 until rows yield
      val row = iloc(i)
      val diff = mean - row(target).head.head.asInstanceOf[Number].doubleValue

      diff * diff
    ).sum

  def error(predictions: String, target: String): Double = sqrt(residualSumOfSquares(predictions, target) / rows)

  def r2(predictions: String, target: String): Double =
    1 - residualSumOfSquares(predictions, target) / totalSumOfSquares(target)

  def adjustedR2(predictions: String, target: String): Double =
    1 - (1 - r2(predictions, target) * (rows - 1) / (rows - (cols - 2)))
end Dataset

object Dataset:

  def apply(data: collection.Seq[collection.Seq[Any]]): Dataset =
    Dataset(if data.isEmpty then Nil else 1 to data.head.length map (_.toString), data)

  def apply(
      columns: collection.Seq[String],
      data: collection.Seq[collection.Seq[Any]],
      types: collection.Seq[Type] = Seq(InferType),
      indices: collection.Seq[Any] = Nil,
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
                case (TimestampType, TimestampType)       => TimestampType
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
                if tempType == InferType then null
                else tempType.convert(tempValues(r), true) getOrElse convertError(tempValues(r), tempType.name, r, c)

      for (r <- dataArray.indices)
        dataArray(r)(c) = tempValues(r)
    }

    new Dataset(
      columnNameMap,
      columnNameArray,
      dataArray zip rowIndexArray map { case (r, i) => (i +: r).toVector } toVector,
      columnTypeArray.toVector,
    )

  def apply(cname: String, data: collection.Seq[Any]): Dataset = Dataset(Seq(cname), data map (Seq(_)))

  def apply(m: collection.Map[String, Seq[Any]]): Dataset =
    require(m.nonEmpty, "map is empty")
    require(m.values.map(_.length).toSet.size == 1, "all sequences must have the same length")

    val entries = m.toList

    Dataset(entries map (_._1), entries map (_._2) transpose)

  def fromJSONString(s: String): Dataset = Dataset(
    DefaultJSONReader.fromString(s).asInstanceOf[collection.Map[String, Seq[Any]]],
  )

  def fromJSONFile(file: String): Dataset = Dataset(
    DefaultJSONReader.fromFile(file).asInstanceOf[collection.Map[String, Seq[Any]]],
  )

  def fromTabString(table: String, s: String): Dataset =
    Importer.importFromString(s, doubleSpaces = true).tables find (_.name == table) match
      case None                         => sys.error(s"table '$table' not found")
      case Some(Table(_, header, data)) => Dataset(header map (_.name), data)

  def fromTabFile(table: String, file: String): Dataset =
    Importer.importFromFile(file, doubleSpaces = true).tables find (_.name == table) match
      case None                         => sys.error(s"table '$table' not found")
      case Some(Table(_, header, data)) => Dataset(header map (_.name), data)

  def fromCSVString(s: String, columns: Seq[String] = null): Dataset =
    val csv = CSVRead.fromString(s).get
    val (header, data) =
      if (columns eq null) (csv.head, csv drop 1)
      else (columns, csv)

    Dataset(header, data)

  def fromCSVFile(file: String, columns: Seq[String] = null): Dataset =
    val csv = CSVRead.fromFile(file).get
    val (header, data) =
      if (columns eq null) (csv.head, csv drop 1)
      else (columns, csv)

    Dataset(header, data)

enum Axis:
  case INDEX, COLUMN
