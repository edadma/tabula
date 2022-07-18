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
    types: Seq[Datatype] = Seq(Infer),
) /*extends (Int => Dataset)*/:
  private val columnNameArray = ArrayBuffer from columns
  private val columnNameMap = new mutable.HashMap[String, Int]
  private val columnTypeArray = ArrayBuffer from types
  private val dataArray = data map (_ to ArrayBuffer) to ArrayBuffer

  require(columnNameArray.nonEmpty, "require at least one column")
  require(dataArray.head.length == cols, "require number of data columns equal number of column names")

  def col(name: String): Seq[Any] = col(columnNameMap(name))

  def col(cidx: Int): Seq[Any] = data map (_(cidx)) to ArraySeq

  def mean(cidx: Int): Double = {
    val c = col(cidx).asInstanceOf[Seq[Double]]

    c.sum / c.length
  }

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

        for (((n, t), i) <- columnNameArray zip columnTypeArray zipWithIndex)
          row(i, n, "-", t)

        rightAlignment(1)
        rightAlignment(3)
      },
    )

  override def toString: String = head

end Dataset

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

end Dataset

// todo: columns: Seq[String] | Int // so that you can specify starting column number
