package com.vinctus.scandas

import io.github.edadma.csv.CSVRead
import io.github.edadma.matrix.Matrix
import io.github.edadma.table.TextTable

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class Dataset(
    columns: collection.Seq[String],
    data: collection.Seq[collection.Seq[Any]],
) /*extends (Int => Dataset)*/ {
  private val columnNameArray = new ArrayBuffer[String](columns.length)
  private val columnNameMap = new mutable.HashMap[String, Int]
  private val columnTypeArray = ArrayBuffer.from[Datatype](columns)
  private val dataArray = data map (_ to ArrayBuffer) to ArrayBuffer

  require(columns.nonEmpty, "require at least one column")
//  require(data.cols == width, "require number of data columns equal number of column names")

  val width: Int = columns.length

//  def apply(ridx: Int): Dataset = new Dataset(columns, columnMap, data.row(ridx))

  def col(name: String): Matrix[Double] = data.col(columnMap(name))

  def mean(cidx: Int): Double = {
    val c = data.col(cidx)

    c.sum / c.length
  }

  def rows: Int = data.rows

  def cols: Int = data.cols

  def rowIterator: Iterator[Matrix[Double]] = (1 to rows).iterator map data.row

//  def datum(ridx: Int): (MVector, Double) = (data row ridx removeColView data.cols prepend ONE, data.row(ridx).last)

  def transform(elem: (Int, Int) => Double) = new Dataset(columns, columnMap, data.build(elem))

  override def toString: String =
    new TextTable() {
      headerSeq(columns)

      for (i <- 1 to data.rows)
        rowSeq(data.row(i))

      1 to data.cols foreach rightAlignment
    }.toString

}

object Dataset {

  def apply(columns: collection.Seq[String], data: Matrix[Double]): Dataset =
    new Dataset(columns, data)

  def apply(columns: collection.Seq[String], data: Seq[Seq[Any]]): Dataset =
    new Dataset(columns, Matrix.fromArray(data map (_ map (_.asInstanceOf[Number].doubleValue) toArray) toArray))

  def fromCSV(file: String, columns: collection.Seq[String] = null): Dataset = {
    val csv = CSVRead.fromFile(file).get
    val (header, data) =
      if (columns eq null) (csv.head, csv drop 1)
      else (columns, csv)

    Dataset(header, data map (_ map (_.toDouble)))
  }

}
