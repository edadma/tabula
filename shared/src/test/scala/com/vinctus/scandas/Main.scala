package com.vinctus.scandas

object Main extends App:

//  val ds = Dataset(
//    Seq("col1", "col2"),
//    for (r <- 1 to 3) yield for (c <- 1 to 2) yield if r == c then s"$r.$c" else s"$r$c",
//  )

//  val ds = Dataset.fromCSV("iris.csv")

//  val ds = Dataset(
//    Seq("Value"),
//    Seq(2, 4, 6, 8, 13, 16, 22, 35, 40, 42, 48).map(Seq(_)),
//  )

//  val ds = Dataset.fromString("""
//      |time,value
//      |2018-08-22T19:10:53.094,true
//      |""".trim.stripMargin)

//  val ds = Dataset.fromCSV("iris.csv")
//
//  ds.info()

  val ds = Dataset(
    Map(
      "col1" -> Seq(1, 15, 2, 2, 2, 3, 1, 1, 2, 2, 2, 3, 1, 1, 2),
//      "col2" -> Seq(1, 15, 2, 2, 2, 3, 1, 1, 2, 2, 2, 3, 1, 1, 2),
    ),
  )

  ds.describe.print()
//  println(ds((ds.zcode.abs < 3).all))

//  val ds = Dataset(Map("col1" -> Seq(3, 4), "col2" -> Seq(5, 6)))
//
//  println(ds((ds > 3).all))
