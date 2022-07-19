package com.vinctus.scandas

object Main extends App:

//  val ds = new Dataset(
//    Seq("column 1", "column 2"),
//    for (r <- 1 to 2) yield for (c <- 1 to 2) yield if r == c then s"$r.$c" else s"$r$c",
//  )

//  val ds = Dataset.fromCSV("iris.csv")

  val ds = Dataset(
    Seq("Value"),
    Seq(2, 4, 6, 8, 13, 16, 22, 35, 40, 42, 48).map(Seq(_)),
  )

  ds.info()
  ds.describe.print()

//  ds.print()
//  println(ds.dataArray.flatten map (a => if a == null then "<null>" else a.getClass))
