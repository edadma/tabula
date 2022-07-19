package com.vinctus.scandas

object Main extends App:

  val ds = new Dataset(
    Seq("column 1", "column 2"),
    for (r <- 1 to 2) yield for (c <- 1 to 2) yield if r == 1 && c == 1 then null else s"$r$c",
  )

  ds.info()
  println(ds)
  println(ds.dataArray.flatten map (a => if a == null then "<null>" else a.getClass))
