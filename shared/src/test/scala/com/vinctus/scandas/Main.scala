package com.vinctus.scandas

object Main extends App:

  val ds = new Dataset(
    Seq("column 1", "column 2"),
    for (r <- 1 to 2) yield for (c <- 1 to 2) yield if c == 1 then s"s$r.$c" else s"$r$c",
  )

  ds.info()
  println(ds)
  println(ds.dataArray.flatten map (_.getClass))
