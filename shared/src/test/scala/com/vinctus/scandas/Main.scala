package com.vinctus.scandas

object Main extends App:

  val ds = new Dataset(
    Seq("col1", "col2"),
    for (r <- 1 to 2) yield for (c <- 1 to 2) yield if c == r then s"$r.${c}0" else s"0$r$c",
  )

  ds.info()
  println(ds)
  println(ds.dataArray.flatten map (_.getClass))
