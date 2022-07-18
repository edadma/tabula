package com.vinctus.scandas

object Main extends App:

  val ds = new Dataset(Seq("col1", "col2"), for (r <- 1 to 15) yield for (c <- 1 to 2) yield s"$r.$c")

  ds.info()
