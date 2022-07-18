package com.vinctus.scandas

object Main extends App:

  val ds = new Dataset(Seq("col1", "col2"), Seq(Seq(1, 2), Seq(3, 4)))

  println(ds)
