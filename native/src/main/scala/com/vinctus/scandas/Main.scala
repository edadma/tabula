package com.vinctus.scandas

@main def run(): Unit =
  val ds = Dataset.fromCSVString("""
      |time
      |2018-08-22T19:10:53.095
      |2018-08-22T19:10:53.094
      |2018-08-22T19:10:53.094
      |""".trim.stripMargin)

  println(ds.valuesInstant("time"))
  ds.info()
