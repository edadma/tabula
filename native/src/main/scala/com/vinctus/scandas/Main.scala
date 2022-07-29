package com.vinctus.scandas

@main def run(): Unit =
  val ds = Dataset.fromCSVString("""
      |time
      |2018-08-22T19:10:53.095Z
      |2018-08-22T19:10:53.094Z
      |2018-08-22T19:10:53.094Z
      |""".trim.stripMargin)

  println(ds.valuesTimestamp("time"))
  ds.info()
