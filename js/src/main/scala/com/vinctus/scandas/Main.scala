package com.vinctus.scandas

import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit._
import java.time.format.DateTimeFormatter

@main def run(): Unit =
  val ds = Dataset.fromCSVFile("trips3.csv")

  println(ds)
  ds.info()
