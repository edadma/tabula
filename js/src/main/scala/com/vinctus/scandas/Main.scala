package com.vinctus.scandas

import java.time.{Duration, Instant}
import java.time.temporal.ChronoUnit._

@main def run(): Unit =
  val t1 = Instant.now
  val t2 = Instant.now.plus(5, SECONDS)

  println((t1, t2, Duration.between(t2, t1).toMillis))
