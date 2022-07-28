package com.vinctus.scandas

import java.time.{Instant, LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}

@main def run(): Unit =
  println(
    ZonedDateTime
      .parse("2022-07-28T19:22:45-04:00")
      .toInstant, /*.atZone(ZoneId.of("America/Toronto"))*/
  )
