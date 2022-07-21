package com.vinctus.scandas

import scala.language.postfixOps

object Sample:
  def std(s: Seq[Double]): Double =
    val m = mean(s)

    math.sqrt(
      (s map (a => (a - m) * (a - m)) sum) / (count(s) /*- 1*/ ),
    ) // todo: different ways of calculating standard deviation

  def mean(s: Seq[Double]): Double = s.sum / count(s)

  def count(s: Seq[Double]): Double = s.length

  def min(s: Seq[Double]): Double = s.min

  def max(s: Seq[Double]): Double = s.max
