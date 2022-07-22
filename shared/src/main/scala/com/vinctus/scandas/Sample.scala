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

  def zcode(s: Seq[Double]): Seq[Double] =
    val mean = Sample.mean(s)
    val std = Sample.std(s)

    s map (x => (x - mean) / std)

  // https://statisticsbyjim.com/basics/percentiles/
  def percentile(s: Seq[Double], percent: Int): Double =
    val data = s.sorted.toIndexedSeq

    if data.isEmpty then Double.NaN
    else if data.length < 3 then data.head
    else
      val p = percent / 100d
      val rank = p * (data.length + 1)
      val upperIndex = rank.toInt
      val lowerIndex = upperIndex - 1
      val lower = data(lowerIndex)

      if rank.isWhole then lower
      else (data(upperIndex) - lower) * (rank - rank.toInt) + lower

  def q1(s: Seq[Double]): Double = percentile(s, 25)

  def q2(s: Seq[Double]): Double = percentile(s, 50)

  def q3(s: Seq[Double]): Double = percentile(s, 75)
