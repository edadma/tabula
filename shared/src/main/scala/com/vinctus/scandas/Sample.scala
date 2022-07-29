package com.vinctus.scandas

import math._
import scala.language.postfixOps

object Sample:
  def std(s: Seq[Any]): Double = sqrt(
    s2(s),
  ) // todo: different ways of calculating standard deviation

  def sem(s: Seq[Any]): Double = std(s) / sqrt(s.length)

  def s2(s: Seq[Any]): Double =
    require(s.nonEmpty, "can't compute variance of an empty list")

    if s.lengthCompare(1) == 0 then 0
    else
      val n = numerical(s)
      val m = mean(n)

      (n map (a => (a - m) * (a - m)) sum) / (count(n) - 1)

  def numerical(s: Seq[Any]): Seq[Double] = s map (_.asInstanceOf[Number].doubleValue)

  def mean(s: Seq[Any]): Double = numerical(s).sum / count(s)

  def count(s: Seq[Any]): Double = s.length

  def min(s: Seq[Any]): Double = numerical(s).min

  def max(s: Seq[Any]): Double = numerical(s).max

  def zcode(s: Seq[Any]): Seq[Double] =
    val mean = Sample.mean(s)
    val std = Sample.std(s)

    numerical(s) map (x => (x - mean) / std)

  // https://statisticsbyjim.com/basics/percentiles/
  def percentile(s: Seq[Any], percent: Double): Double =
    val data = numerical(s).sorted.toIndexedSeq

    if data.isEmpty then Double.NaN
    else if data.length < 3 then data.head
    else
      val p = percent / 100
      val rank = p * (data.length + 1)
      val upperIndex = rank.toInt
      val lowerIndex = upperIndex - 1
      val lower = data(lowerIndex)

      if rank.isWhole then lower
      else (data(upperIndex) - lower) * (rank - rank.toInt) + lower

  def q1(s: Seq[Any]): Double = percentile(s, 25)

  def q2(s: Seq[Any]): Double = percentile(s, 50)

  def q3(s: Seq[Any]): Double = percentile(s, 75)
