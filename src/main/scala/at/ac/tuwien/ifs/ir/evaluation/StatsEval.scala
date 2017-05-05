package at.ac.tuwien.ifs.ir.evaluation

/**
  * Created by aldo on 15/07/16.
  */

class SimpleStats(val mean: Double, val median: Double, val sd: Double, val n: Int, t: (Double) => Double = (a: Double) => a)

object StatsEval {

  def mean(vs: Seq[Double]) = vs.sum / vs.size

  def median(vs: Seq[Double]) =
    if (vs.size <= 1) Double.NaN
    else {
      if (vs.size % 2 == 0) {
        val i = vs.size / 2
        vs(i - 1) / 2 + vs(i) / 2
      } else {
        val i = (vs.size - 1) / 2
        vs(i)
      }
    }

  def sd2(vs: Seq[Double]) =
    if (vs.size <= 1) Double.NaN
    else {
      val m = mean(vs)
      mean(vs.map(v => Math.pow(v - m, 2)))
    }

  def sd(vs: Seq[Double]) = Math.sqrt(sd2(vs))

  def simpleStats(vs: Seq[Double]) = new SimpleStats(mean(vs), median(vs), sd(vs), vs.size)

  def simpleStats(vs: Seq[Double], t: (Double) => Double) =
    new SimpleStats(t(mean(vs)), t(median(vs)), sd(vs), vs.size, t)
}
