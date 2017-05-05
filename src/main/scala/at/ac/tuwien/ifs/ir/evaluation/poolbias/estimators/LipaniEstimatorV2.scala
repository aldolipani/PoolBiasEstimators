package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{DepthNPool, Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.model._
;

class LipaniEstimatorV2(pool: Pool, metric: String, descs: Descs = null) extends LipaniEstimator(pool, metric, descs) {

  protected def getScoreP(ru: Runs): Score = {
    val d = new PoolAnalyzer(pool).d
    val n = metric.split("_").last.toDouble
    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1 - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
      val nrp = rp ◦ ru
      val δsrp = M(nrp) - M(rp)
      val δasrp = AM(nrp) - AM(rp)
      val δkrp = δsrp - δasrp
      (δsrp, δasrp, δkrp)
    }).seq
    val (δss, δass, δks) = (vs.map(_._1), vs.map(_._2), vs.map(_._3))
    val Δsru = avg(δss)
    val Δasru = avg(δass)
    val λ = kru * (Δsru * asru - Δasru * sru)
    val a =
      if (λ > 0)
        kru * Math.max(avg(δks), 0d)
      else
        0
    new Score(ru.id, sru + a)
  }

  override def getName = "LipaniV2"

  override def getNewInstance(pool: Pool) = new LipaniEstimatorV2(pool, metric, descs)

}

object LipaniEstimatorV2 {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new LipaniEstimatorV2(pool, metric, descs)

}