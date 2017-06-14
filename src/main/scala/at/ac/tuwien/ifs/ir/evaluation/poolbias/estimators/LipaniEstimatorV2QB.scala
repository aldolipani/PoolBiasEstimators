package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 15/07/16.
  */

class LipaniEstimatorV2QB(pool:Pool, metric: String, descs: Descs = null) extends LipaniEstimatorV2(pool, metric, descs) {

  override def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getName = "LipaniV2QB"

  override def getNewInstance(pool:Pool) = new LipaniEstimatorV2QB(pool, metric, descs)

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreP _)
      new Score(ru.id, TRECEval().round(
        avg(scoresPerQuery.map(_._2.score))),
        metric,
        pool.qRels)
    }else if (metric.startsWith("recall_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreR _)
      new Score(ru.id, TRECEval().round(
        avg(scoresPerQuery.map(_._2.score))),
        metric,
        pool.qRels)
    }else
      null
  }

  def getScoreR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n:Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt

    val srun = M(n, ru)
    val an = getAdjP(n, ru, pool)
    val ad = getAdjP(d, ru, pool)
    val R = pool.qRels.sizeRel
    val srua = (srun * n + an * n)/(R + an * n + ad * (d - n))
    new Score(ru.id, srua, metric, pool.qRels)
  }

}


object LipaniEstimatorV2QB {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new LipaniEstimatorV2QB(pool, metric, descs)

}