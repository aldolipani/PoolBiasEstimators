package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.evaluation.{StatsEval, TRECEval}
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 02/05/15.
  */

class WebberOnRunsEstimatorV2QB(pool: Pool, metric: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends WebberOnRunsEstimatorV2(pool, metric, descs, l1xo) {

  override def isMetricSupported(metric: String): Boolean =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getName: String =
    if (l1xo == L1xo.run)
      "WebberOnRunsV2QB"
    else
      "WebberOnRunsV2QBL1OO"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV2QB(pool, metric, descs, l1xo)

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreP _)
      new Score(ru.id, new TRECEval().round(
        avg(scoresPerQuery.map(_._2.score))),
        metric,
        pool.qRels)
    } else if (metric.startsWith("recall_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreRecall _)
      new Score(ru.id, new TRECEval().round(
        avg(scoresPerQuery.map(_._2.score))),
        metric,
        pool.qRels)
    } else
      null
  }

  override def getScoreRecall(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt

    val R = pool.qRels.sizeRel
    val srua =
      if (R > 0) {
        val srun = M(n, ru)
        val an = getAdjP(n, ru, pool)
        val ad = getAdjP(d, ru, pool)
        (srun * n + an * n) / (R + an * n + ad * Math.max(d - n, 0))
      } else {
        0d
      }
    new Score(ru.id, srua, metric, pool.qRels)
  }


}

object WebberOnRunsEstimatorV2QB {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo: L1xo = L1xo.run) =
    new WebberOnRunsEstimatorV2QB(pool, metric, descs, l1xo)

}