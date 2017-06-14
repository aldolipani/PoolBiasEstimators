package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 02/05/15.
  */

class WebberOnRunsEstimatorQB(pool: Pool, metric: String, descs: Descs = null, l1xo:L1xo = L1xo.run) extends WebberOnRunsEstimator(pool, metric, descs, l1xo) {

  override def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getName =
    if(l1xo == L1xo.run)
      "WebberOnRunsQB"
    else
      "WebberOnRunsQBL1OO"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorQB(pool, metric, descs)

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreM _)
      new Score(ru.id, new TRECEval().round(
        avg(scoresPerQuery.map(_._2.score))),
        metric,
        pool.qRels)
    }else if (metric.startsWith("recall_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreR _)
      new Score(ru.id, new TRECEval().round(
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
    val an = getAdj("P_" + n, ru, pool)
    val ad = getAdj("P_" + d, ru, pool)
    val R = pool.qRels.sizeRel
    val srua = (srun * n + an * n)/(R + an * n + ad * (d - n))
    new Score(ru.id, srua, metric, pool.qRels)
  }

}

object WebberOnRunsEstimatorQB {

  def apply(pool:Pool, metric: String, descs: Descs, l1xo:L1xo = L1xo.run) = new WebberOnRunsEstimatorQB(pool, metric, descs, l1xo)

}