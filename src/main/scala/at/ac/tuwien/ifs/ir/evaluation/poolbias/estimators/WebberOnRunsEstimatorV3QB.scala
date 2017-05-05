package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 17/10/16.
  */

class WebberOnRunsEstimatorV3QB(pool: Pool, metric: String, descs: Descs = null, l1xo:L1xo = L1xo.run) extends WebberOnRunsEstimatorV3(pool, metric, descs, l1xo) {

  override def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getName =
    if(l1xo == L1xo.run)
      "WebberOnRunsV3QB"
    else
      "WebberOnRunsV3QBL1OO"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV3QB(pool, metric, descs)


  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_"))
      new Score(ru.id, new TRECEval().round(
        avg(getScoresPerQuery(ru, getScoreP _).map(_._2.score))),
        metric, pool.qRels)
    else if (metric.startsWith("recall_")) {
      new Score(ru.id, new TRECEval().round(
        avg(getScoresPerQuery(ru, getScoreR _).map(_._2.score))),
        metric, pool.qRels)
    }else
      null
  }

  def getScoreR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    def AM(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)
    val R = pool.qRels.topicQRels(ru.id.split("@").last.toInt).sizeRel
    lazy val an = getAdjP(n, ru, pool)
    lazy val ad = getAdjP(d, ru, pool)
    val srua =
      (srun * n + an * n) / (R + an * n + ad * (d-n))
    //(srun * n + an * n) / (R + ad * d)

    //println(ru.id, srua, n, an, R, ad, d, srun)
    new Score(ru.id, srua, metric, pool.qRels)
  }
}

object WebberOnRunsEstimatorV3QB {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo:L1xo = L1xo.run) = new WebberOnRunsEstimatorV3QB(pool, metric, descs, l1xo)

}