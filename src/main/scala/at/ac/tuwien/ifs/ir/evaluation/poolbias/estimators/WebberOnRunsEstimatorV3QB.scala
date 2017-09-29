package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 17/10/16.
  */

class WebberOnRunsEstimatorV3QB(pool: Pool, metric: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends WebberOnRunsEstimatorV3(pool, metric, descs, l1xo) {

  override def isMetricSupported(metric: String) =
    metric.startsWith("recall_")

  override def getName =
    if (l1xo == L1xo.run)
      "WebberOnRunsV3QB"
    else
      "WebberOnRunsV3QBL1OO"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV3QB(pool, metric, descs, l1xo)

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("recall_")) {
      new Score(ru.id, new TRECEval().round(
        avg(getScoresPerQuery(ru, getScoreRecall _).map(_._2.score))),
        metric, pool.qRels)
    } else
      null
  }

}

object WebberOnRunsEstimatorV3QB {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo: L1xo = L1xo.run) = new WebberOnRunsEstimatorV3QB(pool, metric, descs, l1xo)

}