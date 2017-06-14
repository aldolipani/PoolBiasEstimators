package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model._
import at.ac.tuwien.ifs.utils.Exporter

/**
  * Created by aldo on 17/10/16.
  */

class WebberOnRunsEstimatorV4(pool: Pool, metric: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) with Exporter {

  def isMetricSupported(metric: String) =
    metric.startsWith("recall_")

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("recall"))
      getScoreRecall(ru)
    else
      null
  }

  def getScoreRecall(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("recall_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val sru = M(n, ru, pool.qRels)
    val a = getAdjRecall(n, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  def getAdjRecall(n: Int, ru: Runs, pool: Pool): Double = {
    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("recall_" + n, ru, qRels)

    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    val d = System.getProperty("pool.depth").toInt

    val kru = 1d -
      TRECEval().computeMetric("P_" + d, ru, pool.qRels) -
      TRECEval().computeAntiMetric("P_" + d, ru, pool.qRels)

    val as = pool.lRuns.par.map(nRun => {
      val nRp =
        if (l1xo == L1xo.organization)
          filterOrganization(nRun, pool.lRuns, olRuns)
        else
          filterRun(nRun, pool.lRuns)
      val nQRels = pool.getNewInstance(nRp).qRels
      val δr = M(nRun) - M(nRun, nQRels)

      val krs = 1d -
        TRECEval().computeMetric("P_" + d, nRun, nQRels) -
        TRECEval().computeAntiMetric("P_" + d, nRun, nQRels)

      (δr*nQRels.sizeRel, krs)
    }).filter(_._2 > 0).seq

    kru * avg(as.map(e => e._1/e._2)) / pool.qRels.sizeRel
  }

  override def getName =
    if (l1xo == L1xo.organization)
      "WebberOnRunsV4L1OO"
    else
      "WebberOnRunsV4"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV4(pool, metric, descs, l1xo)

}


object WebberOnRunsEstimatorV4 {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo: L1xo = L1xo.run) = new WebberOnRunsEstimatorV4(pool, metric, descs, l1xo)

}