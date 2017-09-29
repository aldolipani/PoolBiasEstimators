package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 02/05/15.
  */

class WebberOnRunsEstimator(pool: Pool, metric: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) {

  def isMetricSupported(metric: String): Boolean = true

  override def getScore(ru: Runs): Score = {
    getScoreMeasure(ru, pool)
  }

  def getScoreMeasure(ru: Runs, pool: Pool = this.pool): Score = {
    val sru = M(ru)
    val a = getAdjM(metric, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  protected def getAdjM(metric: String, ru: Runs, pool: Pool): Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeRawMetric(metric, ru, qRels)

    val as = pool.lRuns.par.map(nRun => {
      val nRp =
        if (l1xo == L1xo.organization)
          filterOrganization(nRun, pool.lRuns, olRuns)
        else
          filterRun(nRun, pool.lRuns)

      val nQRels = pool.getNewInstance(nRp).qRels
      M(nRun) - M(nRun, nQRels)
    }).seq

    avg(as)
  }

  override def getName: String =
    if (l1xo == L1xo.organization)
      "WebberOnRunsL1OO"
    else
      "WebberOnRuns"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimator(pool, metric, descs, l1xo)

}

object WebberOnRunsEstimator {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo: L1xo = L1xo.run) =
    new WebberOnRunsEstimator(pool, metric, descs, l1xo)

}