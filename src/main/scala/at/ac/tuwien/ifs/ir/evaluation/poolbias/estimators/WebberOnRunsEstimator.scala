package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
 * Created by aldo on 02/05/15.
 */

class WebberOnRunsEstimator(pool:Pool, metric: String, descs: Descs = null, l1xo:L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) {

  def isMetricSupported(metric:String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getScore(ru: Runs): Score = {
    getScoreM(ru, pool)
  }

  def getScoreM(ru: Runs, pool: Pool = this.pool): Score = {
    val sru = M(ru)
    val a = getAdj(metric, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  protected def getAdj(metric:String, ru: Runs, pool: Pool): Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric(metric, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric(metric, ru, qRels)

    val sru = M(ru)
    val as = pool.lRuns.par.map(nRun => {
      val nRp =
        if(l1xo == L1xo.organization)
          filterOrganization(nRun, pool.lRuns, olRuns)
        else
          filterRun(nRun, pool.lRuns)
      val nQRels = pool.getNewInstance(nRp).qRels
      M(nRun) - M(nRun, nQRels)
    }).seq

    avg(as)
  }

    override def getName =
    if(l1xo == L1xo.organization)
      "WebberOnRunsL1OO"
    else
      "WebberOnRuns"

  override def getNewInstance(pool:Pool) = new WebberOnRunsEstimator(pool, metric, descs, l1xo)

}

object WebberOnRunsEstimator {

  def apply(pool:Pool, metric: String, descs: Descs, l1xo:L1xo = L1xo.run) = new WebberOnRunsEstimator(pool, metric, descs, l1xo)

}