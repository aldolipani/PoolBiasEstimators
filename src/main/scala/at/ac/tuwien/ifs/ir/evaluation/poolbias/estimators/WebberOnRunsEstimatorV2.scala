package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.{SimpleStats, StatsEval, TRECEval}
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 02/05/15.
  */

class WebberOnRunsEstimatorV2(pool: Pool, metric: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) {

  def isMetricSupported(metric: String) =
    metric.startsWith("P_")

  def getScoreP(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val sru = M(n, ru, pool.qRels)
    val a = getAdjP(n, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P"))
      getScoreP(ru)
    else
      null
  }

  def getAdjP(n: Int, ru: Runs, pool: Pool): Double = {

    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_" + n, ru, qRels)

    def avg(vs: Seq[Double]): Double =
      if (vs.nonEmpty)
        vs.sum / vs.size
      else
        0d

    def logAvg(vs: Seq[Double]): Double =
      if (vs.nonEmpty)
        Math.log(avg(vs.map(e => Math.exp(e))))
      else
        0d

    val kru = 1d - M(ru) - AM(ru)

    val as = pool.lRuns.par.map(runs => {
      val nRp =
        if (l1xo == L1xo.organization)
          filterOrganization(runs, pool.lRuns, olRuns)
        else
          filterRun(runs, pool.lRuns)

      val nQRels = pool.getNewInstance(nRp).qRels
      val krp = 1d - M(runs, nQRels) - AM(runs, nQRels)
      val deltaP = M(runs) - M(runs, nQRels)
      (deltaP, krp)
    }).filter(e => e._1 > 0d).seq

    kru * logAvg(as.map(e => e._1 / e._2))
  }

  override def getName =
    if (l1xo == L1xo.organization)
      "WebberOnRunsV2L1OO"
    else
      "WebberOnRunsV2"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV2(pool, metric, descs, l1xo)

}

object WebberOnRunsEstimatorV2 {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo: L1xo = L1xo.run) = new WebberOnRunsEstimatorV2(pool, metric, descs, l1xo)

}