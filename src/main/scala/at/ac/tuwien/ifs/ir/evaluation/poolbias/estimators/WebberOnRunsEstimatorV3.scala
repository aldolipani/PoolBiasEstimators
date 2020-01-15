package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model._
import at.ac.tuwien.ifs.utils.Exporter
import org.apache.commons.math3.stat.correlation.KendallsCorrelation

/**
  * Created by aldo on 17/10/16.
  */

class WebberOnRunsEstimatorV3(pool: Pool, measure: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends ScoreEstimator(pool, measure, descs) with Exporter {

  def isMetricSupported(metric: String): Boolean =
    metric.startsWith("recall_")

  override def getScore(ru: Runs): Score = {
    if (measure.startsWith("recall"))
      getScoreRecall(ru)
    else
      null
  }

  def getScoreRecall(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().recall_n(n, ru, qRels)

    val n = measure.split("_").last.toInt
    val sru = M(n, ru, pool.qRels)
    val a = getAdjRecall(n, ru, pool)
    new Score(ru.id, sru + a, measure, pool.qRels)
  }

  def getAdjRecall(n: Int, ru: Runs, pool: Pool): Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().recall_n(n, ru, qRels)

    val qs = pool.qRels.qRels.map(qRel => {
      val qRuns = pool.lRuns.map(run => run.getRunsTopic(qRel.id))

      val as = qRuns.par.map(runs => {
        val nRp =
          if (l1xo == L1xo.organization)
            filterOrganization(runs, qRuns, olRuns)
          else
            filterRun(runs, qRuns)

        val oQRels = pool.getNewInstance(qRuns).qRels
        if (oQRels.sizeRel == 0)
          0.0
        else {
          val nQRels = pool.getNewInstance(nRp).qRels
          val δr = M(runs, oQRels) - M(runs, nQRels)
          δr * nQRels.sizeRel / oQRels.sizeRel
        }
      }).seq

      //val ma = as.min
      //val res = gavg(as.filter(a => a != ma).map(a => a - ma)) + ma
      //println(qRel.id + " " + res)
      avg(as)
    })

    avg(qs)
  }

  override def getName: String =
    if (l1xo == L1xo.organization)
      "WebberOnRunsV3L1OO"
    else
      "WebberOnRunsV3"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV3(pool, measure, descs, l1xo)

}

object WebberOnRunsEstimatorV3 {

  def apply(pool: Pool, measure: String, descs: Descs, l1xo: L1xo = L1xo.run) =
    new WebberOnRunsEstimatorV3(pool, measure, descs, l1xo)

}