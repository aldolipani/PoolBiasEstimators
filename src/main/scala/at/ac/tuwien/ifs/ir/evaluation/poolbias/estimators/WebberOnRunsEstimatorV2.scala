package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.{SimpleStats, StatsEval, TRECEval}
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

class WebberOnRunsEstimatorV2(pool: Pool, metric: String, descs: Descs = null, l1xo: L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) {

  def isMetricSupported(metric: String): Boolean =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P"))
      getScoreP(ru)
    else
      getScoreRecall(ru)
  }

  def getScoreP(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val sru = M(n, ru, pool.qRels)
    val a = getAdjP(n, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  def getAdjP(n: Int, ru: Runs, pool: Pool): Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_" + n, ru, qRels)

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

    kru * gavg(as.map(e => e._1 / e._2))
  }

  def getScoreRecall(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("recall_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val sru = M(n, ru, pool.qRels)
    val a = getAdjRecall(n, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  /*def getAdjRecall(n: Int, ru: Runs, pool: Pool): Double = {
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
        val nQRels = pool.getNewInstance(nRp).qRels

        //println(qRel.id + " " + qRuns.length)
        M(runs, oQRels) - M(runs, nQRels)
      }).seq

      val ma = as.min
      gavg(as.filter(a => a != ma).map(a => a - ma)) + ma
      //avg(as)
    })

    avg(qs)
  }*/

  def med(xs:Seq[Double]):Double = {
    import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
    val stats = new DescriptiveStatistics
    xs.map(x => stats.addValue(x))
    stats.getPercentile(50)
  }

  def getAdjRecall(n: Int, ru: Runs, pool: Pool): Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("recall_" + n, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("recall_" + n, ru, qRels)

    val as = pool.lRuns.par.map(runs => {
      val nRp =
        if (l1xo == L1xo.organization)
          filterOrganization(runs, pool.lRuns, olRuns)
        else
          filterRun(runs, pool.lRuns)

      val nQRels = pool.getNewInstance(nRp).qRels
      M(runs) - M(runs, nQRels)
    }).seq

    med(as)
  }

  /*def getAdjRecall(n: Int, ru: Runs, pool: Pool): Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("recall_" + n, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("recall_" + n, ru, qRels)

    val as = pool.lRuns.par.map(runs => {
      val nRp =
        if (l1xo == L1xo.organization)
          filterOrganization(runs, pool.lRuns, olRuns)
        else
          filterRun(runs, pool.lRuns)

      val nQRels = pool.getNewInstance(nRp).qRels
      M(runs) - M(runs, nQRels)
    }).seq

    val ma = as.min
    gavg(as.filter(a => a != ma).map(a => a - ma)) + ma
  }*/

  override def getName: String =
    if (l1xo == L1xo.organization)
      "WebberOnRunsV2L1OO"
    else
      "WebberOnRunsV2"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV2(pool, metric, descs, l1xo)

}

object WebberOnRunsEstimatorV2 {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo: L1xo = L1xo.run) =
    new WebberOnRunsEstimatorV2(pool, metric, descs, l1xo)

}