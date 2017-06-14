package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.model._

class LipaniEstimatorV2(pool: Pool, metric: String, descs: Descs = null) extends ScoreEstimator(pool, metric, descs) {

  override def isMetricSupported(metric: String): Boolean =
    metric.startsWith("P_")

  implicit def shufflableRuns(runs: Runs) = new {
    def ◦(sRuns: Runs, N: Int = 0): Runs = {
      getNewRunBySelectedRuns(runs, sRuns, N)
    }
  }

  protected def getAdjP(n: Int, ru: Runs, pool: Pool): Double = {
    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_" + n, ru, qRels)

    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1d - (sru + asru)
    // optimization: if kru is 0 no matters what the correction is going to be 0
    if (kru == 0d) return 0d

    val δks = pool.lRuns.par.map(rp => {
      val nrp = rp ◦ ru
      val δsrp = M(nrp) - M(rp)
      val δasrp = AM(nrp) - AM(rp)
      val δkrp = -δsrp - δasrp
      δkrp
    }).seq

    kru * Math.max(avg(δks), 0d)
  }

  protected def getScoreP(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n:Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

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

  protected def getNewRunBySelectedRuns(runs: Runs, sRuns: Runs, N: Int = 0): Runs = {
    new Runs(runs.id + "_" + sRuns.id + ".N_" + N,
      (for (id <- sRuns.topicIds.toList) yield {
        val run = runs.selectByTopicId(id)
        if (run != null) {
          val sRun = sRuns.selectByTopicId(id)
          new Run(id,
            Run.normalizeRank(
              run.runRecords.map(rR => {
                new RunRecord(
                  rR.iteration,
                  rR.document,
                  0,
                  getNewScore(sRun, rR, N))
              })))
        } else
          null
      }).filter(_ != null))
  }

  protected def getNewScore(sRun: Run, runRecord: RunRecord, N: Int): Float = (sRun.runRecords.size + 1) - {
    val alpha = 1d
    val sRunRecord = sRun.getByDocument(runRecord.document)

    if (sRunRecord != null && sRunRecord.rank > N) {
      val step = sRunRecord.rank - runRecord.rank
      val fix =
        if (step == 0)
          1d / 10000d
        else if (step > 0)
          (runRecord.rank.toDouble + 1d) / 10000d
        else
          runRecord.rank.toDouble / (10000d * 10000d)
      sRunRecord.rank.toDouble * alpha + runRecord.rank.toDouble * (1d - alpha) + fix
    } else runRecord.rank.toDouble + 1d / 10000d
  }.toFloat

  override def getName = "LipaniV2"

  override def getNewInstance(pool: Pool) = new LipaniEstimatorV2(pool, metric, descs)

}

object LipaniEstimatorV2 {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new LipaniEstimatorV2(pool, metric, descs)

}