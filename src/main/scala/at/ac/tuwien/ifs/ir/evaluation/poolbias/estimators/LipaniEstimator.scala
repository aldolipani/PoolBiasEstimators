package at.ac.tuwien.ifs.poolbias.estimators

import at.ac.tuwien.ir.model._
;

class LipaniEstimator(qRels: QRels, Rp: List[Runs], metric: String, descs: Descs = null) extends ScoreEstimator(qRels, Rp, metric, descs) {

  implicit def shufflableRuns(runs: Runs) = new {
    def ◦(sRuns: Runs, N: Int = 0): Runs = getNewRunBySelectedRuns(runs, sRuns, N)
  }

  override def getScore(ru: Runs): Score = {
    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1 - (sru + asru)
    val vs = Rp.par.map(rp => {
      val nrp = rp ◦ ru
      val δsrp = M(nrp) - M(rp)
      val δasrp = AM(nrp) - AM(rp)
      val δkrp = -δsrp - δasrp
      (δsrp, δasrp, δkrp)
    }).seq
    val (δss, δass, δks) = (vs.map(_._1), vs.map(_._2), vs.map(_._3))
    val Δsru = avg(δss)
    val Δasru = avg(δass)
    val λ = kru * (Δsru * asru - Δasru * sru)
    val a =
      if (λ > 0)
        kru * Math.max(avg(δks), 0d)
      else
        0
    new Score(ru.id, sru + a)
  }

  private def getNewRunBySelectedRuns(runs: Runs, sRuns: Runs, N: Int = 0): Runs = {
    new Runs("s." + runs.id,
      for (id <- sRuns.topicIds.toList) yield {
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
      })
  }

  private def getNewScore(sRun: Run, runRecord: RunRecord, N: Int): Float = (sRun.runRecords.size + 1) - {
    val alpha = 1d
    val sRunRecord = sRun.getByDocumentId(runRecord.document.id)

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

  override def getName = "Lipani"

  override def getNewInstance(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) = new LipaniEstimator(qRels, lRuns, metric, descs)

}

object LipaniEstimator {

  def apply(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) = new LipaniEstimator(qRels, lRuns, metric, descs)

}