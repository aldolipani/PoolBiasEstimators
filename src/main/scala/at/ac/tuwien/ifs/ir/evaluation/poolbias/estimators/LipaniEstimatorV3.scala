package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{DepthNPool, Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.model._

class LipaniEstimatorV3(pool: Pool, metric: String, descs: Descs = null) extends ScoreEstimator(pool, metric, descs) {

  override def isMetricSupported(metric: String): Boolean =
    metric.startsWith("P_") || metric.startsWith("recall_")

  implicit def shufflableRuns(runs: Runs) = new {
    def ◦(sRuns: Runs, N: Int = 0): Runs = {
      getNewRunBySelectedRuns(runs, sRuns, N)
    }
  }

  protected def getScoreP(ru: Runs): Score = {
    val n = metric.split("_").last.toInt

    def M(ru: Runs) =
      TRECEval().computeMetric("P_"+n, ru, pool.qRels)

    def AM(ru: Runs) =
      TRECEval().computeAntiMetric("P_"+n, ru, pool.qRels)

    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1 - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
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
        0d

    new Score(ru.id, sru + a)
  }

  protected def getScoreP(ru: Runs, R:Int): Score = {
    val n = metric.split("_").last.toInt

    def M(ru: Runs) =
      TRECEval().computeMetric("P_"+n, ru, pool.qRels)

    def AM(ru: Runs) =
      TRECEval().computeAntiMetric("P_"+n, ru, pool.qRels)

    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1 - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
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
        0d

    new Score(ru.id, (sru + a)*n/R)
  }

  protected def getScoreRPool(ru: Runs, pool:Pool): Score = {
    /*val d = new PoolAnalyzer(pool).d
    val ku =
      (1d -
        TRECEval().computeMetric("P_" + d, ru, pool.qRels) -
        TRECEval().computeAntiMetric("P_" + d, ru, pool.qRels))*d
    val C = pool.qRels.size + ku
    val sru = pool.qRels.sizeRel.toDouble/C
    val asru = (0d+
        pool.qRels.size -
        pool.qRels.sizeRel -
        ku
      )/C
    val kru = 1d - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
      val nrp = rp ◦ ru
      val nQRels = new DepthNPool(d, pool.lRuns.filter(_.id != rp.id).toList :+ nrp, pool.qRels).qRels
      val δsrp = nQRels.sizeRel.toDouble/C - pool.qRels.sizeRel.toDouble/C // repool
      val nkp = 1d - (TRECEval().computeMetric("P_" + d, nrp, nQRels)     + TRECEval().computeAntiMetric("P_" + d,  nrp, nQRels))*d
      val kp =  1d - (TRECEval().computeMetric("P_" + d, rp,  pool.qRels) + TRECEval().computeAntiMetric("P_" + d,  rp, pool.qRels))*d
      val δasrp =
        (nQRels.size - nQRels.sizeRel - nkp)/C -
        (pool.qRels.size - pool.qRels.sizeRel - kp)/C
      val δkrp = δsrp - δasrp
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
        0d

    def f(d:Double) = (if(d>=0) "+" else "") + "%1.5f" format d
    //println(ru.id + "\t" + f(sru) + "\t" + f(an) + "\t" + f(ad) + "\t" + f(nsru) + "\t" + R)
    println(ru.id + "\t" + f(sru) + "\t" + (asru) + "\t" + (kru) + "\t" + f(Math.max(avg(δks), 0d)) + "\t" + sru*C + "\t" + (sru + a)*C + "\t" + C + "\t" + pool.qRels.size + "\t" + 1d/((sru + a)*C) + "\t" + λ)
    */
    //new Score(ru.id, 1d/((sru + a)*C))
    new Score(ru.id, 1d/(pool.qRels.sizeRel))
  }

  def getScoreR(ru:Runs):Score ={
    val n = metric.split("_").last.toInt

    def getScoreP(ru: Runs):Double =
      TRECEval().computeMetric("P_"+n, ru, pool.qRels)

    new Score(ru.id,
      getScoreP(ru) * n *
        avg(pool.qRels.topicIds.map(tId =>
          1d/pool.qRels.qRels.filter(_.id == tId).head.sizeRel).toList))//getScorePerQuery(ru, getScoreRPool).score)
  }

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P"))
      getScoreP(ru)
    else if (metric.startsWith("recall"))
      getScoreR(ru)
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

  override def getName = "LipaniV3"

  override def getNewInstance(pool: Pool) = new LipaniEstimatorV3(pool, metric, descs)


}

//  def getScoreR(ru: Runs): Score = {
//    val n = metric.split("_").last.toInt
//    val d = new PoolAnalyzer(pool).d
//    val sru = M(ru)
//    val asru = getAntiRecall(ru, pool.qRels, d) //
//    val kru = getKRecall(ru, pool.qRels, n, d)
//    val vs = pool.lRuns.par.map(rp => {
//      val nrp = rp ◦ ru
//      val nQRels = new DepthNPool(d, pool.lRuns.filter(_.id != rp.id).toList :+ nrp, pool.qRels).qRels
//      val δsrp = TRECEval().computeMetric(metric, nrp, nQRels) - M(rp) // repool
//      val δasrp = getAntiRecall(nrp, nQRels, d) - getAntiRecall(rp, pool.qRels, d)
//      //val δkrp = -δsrp - δasrp
//      val δkrp = getKRecall(nrp, nQRels, n, d) - getKRecall(rp, pool.qRels, n, d)
//      //(TRECEval().computeMetric("P_"+n, nrp, nQRels) - TRECEval().computeMetric("P_"+n, rp, nQRels),
//      //  TRECEval().computeAntiMetric("P_"+n, nrp, nQRels) - TRECEval().computeAntiMetric("P_"+n, rp, nQRels),
//      //  δkrp)
//      (δsrp, δasrp, δkrp)
//    }).seq
//    val (δss, δass, δks) = (vs.map(_._1), vs.map(_._2), vs.map(_._3))
//    val Δsru = avg(δss)
//    val Δasru = avg(δass)
//    val λ = kru * (Δsru * asru - Δasru * sru)
//    val a =
//      if (λ > 0)
//        kru * Math.max(avg(δks), 0d) //*(-sru/asru)
//      else
//        0
//
//    println(ru.id + "\t" + sru + "\t" + a + "\t" + asru + "\t" + Δsru + "\t" + Δasru + "\t" + kru + "\t" + avg(δks) + "\t" + λ)
//    new Score(ru.id, sru + a)
//  }
//
//  /*
//  Pool:
//	MAE   	0.0026 ±0.0011
//	SRE   	21
//	SRE*  	0	p<0.05
//	KTauB 	0.9741
//
//WebberOnRuns:
//	MAE   	0.0025 ±0.0009
//	SRE   	19
//	SRE*  	0	p<0.05
//	KTauB 	0.9741
//
//LipaniV3:
//	MAE   	0.0040 ±0.0009
//	SRE   	38
//	SRE*  	0	p<0.05
//	KTauB 	0.9741
//   */
//  // Next if this work do it per query
//  // It it works equally, do the interpolation putting the delta k in the x --> see the whiteboard :)
//  def getScoreRP(ru: Runs, pool: Pool = this.pool): Score = {
//    val n = metric.split("_").last.toInt
//    val sru = M(ru)
//    val asru = getAntiRecall(ru, pool.qRels, n)
//    //val kru = 1 - sru - asru // it is always 0
//    val kru = getKRecall(ru, pool.qRels, n, n)
//    val vs = pool.lRuns.par.map(rp => {
//      val nrp = rp ◦ ru
//      val δsrp = TRECEval().computeMetric("P_" + n, nrp, pool.qRels) - TRECEval().computeMetric("P_" + n, rp, pool.qRels)
//      val δasrp = TRECEval().computeAntiMetric("P_" + n, nrp, pool.qRels) - TRECEval().computeAntiMetric("P_" + n, rp, pool.qRels)
//      val δkrp = -δsrp - δasrp
//      (δsrp, δasrp, δkrp)
//    }).seq
//    val (δss, δass, δks) = (vs.map(_._1), vs.map(_._2), vs.map(_._3))
//    val Δsru = avg(δss)
//    val Δasru = avg(δass)
//    val λ = kru * (Δsru * asru - Δasru * sru)
//    val a =
//      if (λ > 0)
//        kru * Math.max(avg(δks), 0d) //*(-sru/asru)
//      else
//        0
//    println(ru.id + "\t" + sru + "\t" + a + "\t" + asru + "\t" + Δsru + "\t" + Δasru + "\t" + kru + "\t" + avg(δks) + "\t" + λ)
//    new Score(ru.id, sru + a)
//  }
//
//  def getAntiRecall(ru: Runs, qRels: QRels, n: Int) = {
//    TRECEval().avg(qRels.qRels.filter(qrel => ru.selectByTopicId(qrel.id) != null).map(qrel =>
//      new QRel(qrel.id, {
//        val docId = ru.selectByTopicId(qrel.id).runRecords.take(n).map(_.document.id)
//        qrel.qrelRecord.filter(qr => !(qr.rel > 0 && docId.contains(qr.document.id)))
//      }).sizeRel.toDouble / qRels.topicQRels(qrel.id).sizeRel), qRels.sizeTopics)
//  }
//
//  def getKRecall(ru: Runs, qRels: QRels, n: Int, d: Int) = {
//    TRECEval().avg(qRels.qRels.filter(qrel => ru.selectByTopicId(qrel.id) != null).map(qrel => {
//      val docId = qrel.qrelRecord.map(_.document.id)
//      val k = ru.selectByTopicId(qrel.id).runRecords.take(d).filter(d => !docId.contains(d.document.id)).size.toDouble
//      val p = ru.selectByTopicId(qrel.id).runRecords.take(n).filter(d => !docId.contains(d.document.id)).size.toDouble
//      p / (qRels.topicQRels(qrel.id).sizeRel + k)
//    }), qRels.sizeTopics)
//  }
//
//  def getK(ru: Runs, qRels: QRels, n: Int, d: Int):(Double, Double) = {
//    val ab = qRels.qRels.filter(qrel => ru.selectByTopicId(qrel.id) != null).map(qrel => {
//      val docId = qrel.qrelRecord.map(_.document.id)
//      val k = ru.selectByTopicId(qrel.id).runRecords.take(d).filter(d => !docId.contains(d.document.id)).size.toDouble
//      val p = ru.selectByTopicId(qrel.id).runRecords.take(n).filter(d => !docId.contains(d.document.id)).size.toDouble
//      (k, p)
//    })
//    (TRECEval().avg(ab.map(_._1), qRels.sizeTopics), TRECEval().avg(ab.map(_._2), qRels.sizeTopics))
//  }
//
//  override def getScore(ru: Runs): Score = {
//    if (metric.startsWith("P_"))
//      getScoreP(ru)
//    else if (metric.startsWith("recall_"))
//      getScorePerQuery(ru, getScoreRO)
//    else
//      null
//  }
//
//  def getScoreRO(ru: Runs, pool: Pool = this.pool): Score = {
//    val n = metric.split("_").last.toInt
//    val d = new PoolAnalyzer(pool).d
//
//    val sru = M(ru, pool.qRels)
//    val R = pool.qRels.sizeRel
//    val asru = getAntiRecall(ru, pool.qRels, n)
//    val kru = getKRecall(ru, pool.qRels, n, n)
//    val k = getK(ru, pool.qRels, n, n)
//    val vs = pool.lRuns.par.map(rp => {
//      val nrp = rp ◦ ru
//      val nQRels = pool.getNewInstance(pool.lRuns.filter(_.id != rp.id) :+ nrp).qRels
//      val δsrp = TRECEval().computeMetric(metric, nrp, nQRels) - M(rp, pool.qRels)
//      val δasrp = getAntiRecall(nrp, nQRels, n) - getAntiRecall(rp, pool.qRels, n) //getAntiRecall(nrp, pool.qRels, n) - getAntiRecall(rp, pool.qRels, n)
//      val δkrp = getKRecall(nrp, nQRels, n, n) - getKRecall(rp, pool.qRels, n, n)
//      //val δkrp = -δsrp - δasrp
//      (δsrp, δasrp, δkrp)
//    }).seq
//    val (δss, δass, δks) = (vs.map(_._1), vs.map(_._2), vs.map(_._3))
//    val Δsru = avg(δss)
//    val Δasru = avg(δass)
//    val λ = kru * (Δsru * asru - Δasru * sru)
//    val a =
//      if (λ > 0) {
//        val Δkru = avg(δks)
//        sru + (k._1 * Δkru)/(R + k._2 * Δkru)
//        //(sru * R + kru * Math.max(Δkru, 0d)) /
//        //  (R + kru * Math.max(Δkru, 0d) * d) // * asru//*(-sru/asru)
//      } else
//        sru
//
//    def f(d: Double) = (if (d >= 0) "+" else "") + "%1.5f" format d
//    println(ru.id + "\t" + f(sru) + "\t" + f(a) + "\t" + f(asru) + "\t" + f(Δsru) + "\t" + f(Δasru) + "\t" + f(kru) + "\t" + f(avg(δks)) + "\t" + f(λ))
//
//    new Score(ru.id, a)
//  }
//
//  override def getName = "LipaniV3"
//
//  override def getNewInstance(pool: Pool) = new LipaniEstimatorV3(pool, metric, descs)
//
//}
//
object LipaniEstimatorV3 {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new LipaniEstimatorV3(pool, metric, descs)

}