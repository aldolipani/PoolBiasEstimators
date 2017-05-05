package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.evaluation.{StatsEval, TRECEval}
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 02/05/15.
  */

class WebberOnRunsEstimatorV2QB(pool: Pool, metric: String, descs: Descs = null, l1xo:L1xo = L1xo.run) extends WebberOnRunsEstimatorV2(pool, metric, descs, l1xo) {

  override def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getName =
    if(l1xo == L1xo.run)
      "WebberOnRunsV2QB"
    else
      "WebberOnRunsV2QBL1OO"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV2QB(pool, metric, descs)

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_"))
      new Score(ru.id, new TRECEval().round(
        avg(getScoresPerQuery(ru, getScoreP _).map(_._2.score))),
        metric, pool.qRels)
    else if (metric.startsWith("recall_")) {
      new Score(ru.id, new TRECEval().round(
        avg(getScoresPerQuery(ru, getScoreR _).map(_._2.score))),
        metric, pool.qRels)
    }else
      null
  }
/*
  //Bayesian
  def getScoreRB(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    def AM(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)
    val krun = 1 - M(n, ru) - AM(n, ru)
    val krud = 1 - M(d, ru) - AM(d, ru)

    val apn = getAdjP(n, ru, pool)
    val apd = getAdjP(d, ru, pool)

    val an = apn._1
    val R = pool.qRels.sizeRel

    val sapn = StatsEval.simpleStats(apn._2)
    val sapd = StatsEval.simpleStats(apd._2)

    val ad =
      if (sapn.n > 1){
        val v0 = Math.pow(sapn.sd, 2)/Math.pow(n, 2)
        val vd = Math.pow(sapd.sd, 2)/Math.pow(d, 2)
        // Bayesian
        krud *
          Math.exp(v0/(vd/sapd.n + v0) * sapd.mean + vd/(vd/sapd.n + v0) * sapn.mean*d/n)
      } else
        apd._1

    if(ad*d > an*n) println("ko", an*n, ad*d)

    val sruna = (srun * n + an * n) / (R + ad * (d - n))
    new Score(ru.id, sruna)
  }
*/
/*  def getScoreOriR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)
    val an = getAdjP(n, ru, pool)._1
    val ad = getAdjP(d, ru, pool)._1
    val R = pool.qRels.sizeRel
    val srua = (srun * n + an * n) / (R + an * n + ad * (d - n))
    new Score(ru.id, srua)
  }*/

  def getScoreMacroR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_" + n, ru, qRels)

    def AM(n: Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_" + n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)
    val R = pool.qRels.topicQRels(ru.id.split("@").last.toInt).sizeRel
    lazy val an = getAdjP(n, ru, pool)
    lazy val ad = getAdjP(d, ru, pool)
    val srua =
      (srun * n + an * n) / (R + ad * d)
    //println(ru.id, srua)
    new Score(ru.id, srua, metric, pool.qRels)
  }

  def getScoreR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n:Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)

    val as = pool.lRuns.map(rp => {
      val nPool = new Pool(List(rp), pool.qRels)
      getAdjP(n, ru, nPool)
    })
    val R = pool.qRels.sizeRel
    val srua = avg(as.map(an => (srun * n + an * n)/(R + an * n)))
    new Score(ru.id, srua, metric, pool.qRels)
  }

}

object WebberOnRunsEstimatorV2QB {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo:L1xo = L1xo.run) = new WebberOnRunsEstimatorV2QB(pool, metric, descs, l1xo)

}