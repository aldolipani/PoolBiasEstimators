package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 15/07/16.
  */

class LipaniEstimatorQB(pool:Pool, metric: String, descs: Descs = null) extends LipaniEstimator(pool, metric, descs) {

  override def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getName = "LipaniQB"

  override def getNewInstance(pool:Pool) = new LipaniEstimatorQB(pool, metric, descs)

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreP _)
      System.out.println(scoresPerQuery.head._2.qRels.topicIds.size + "********************* LipaniEstimator getScore")
      new Score(ru.id, new TRECEval().round(
        avg(scoresPerQuery.map(_._2.score))),
        scoresPerQuery.head._2.metric,
        scoresPerQuery.head._2.qRels)
    }else if (metric.startsWith("recall_")) {
      val scoresPerQuery = getScoresPerQuery(ru, getScoreP _)
      System.out.println(scoresPerQuery.head._2.qRels.topicIds.size + "********************* LipaniEstimator getScore")
      new Score(ru.id, new TRECEval().round(
        avg(getScoresPerQuery(ru, getScoreR _).map(_._2.score))),
        scoresPerQuery.head._2.metric,
        scoresPerQuery.head._2.qRels)
    }else
      null
  }
  // Macro Average
  def getScoreMacroR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n:Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)
    val an = getAdjP(n, ru, pool)
    val ad = getAdjP(d, ru, pool)
    val R = pool.qRels.sizeRel
    val srua = (srun * n + an * n)/(R + an * n + ad * (d - n))
    new Score(ru.id, srua, metric, pool.qRels)
  }

  // Micro Average
  def getScoreR(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n:Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)

    val as = pool.lRuns.map(rp => {
      val nPool = new Pool(List(rp), pool.qRels)
      val an = getAdjP(n, ru, nPool)
      //val ad = if(an > 0d) getAdjP(d, ru, nPool) else 0d // 0.0057 ±0.0019
      (an)//, ad)
    })
    //val ad = getAdjP(d, ru, pool)
    val R = pool.qRels.sizeRel
    val srua = avg(as.map(an => (srun * n + an * n)/(R + an * n)))
    //val srua = Math.log(avg(as.map(an =>
    //  Math.exp((srun * n + an * n)/(R + an * n)))))// + ad * (d - n))))//0.0008 +- 0.0003
    //val srua = Math.exp(avg(as.map(an =>
    //    Math.log((srun * n + an * n)/(R + an * n)))))// + ad * (d - n))))//0.0008 +- 0.0003
    new Score(ru.id, srua, metric, pool.qRels)
  }

  def getScoreR2(ru: Runs, pool: Pool = this.pool): Score = {
    def M(n:Int, ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

    val n = metric.split("_").last.toInt
    val d = System.getProperty("pool.depth").toInt
    val srun = M(n, ru)

    val as = pool.lRuns.map(rp => {
      val nPool = new Pool(List(rp),pool.qRels)
      (getAdjP(n, ru, nPool))//, getAdjP(d, ru, nPool))
    })
    val ad = getAdjP(d, ru, pool)
    /*val ads = pool.lRuns.par.map(rp => {
      val nPool = new Pool(List(rp),pool.qRels)
      getAdjP(d, ru, nPool)
    })*/
    val R = pool.qRels.sizeRel
    val srua = avg(as.map(a =>
      (srun * n + a * n)/(R + a * n + ad * (d - n))).toList)
    new Score(ru.id, srua, metric, pool.qRels)
  }

}

object LipaniEstimatorQB {

  def apply(pool:Pool, metric: String, descs: Descs) = new LipaniEstimatorQB(pool, metric, descs)

}