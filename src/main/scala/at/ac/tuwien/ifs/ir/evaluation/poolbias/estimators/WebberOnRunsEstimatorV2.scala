package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.{SimpleStats, StatsEval, TRECEval}
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
  * Created by aldo on 02/05/15.
  */

class WebberOnRunsEstimatorV2(pool: Pool, metric: String, descs: Descs = null, l1xo:L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) {

  def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getScore(ru: Runs): Score = {
    //if (metric.startsWith("P"))
      getScoreP(ru)
    //else
    //  null
  }

  def getAdjP(n:Int, ru:Runs, pool:Pool):Double = {
    lazy val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    def filterOrganization(ru:Runs, lRuns:List[Runs]):List[Runs] = {
      val sRuns = olRuns.find(_.map(_.id).contains(ru.id)).get
      ScoreEstimator.excludeRuns(sRuns, lRuns)
    }

    def filterRun(ru:Runs, lRuns:List[Runs]):List[Runs] =
      lRuns.filterNot(_.id == ru.id)

    def M(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeMetric("P_"+n, ru, qRels)

    def AM(ru: Runs, qRels: QRels = pool.qRels) =
      TRECEval().computeAntiMetric("P_"+n, ru, qRels)

    def avg(vs:Seq[Double]) : Double =
      if(vs.size != 0){
        vs.sum/vs.size
      }else
        0d

    def logAvg(vs:Seq[Double]) : Double =
      if(vs.size !=0) {
        Math.log(avg(vs.map(e => Math.exp(e))))
      }else
        0d

    val sru = M(ru)
    val kru = 1d - sru - AM(ru)

    val as = pool.lRuns.map(nRun => {
      val nRp =
        if(l1xo == L1xo.organization)
          filterOrganization(nRun, pool.lRuns)
        else
          filterRun(nRun, pool.lRuns)

      val nQRels = pool.getNewInstance(nRp).qRels
      val krp = 1d - M(nRun, nQRels) - AM(nRun, nQRels)
      val deltaP = M(nRun) - M(nRun, nQRels)
      (deltaP, krp)
    }).filter(e => e._1 > 0d).seq

    kru * logAvg(as.map(e => e._1/e._2))
  }

  def getScoreP(ru: Runs, pool: Pool = this.pool): Score = {
    val n = metric.split("_").last.toInt
    val sru = M(ru, pool.qRels)
    val a = getAdjP(n, ru, pool)
    new Score(ru.id, sru + a)
  }


  override def getName =
    if(l1xo == L1xo.organization)
      "WebberOnRunsV2L1OO"
    else
      "WebberOnRunsV2"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV2(pool, metric, descs, l1xo)

}

object WebberOnRunsEstimatorV2 {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo:L1xo = L1xo.run) = new WebberOnRunsEstimatorV2(pool, metric, descs, l1xo)

}