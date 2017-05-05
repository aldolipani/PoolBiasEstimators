package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.model._

class LipaniEstimatorV2QB(pool: Pool, metric: String, descs: Descs = null) extends LipaniEstimatorV2(pool, metric, descs) {

  override def isMetricSupported(metric:String):Boolean =
    metric.startsWith("P_") || metric.startsWith("recall_")

  protected def getScorePerQuery(ru: Runs, getScore:(Runs, Pool) => Score): Score = {
    def getRunsOnly(ru:Runs, tId:Int):Runs = {
      val sru:Run = ru.runsMapByTopicId.getOrElse(tId, new Run(tId, List()))
      new Runs(ru.id+"@"+sru.id, List(sru))
    }

    val scores = pool.qRels.topicIds.toList.filter(tId => ru.selectByTopicId(tId) != null).sorted.map(tId => {
      val nRu = getRunsOnly(ru, tId)
      val nPool = pool.getNewInstance(pool.lRuns.map(rs => getRunsOnly(rs, tId)))
      getScore(nRu, nPool)
    })
    new Score(ru.id, avg(scores.map(_.score)))
  }

  def getAdjP(ru:Runs, pool:Pool = this.pool, n:Int = metric.split("_").last.toInt): Double = {
    def M(ru: Runs) =
      TRECEval().computeMetric("P_"+n, ru, pool.qRels)

    def AM(ru: Runs) =
      TRECEval().computeAntiMetric("P_"+n, ru, pool.qRels)

    val R = pool.qRels.sizeRel
    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1 - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
      val nrp = rp ◦ ru
      val δsrp = M(nrp) - M(rp)
      val δasrp = AM(nrp) - AM(rp)
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
        0
    a
  }

  protected def getScoreP(ru: Runs, pool:Pool = this.pool): Score = {
    val sru = M(ru)
    val a = getAdjP(ru, pool)
    new Score(ru.id, sru + a)
  }

  /*
  a. (sru*R + an*n)/(R + an*n + ad*(d-n))
  b. (sru*R + an*n)/(R + an*n + ad*(d-n)) only if an > 0d && ad > 0d
  c. (sru*R + an*n)/(R + ad*d) only if an > ad, (sru*R + an*n)/(R + an*n) if an > 0
  */
  protected def getScoreR(ru: Runs, pool:Pool = this.pool): Score = {
    val n = metric.split("_").last.toInt
    val d = new PoolAnalyzer(pool).d
    val R = pool.qRels.sizeRel
    val sru = M(ru, pool.qRels)
    val an = getAdjP(ru, pool, n)
    lazy val ad = getAdjP(ru, pool, d) // 1 Original 2 with an*d 3 with an*n
    /*val nsru =
      if(an*n < ad*d)
        (sru*R + an*n)/(R + ad*d)
      else if (an>0d)
        (sru*R + an*n)/(R + an*n)
      else
        sru*/
    //val nsru = (sru*R + an*n)/(R + an*n + ad*(d-n))
    //val nsru = (sru*R + an*n)/(R + an*n + ad*(d-n))
    //val nsru = (sru*R + an*n)/(R+an*n) // 0.0020
    //val nsru = (sru*R + an*n)/(R+an*n+ ad*(d-n)) // 0.0026
    //val nsru = (sru*R + an*n)/(R+ Math.max(an*n,ad*d)) // 0.0031
    //val nsru = (sru*R + an*n)/(R + ad*d) // 0.0031 - 0.0027
    //val nsru = (sru*R + an*n)/(R + an*d) // 0.0011 ±0.0004
    //val nsru = (sru*R + an*n)/(R + Math.min(an, ad)*d) // 0.0012 ±0.0003
    val nsru =
      if(ad >0)
        (sru*R + an*n)/(R + Math.min(an, ad)*d)
      else
        (sru*R + an*n)/(R + an*d)// 0.0012 ±0.0003
    def f(d:Double) = (if(d>=0) "+" else "") + "%1.5f" format d
    //println(ru.id + "\t" + f(sru) + "\t" + f(an) + "\t" + f(ad) + "\t" + f(nsru) + "\t" + R)
    println(ru.id + "\t" + f(sru) + "\t" + f(an) + "\t" + f(ad) + "\t" + f(nsru) + "\t" + R)
    new Score(ru.id, nsru)
  }

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_"))
      getScorePerQuery(ru, getScoreP)
    else if (metric.startsWith("recall_"))
      getScorePerQuery(ru, getScoreR)
    else
      null
  }

  override def getName = "LipaniV2QB"

  override def getNewInstance(pool: Pool) = new LipaniEstimatorV2QB(pool, metric, descs)

  import scala.sys.process._

  lazy val shouldIPrint = ("hostname" !!).trim == "Aldos-MBP" || ("hostname" !!).trim == "Aldos-MacBook-Pro.local"

  def println(str: String) =  if(shouldIPrint) System.out.println(str)
}

object LipaniEstimatorV2QB {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new LipaniEstimatorV2QB(pool, metric, descs)

}