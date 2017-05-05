package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{DepthNPool, Pool, PoolAnalyzer}
import at.ac.tuwien.ifs.ir.model._

class LipaniEstimatorV2QB(pool: Pool, metric: String, descs: Descs = null, D:Int = -1) extends LipaniEstimatorV2(pool, metric, descs) {

  lazy val d = if(D < 0) new PoolAnalyzer(pool).d else D

  override def isMetricSupported(metric: String): Boolean =
    metric.startsWith("P_") || metric.startsWith("recall_")

  def getAdjP(ru: Runs, lPool: Pool = this.pool, n: Int = metric.split("_").last.toInt): Double = {
    def M(ru: Runs) =
      TRECEval().computeMetric("P_" + n, ru, lPool.qRels)

    def AM(ru: Runs) =
      TRECEval().computeAntiMetric("P_" + n, ru, lPool.qRels)

    val sru = M(ru)
    val asru = AM(ru)
    val kru = 1d - (sru + asru)
    if(kru == 0d) return 0d

    val vs = lPool.lRuns.par.map(rp => {
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
    //val a =
    if (λ > 0)
      kru * Math.min(Math.max(avg(δks), 0d), 1d)
    else
      0d
//    def f(d: Double) = (if (d >= 0) "+" else "") + "%1.5f" format d
//    println(
//        "n" + "\t" + n + "\t" +
//        "id" + "\t" + ru.id + "\t" +
//        "sru" + "\t" + f(sru) + "\t" +
//        "asru" + "\t" + f(asru) + "\t" +
//        "kru" + "\t" + f(kru) + "\t" +
//        "sru" + "\t" + f(Δsru) + "\t" +
//        "asru" + "\t" + f(Δasru) + "\t" +
//        "ksu" + "\t" + f(Math.max(avg(δks), 0d)) + "\t" +
//        "lambda" + "\t" + λ + "\t" +
//        "R" + "\t" + lPool.qRels.sizeRel + "\t" +
//        "t" + "\t" + ru.topicIds.head)
//    (λ, a)
  }

  protected def getScoreRPool(ru: Runs, pool: Pool): Score = {
    val ku = (1d - (TRECEval().computeMetric("P_" + d, ru, pool.qRels) + TRECEval().computeAntiMetric("P_" + d, ru, pool.qRels))) * d
    val C = pool.qRels.size + ku
    val sru = pool.qRels.sizeRel.toDouble / C
    val asru = (0d +
      pool.qRels.size -
      pool.qRels.sizeRel -
      ku
      ) / C
    val kru = 1d - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
      val nrp = rp ◦ ru
      val nQRels = new DepthNPool(d, pool.lRuns.filter(_.id != rp.id) :+ nrp, pool.qRels).qRels
      val nkp = (1d - (TRECEval().computeMetric("P_" + d, nrp, nQRels) + TRECEval().computeAntiMetric("P_" + d, nrp, nQRels))) * d
      val kp = (1d - (TRECEval().computeMetric("P_" + d, rp, pool.qRels) + TRECEval().computeAntiMetric("P_" + d, rp, pool.qRels))) * d
      val Cn = nQRels.size + nkp
      val Ck = pool.qRels.size + kp
      val δsrp = nQRels.sizeRel.toDouble / Cn - pool.qRels.sizeRel.toDouble / Ck // repool
      val δasrp =
        (nQRels.size - nQRels.sizeRel - nkp) / Cn -
          (pool.qRels.size - pool.qRels.sizeRel - kp) / Ck
      val δkrp = δsrp - δasrp
      (δsrp, δasrp, δkrp)
    }).seq
    val (δss, δass, δks) = (vs.map(_._1), vs.map(_._2), vs.map(_._3))
    val Δsru = avg(δss)
    val Δasru = avg(δass)
    val λ = kru * (Δsru * asru - Δasru * sru)
    val a =
      //if (λ > 0)
        kru * Math.max(avg(δks), 0d)
      //else
      //  0d

    def f(d: Double) = (if (d >= 0) "+" else "") + "%1.5f" format d
    //println(ru.id + "\t" + f(sru) + "\t" + f(an) + "\t" + f(ad) + "\t" + f(nsru) + "\t" + R)
//    println(ru.id + "\t" + f(sru) + "\t" + (asru) + "\t" + (kru) + "\t" + f(Math.max(avg(δks), 0d)) + "\t" + sru * C + "\t" + (sru + a) * C + "\t" + C + "\t" + pool.qRels.size + "\t" + 1d / ((sru + a) * C) + "\t" + λ)

    new Score(ru.id, (sru + a) * C, metric, pool.qRels)
    //new Score(ru.id, pool.qRels.sizeRel)
  }

  protected def getScoreRPool2(ru: Runs, pool: Pool): Score = {
    val ku = (1d - (TRECEval().computeMetric("P_" + d, ru, pool.qRels) + TRECEval().computeAntiMetric("P_" + d, ru, pool.qRels))) * d
    val C = d * (pool.lRuns.size + 1)
    val sru = (pool.lRuns.map(rr => TRECEval().computeMetric("P_" + d, rr, pool.qRels)*d).sum + TRECEval().computeMetric("P_" + d, ru, pool.qRels)*d) / C // this is not true
    val asru = (C - sru*C - ku) / C
    val kru = 1d - (sru + asru)
    val vs = pool.lRuns.par.map(rp => {
      val nrp = rp ◦ ru
      val nQRels = new DepthNPool(d, pool.lRuns.filter(_.id != rp.id).toList :+ nrp, pool.qRels).qRels

      val nkp = (1d - (TRECEval().computeMetric("P_" + d, nrp, nQRels) + TRECEval().computeAntiMetric("P_" + d, nrp, nQRels))) * d
      val kp = (1d - (TRECEval().computeMetric("P_" + d, rp, pool.qRels) + TRECEval().computeAntiMetric("P_" + d, rp, pool.qRels))) * d

      val δsrp = (pool.lRuns.map(rr => TRECEval().computeMetric("P_" + d, rr, nQRels    )*d).sum + TRECEval().computeMetric("P_" + d, ru, nQRels    )*d) / C -
                 (pool.lRuns.map(rr => TRECEval().computeMetric("P_" + d, rr, pool.qRels)*d).sum + TRECEval().computeMetric("P_" + d, ru, pool.qRels)*d) / C
      val δasrp = (C - (pool.lRuns.map(rr => TRECEval().computeMetric("P_" + d, rr, nQRels    )*d).sum + TRECEval().computeMetric("P_" + d, ru, nQRels    )*d) - nkp) / C -
        (C - (pool.lRuns.map(rr => TRECEval().computeMetric("P_" + d, rr, pool.qRels)*d).sum + TRECEval().computeMetric("P_" + d, ru, pool.qRels)*d) - kp) / C
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

    def f(d: Double) = (if (d >= 0) "+" else "") + "%1.5f" format d
    //println(ru.id + "\t" + f(sru) + "\t" + f(an) + "\t" + f(ad) + "\t" + f(nsru) + "\t" + R)
//    println("R - " + ru.id + "\t" + f(sru) + "\t" + (asru) + "\t" + (kru) + "\t" + f(Math.max(avg(δks), 0d)) + "\t" + sru * C + "\t" + (sru + a) * C + "\t" + C + "\t" + pool.qRels.size + "\t" + 1d / ((sru + a) * C) + "\t" + λ)

    new Score(ru.id, a * C, metric, pool.qRels)
    //new Score(ru.id, pool.qRels.sizeRel)
  }

   protected def getScoreeP(ru: Runs, pool: Pool = this.pool): Score = {
    val sru = M(ru)
    val a = getAdjP(ru, pool)
    //new Score(ru.id, sru + (if (a._1 > 0) a._2 else 0d))
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  /*
  a. (sru*R + an*n)/(R + an*n + ad*(d-n))
  b. (sru*R + an*n)/(R + an*n + ad*(d-n)) only if an > 0d && ad > 0d
  c. (sru*R + an*n)/(R + ad*d) only if an > ad, (sru*R + an*n)/(R + an*n) if an > 0
  */
  protected def getScoreR(ru: Runs, lPool:Pool): Score = {
    val n = metric.split("_").last.toInt
    val R = lPool.qRels.sizeRel
    val sru = M(ru, lPool.qRels)
    //val d = 100
    val an = /*if(n<R) */getAdjP(ru, lPool, n)// else (0d,0d)
    val ad = getAdjP(ru, lPool, d) // 1 Original 2 with an*d 3 with an*n
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
    /*val nsru =
      if(an <= 0d || ad <= 0d){
        sru
      }else{
        if(ad*d >= an*n){
          (sru * R + an * n) / (R + ad * d)
        }else {
          (sru * R + an * n) / (R + an * n)
        }
      }*/
    // 0.0008 ±0.0004

    //val can = if (an._1 > 0d) an._2 else 0d
    //val cad = /*if (ad._1 > 0d) */
    //  ad._2// else 0d
    /*val nsru =
      if(an._1 > 0)
      (sru * R + can * n)/getScoreRPool(ru, pool).score
    else
        sru*/

    //MAE   	0.0009 ±0.0003
/*    val nsru =
      if (an._1 > 0d && ad._1 > 0d && R>(TRECEval().computeMetric("recall_"+d, ru, pool.qRels)*R + cad*d)) //0.0034 ±0.0010
      //(sru + can * Math.pow(n/R,2)) / (1 + can * Math.pow(n/R,2) )
        (sru*R + can * n) / (R + can*n + cad * (d-n)) // TREC-3 0.0023 ±0.0011
      //(sru * R + can * n) / (R + getScoreRPool2(ru, pool).score) // 0.0153 ±0.0033 with R>n 0.0066 ±0.0021
      else
        sru*/

//    val nsru =
//      if (an._1 > 0d && ad._1 > 0d && R>(TRECEval().computeMetric("recall_"+d, ru, pool.qRels)*R + can*d)) //0.0034 ±0.0010
//        (sru*R + can * n) / (R + can*n + cad * (d-n)) // TREC-3 0.0023 ±0.0011
//      else
//        sru
    val nsru =
      //if (an._1 > 0d && cad > can && R>(TRECEval().computeMetric("recall_"+d, ru, lPool.qRels)*R + can*n + cad * (d-n))) //0.0034 ±0.0010
        (sru * R + an * n) / (R + an * n + an * (d-n)) // TREC-3 0.0023 ±0.0011
      //else
      //  sru

    //val nsru = (sru*R + an*n)/(R + Math.min(an, ad)*d) // 0.0012 ±0.0003
    //val nsru =
    //  if(ad >0)
    //    (sru*R + an*n)/(R + Math.min(an, ad)*d)
    //  else
    //    (sru*R + an*n)/(R + an*d) //0.0011 ±0.0004
    /*val nsru =
    if(ad >0)
    (sru*R + an*n)/(R + Math.max(an*n, ad*d))
    else
    (sru*R + an*n)/(R + an*n) //0.0011 ±0.0004
    */
    //def f(d: Double) = (if (d >= 0) "+" else "") + "%1.5f" format d
    //println(ru.id + "\t" + f(sru) + "\t" + f(an) + "\t" + f(ad) + "\t" + f(nsru) + "\t" + R)
    //println(ru.id + "\t" + f(sru) + "\t" + (an) + "\t" + (ad) + "\t" + f(nsru) + "\t" + R)
    new Score(ru.id, nsru, metric, pool.qRels)
  }

  override def getScore(ru: Runs): Score = {
    if (metric.startsWith("P_"))
      new Score(ru.id, avg(getScoresPerQuery(ru, getScoreeP _).map(_._2.score)), metric, pool.qRels)
    else if (metric.startsWith("recall_"))
      new Score(ru.id, avg(getScoresPerQuery(ru, getScoreR _).map(_._2.score)), metric, pool.qRels)
    else
      null
  }

  override def getName = "LipaniV2QB"

  override def getNewInstance(pool: Pool) = new LipaniEstimatorV2QB(pool, metric, descs, d)

  import scala.sys.process._

  lazy val shouldIPrint = ("hostname" !!).trim == "Aldos-MBP" || ("hostname" !!).trim == "Aldos-MacBook-Pro.local"

  def println(str: String) = if (shouldIPrint) System.out.println(str)
}

object LipaniEstimatorV2QB {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new LipaniEstimatorV2QB(pool, metric, descs)

}