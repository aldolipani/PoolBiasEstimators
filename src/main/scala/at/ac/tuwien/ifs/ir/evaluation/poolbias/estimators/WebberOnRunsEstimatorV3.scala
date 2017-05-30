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

class WebberOnRunsEstimatorV3(pool: Pool, metric: String, descs: Descs = null, l1xo:L1xo = L1xo.run) extends ScoreEstimator(pool, metric, descs) with Exporter{

  def isMetricSupported(metric: String) =
    metric.startsWith("P_") || metric.startsWith("recall_")

  override def getScore(ru: Runs): Score = {
    //if (metric.startsWith("P"))
      getScoreP(ru)
    //else
    //  null
  }

  def getAdjP(n:Int, ru:Runs, pool:Pool):Double = {
    def round(num: Double): Double = Math.round(num * 10000).toDouble / 10000

    selectPage(getName)
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

    def avg(vs:Seq[Double], n:Double) : Double =
      if(n != 0d){
        vs.sum/n
      }else
        0d

    def avgs(vs:Seq[Double]) : Double =
      avg(vs, vs.size)

    def logAvg(vs:Seq[Double]) : Double =
      if(vs.size !=0) {
        Math.log(avgs(vs.map(e => Math.exp(e))))
      }else
        0d

    val sru = M(ru)
    val kru = 1f - sru - AM(ru)

    /*if(kru <= 0d)
      return 0d*/

    val toExport = scala.collection.mutable.Map[String, String]()
    val as = pool.lRuns.sortBy(_.id).map(nRun => {
      val nRp =
        if(l1xo == L1xo.organization)
          filterOrganization(nRun, pool.lRuns)
        else
          filterRun(nRun, pool.lRuns)

      val rC = getRanksCorrelation(resizeRuns(nRun, n), resizeRuns(ru, n))

      //lazy val rC1 = getRanksCorrelation(
      //  selectRel(reduceRuns(nRun, n), pool.qRels),
      //  selectRel(reduceRuns(ru, n), pool.qRels))
      //lazy val rC2 = getRanksCorrelation(
      //  selectNonRel(reduceRuns(nRun, n), pool.qRels),
      //  selectNonRel(reduceRuns(ru, n), pool.qRels))
      //println(nRun.id + " " + ru.id + " " + rC)// + " " + rC2 + " " + (rC1-rC2))
      val nQRels = pool.getNewInstance(nRp).qRels
      val krp = 1d - M(nRun, nQRels).toFloat - AM(nRun, nQRels).toFloat // OK
      val deltaP = M(nRun) - M(nRun, nQRels) // OK

      //if((rC1 - rC2) > 0)
      //  (deltaP, krp) // deltaP*rC1	 MAE   	0.0030 ±0.0018
      //else
      toExport.put(nRun.id.split("@").head + ".deltaP", round(deltaP) + "")
      toExport.put(nRun.id.split("@").head + ".krp", round(krp) + "")
      toExport.put(nRun.id.split("@").head + ".kTau", round(rC) + "")

      //print(nRun.id + "," + deltaP + "," + krp + "," + rC + ",")
        (deltaP, krp,
          //0.5d + (rC1 - rC2)/2d)
          Math.abs(rC))//Math.pow(Math.abs(rC),2))
    }).filter(e => e._1 > 0d && e._3 > 0d).seq

    toExport.put("ru", ru.id)
    toExport.put("ru.sru", round(sru) + "")
    toExport.put("ru.kru", round(kru) + "")
    //print(sru + "," + kru + ",")
    if(as.nonEmpty) {
      val N = as.map(_._3).sum
      val v = kru * Math.log(avg(as.map(e => Math.exp(e._1 / e._2) * e._3), N)) // OK

      toExport.put("ru.logAvg", round(Math.log(avg(as.map(e => Math.exp(e._1 / e._2) * e._3), N))) + " ")
      toExport.put("ru.logAvgN", round(N) + " ")
      toExport.put("ru.a", round(v) + "")
      //println(avg(as.map(e => Math.exp(e._1 / e._2) * e._3), N) + "," + v)
      //val v = kru * avg(as.map(e => e._1), N)// 142808
      //val v = kru * avg(as.map(e => e._1 * e._3), N) //penultimo  // this is the best per query
      //val v = avg(as.map(e => e._1 * e._3), N) //ultimo bad
      //println(v, N)
      addRow(toExport.toMap)
      v
    } else {
      //println(0d + "," + 0d);
      toExport.put("ru.logAvg", round(0d) + "")
      toExport.put("ru.logAvgN", round(0d) + "")
      toExport.put("ru.a", round(0d) + "")
      addRow(toExport.toMap)
      0d
    }
  }

  def getScoreP(ru: Runs, pool: Pool = this.pool): Score = {
    val n = metric.split("_").last.toInt
    val sru = M(ru, pool.qRels)
    val a = getAdjP(n, ru, pool)
    new Score(ru.id, sru + a, metric, pool.qRels)
  }

  def resizeRuns(runs:Runs, n:Int):Runs = new Runs(runs.id, //(val iteration: String, val document: Document, val rank: Int, val score: Float
    runs.runs.map(run => new Run(run.id,
      run.runRecords.take(n))))

  def reduceRuns(runs:Runs, n:Int):Runs = new Runs(runs.id, //(val iteration: String, val document: Document, val rank: Int, val score: Float
    runs.runs.map(run => new Run(run.id,
      run.runRecords.map(rr => new RunRecord(rr.iteration, rr.document, if(rr.rank <= n) 1 else 2, rr.score)))))

  def selectRR(runs:Runs, qRels:QRels, isRel:Boolean):Runs = new Runs(runs.id, //(val iteration: String, val document: Document, val rank: Int, val score: Float
    runs.runs.map(run => new Run(run.id,
      run.runRecords.filter(rr => qRels.getRel(run.id, rr.document) > 0 && isRel || qRels.getRel(run.id, rr.document) == 0 && !isRel))))

  def selectRel(runs:Runs, qRels:QRels):Runs = selectRR(runs, qRels, true)

  def selectNonRel(runs:Runs, qRels:QRels):Runs = selectRR(runs, qRels, false)

  def getRanksCorrelation(r0:Runs, r1:Runs):Double = {
    val pairs =
      r0.runs.withFilter(runs0 => r1.selectByTopicId(runs0.id)!=null)
        .map(runs0 => (runs0, r1.selectByTopicId(runs0.id)))
    if(pairs.size > 0) {
      avg(pairs.map(rs => getRankCorrelation(rs._1, rs._2)))
    }else{
      0d
    }
  }

  def getRankCorrelation(r0:Run, r1:Run):Double = {
    val kc = new KendallsCorrelation()
    val intersection = r0.runRecords.filter(rr => r1.getByDocumentId(rr.document.id) != null)
    if(intersection.size >  1) {
      val a0 = intersection.map(rr => r0.getByDocumentId(rr.document.id).rank.toDouble).toArray
      val a1 = intersection.map(rr => r1.getByDocumentId(rr.document.id).rank.toDouble).toArray
      if(a0.forall(e => e == a0.head) || a1.forall(e => e == a1.head))
        0d
      else
        kc.correlation(a0, a1)
    }else{
      0d
    }
  }

  override def getName =
    if(l1xo == L1xo.organization)
      "WebberOnRunsV3L1OO"
    else
      "WebberOnRunsV3"

  override def getNewInstance(pool: Pool) = new WebberOnRunsEstimatorV3(pool, metric, descs, l1xo)

}

object WebberOnRunsEstimatorV3 {

  def apply(pool: Pool, metric: String, descs: Descs, l1xo:L1xo = L1xo.run) = new WebberOnRunsEstimatorV3(pool, metric, descs, l1xo)

}