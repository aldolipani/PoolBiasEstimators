package at.ac.tuwien.ifs.ir.evaluation

import java.io.File
import java.nio.file.{Files, Paths}

import at.ac.tuwien.ifs.io.TXTFile
import at.ac.tuwien.ifs.ir.model._

import scala.sys.process._
import scala.util.Random

/**
  * Created by aldo on 10/19/14.
  */
class TRECEval(tempDir: String = ".") {

  val temp: String = TRECEval.makeDir(tempDir)
  //TRECEval.clearFolder(temp)

  def avgInt(vs: Seq[Int]): Double =
    vs.sum.toDouble / vs.size

  def avg(vs: Seq[Double]): Double =
    avg(vs, vs.size)

  def avg(vs: Seq[Double], den: Int): Double =
    vs.sum / den

  def round(num: Double): Double = Math.round(num * 10000).toDouble / 10000

  def recall(run: Run, qRel: QRel): Double =
    if (qRel.sizeRel == 0) {
      0d
    } else {
      num_ret_rel(run, qRel).toDouble / qRel.sizeRel
    }

  def rbpw_p(p: Float, rank: Int): Double = (1d - p) * Math.pow(p, rank - 1)

  def rbp_p(p: Float, run: Run, qRel: QRel): Double =
    run.runRecords.map(rR => if (qRel.getRel(rR.document) > 0)
      rbpw_p(p, rR.rank) else 0d).sum

  def ap(run: Run, qRel: QRel): Double = {
    if (qRel.sizeRel != 0)
      run.runRecords.map(rR =>
        if (qRel.getRel(rR.document) > 0) {
          p_n(rR.rank, run, qRel)
        } else 0d).sum / qRel.sizeRel
    else
      0d
  }

  def dcg(run: Run, qRel: QRel): Double =
    run.runRecords.map(rR => if (qRel.getRel(rR.document) > 0)
      qRel.getRel(rR.document).toDouble / Math.log(rR.rank + 1) * Math.log(2) else 0d).sum

  def idcg(qRel: QRel): Double =
    if (qRel.sizeRel != 0)
      qRel.qrelRecords.filter(_.rel > 0).sortBy(-_.rel).zipWithIndex.map(di =>
        di._1.rel.toDouble / Math.log(di._2 + 2) * Math.log(2)).sum
    else
      0d

  def num_ret_rels(runs: Runs, qRels: QRels): List[Int] =
    runs.runs.withFilter(r => r != null && qRels.topicIds.contains(r.id)).map(run =>
      num_ret_rel(run, qRels.topicQRels(run.id)))

  def num_ret_rel(runs: Runs, qRels: QRels): Int =
    runs.runs.withFilter(r => r != null && qRels.topicIds.contains(r.id)).map(run =>
      num_ret_rel(run, qRels.topicQRels(run.id))).sum

  def recall(runs: Runs, qRels: QRels): List[Double] =
    runs.runs.withFilter(r => r != null && qRels.topicIds.contains(r.id)).map(run =>
      recall(run, qRels.topicQRels(run.id)))

  def rbp_p(p: Float, runs: Runs, qRels: QRels): Double = avg(
    runs.runs.withFilter(r => r != null && qRels.topicIds.contains(r.id)).map(run =>
      rbp_p(p, run, qRels.topicQRels(run.id)))
  )

  def map(runs: Runs, qRels: QRels): Double = avg(
    runs.runs.withFilter(r => r != null && qRels.topicIds.contains(r.id)).map(run => {
      ap(run, qRels.topicQRels(run.id))
    }))

  def ndcg(runs: Runs, qRels: QRels): Double = avg(
    runs.runs.withFilter(r => r != null && qRels.topicIds.contains(r.id)).map(run => {
      val den = idcg(qRels.topicQRels(run.id))
      if (den != 0)
        dcg(run, qRels.topicQRels(run.id)) / den
      else
        0d
    }))

  def p(n: Int, run: Run, qRel: QRel): Double =
    num_ret_rel(run, qRel).toDouble / n

  def num_ret_rel(run: Run, qRel: QRel): Int =
    run.runRecords.map(rR => if (qRel.getRel(rR.document) > 0) 1 else 0).sum

  def cut(n: Int, run: Run): Run = new Run(run.id, run.runRecords.take(n))

  def cut(n: Int, runs: Runs): Runs =
    new Runs(runs.id, runs.runs.withFilter(_ != null).map(run => cut(n, run)))

  def p_n(n: Int, run: Run, qRel: QRel): Double =
    num_ret_rel(cut(n, run), qRel).toDouble / n

  def p_n(n: Int, runs: Runs, qRels: QRels): Double =
    avgInt(
      num_ret_rels(cut(n, runs), qRels)
    ) / n

  def recall_n(n: Int, runs: Runs, qRels: QRels): Double =
    avg(
      recall(cut(n, runs), qRels) //, qRels.sizeTopics
    )

  def computeMetricPerTopic(metric: String, runs: Runs, qRels: QRels): Map[Int, Double] =
    qRels.qRels.map(qRel => (qRel.id -> computeMetric(metric, runs, qRels.getTopicQRels(qRel.id)))).toMap

  def computeRawMetricPerTopic(metric: String, runs: Runs, qRels: QRels): Map[Int, Double] =
    qRels.qRels.map(qRel => (qRel.id -> computeRawMetric(metric, runs, qRels.getTopicQRels(qRel.id)))).toMap

  def computeRawMetric(metric: String, runs: Runs, qRels: QRels): Double = {
    if (metric.startsWith("P_")) {
      val n = metric.split("_").last.toInt
      p_n(n, runs, qRels)
    } else if (metric.startsWith("recall_")) {
      val n = metric.split("_").last.toInt
      recall_n(n, runs, qRels)
    } else if (metric.startsWith("RBP_")) {
      val p = metric.split("_").last.toFloat
      rbp_p(p, runs, qRels)
    } else if (metric.startsWith("map")) {
      map(runs, qRels)
    } else if (metric.startsWith("ndcg")) {
      ndcg(runs, qRels)
    } else if (metric.startsWith("num_ret_rel")) {
      num_ret_rel(runs, qRels)
    } else
      computeUnsupportedMetric(metric: String, runs: Runs, qRels: QRels)
  }

  def computeMetric(metric: String, runs: Runs, qRels: QRels): Double = {
    if (metric.startsWith("P_")) {
      val n = metric.split("_").last.toInt
      round(p_n(n, runs, qRels))
    } else if (metric.startsWith("recall_")) {
      val n = metric.split("_").last.toInt
      round(recall_n(n, runs, qRels))
    } else if (metric.startsWith("RBP_")) {
      val p = metric.split("_").last.toFloat
      round(rbp_p(p, runs, qRels))
    } else if (metric.startsWith("map")) {
      round(map(runs, qRels))
    } else if (metric.startsWith("ndcg")) {
      round(ndcg(runs, qRels))
    } else if (metric.startsWith("num_ret_rel")) {
      num_ret_rel(runs, qRels)
    } else
      computeUnsupportedMetric(metric: String, runs: Runs, qRels: QRels)
  }

  def computeUnsupportedMetricPerTopic(metric: String, runs: Runs, qRels: QRels) = {
    qRels.qRels.map(qRel => (qRel.id -> {
      computeUnsupportedMetric(metric, runs, qRels.getTopicQRels(qRel.id))
    })).toMap
  }

  def computeUnsupportedMetric(metric: String, runs: Runs, qRels: QRels): Double = {
    def getRandomString: String = {
      val rS = TRECEval.getRandomString
      if ((new File(tempDir, "runs." + rS)).exists) getRandomString else rS
    }

    val rS = getRandomString
    val runsP = new File(tempDir, "runs." + rS).getCanonicalPath
    val qRelsP = new File(tempDir, "qRels." + rS).getCanonicalPath
    TXTFile.writeFile(runsP, runs.toString)
    TXTFile.writeFile(qRelsP, qRels.toString)
    try {
      val value = TRECEval.computeMetric(metric, runsP, qRelsP)
      TRECEval.deleteFile(runsP)
      TRECEval.deleteFile(qRelsP)
      value
    } catch {
      case e: Exception => {
        println(metric)
        println(runsP, runs.toString.length)
        println(qRelsP, qRels.toString.length)
        println(s"trec_eval $qRelsP $runsP" !!)
        //println((s"trec_eval $qRelsP $runsP" #| s"grep ^$metric\\s" !!).split("\t").last)
        throw e
      }
    }
  }

  def computeUnsupportedMetric(metric: String, runs: String, qRels: String): Double = {
    TRECEval.computeMetric(metric, runs, qRels)
  }


  def computeAntiMetric(metric: String, runs: Runs, qRels: QRels) = computeMetric(metric, runs, qRels.inverse)

  def computeRawAntiMetric(metric: String, runs: Runs, qRels: QRels) = computeRawMetric(metric, runs, qRels.inverse)

  def computeAntiMetricPerTopic(metric: String, runs: Runs, qRels: QRels) = computeMetricPerTopic(metric, runs, qRels.inverse)

  def computeRawAntiMetricPerTopic(metric: String, runs: Runs, qRels: QRels) = computeRawMetricPerTopic(metric, runs, qRels.inverse)

  def computeMAP(runs: Runs, qRels: QRels) = computeMetric("map", runs, qRels)

  def computeAntiMAP(runs: Runs, qRels: QRels) = computeMetric("map", runs, qRels.inverse)

  def computeNDCG(runs: Runs, qRels: QRels) = computeMetric("ndcg", runs, qRels)

  def computeP5(runs: Runs, qRels: QRels) = computeMetric("P_5", runs, qRels)

  def computeRecall5(runs: Runs, qRels: QRels) = computeMetric("recall_5", runs, qRels)

  def computeP10(runs: Runs, qRels: QRels) = computeMetric("P_10", runs, qRels)

  def computeRecall10(runs: Runs, qRels: QRels) = computeMetric("recall_10", runs, qRels)

  def computeP15(runs: Runs, qRels: QRels) = computeMetric("P_15", runs, qRels)

  def computeRecall15(runs: Runs, qRels: QRels) = computeMetric("recall_15", runs, qRels)

  def computeP20(runs: Runs, qRels: QRels) = computeMetric("P_20", runs, qRels)

  def computeRecall20(runs: Runs, qRels: QRels) = computeMetric("recall_20", runs, qRels)

  def computeP30(runs: Runs, qRels: QRels) = computeMetric("P_30", runs, qRels)

  def computeRecall30(runs: Runs, qRels: QRels) = computeMetric("recall_30", runs, qRels)

  def computeP100(runs: Runs, qRels: QRels) = computeMetric("P_100", runs, qRels)

  def computeRecall100(runs: Runs, qRels: QRels) = computeMetric("recall_100", runs, qRels)

  def computeNumRel(runs: Runs, qRels: QRels) = computeMetric("num_rel", runs, qRels)

  def computeNumRet(runs: Runs, qRels: QRels) = computeMetric("num_ret", runs, qRels)

  def computeAntiP5(runs: Runs, qRels: QRels) = computeMetric("P_5", runs, qRels.inverse)

  def computeAntiP10(runs: Runs, qRels: QRels) = computeMetric("P_10", runs, qRels.inverse)

  def computeAntiP15(runs: Runs, qRels: QRels) = computeMetric("P_15", runs, qRels.inverse)

  def computeAntiP20(runs: Runs, qRels: QRels) = computeMetric("P_20", runs, qRels.inverse)

  def computeAntiP30(runs: Runs, qRels: QRels) = computeMetric("P_30", runs, qRels.inverse)

  def computeAntiP100(runs: Runs, qRels: QRels) = computeMetric("P_100", runs, qRels.inverse)

  def computeAntiNumRel(runs: Runs, qRels: QRels) = computeMetric("num_rel", runs, qRels.inverse)

  def getScores(qRels: QRels, runs: List[Runs], metric: String): List[Score] =
    runs.map(run => {
      val score = new Score(run.id,
        this.computeMetric(metric, run, qRels),
        metric, qRels)
      score
    })
}

object TRECEval {

  def apply(tempDir: String = ".") = new TRECEval(tempDir)

  def computeMetric(metric: String, runs: String, qRels: String): Double = {
    val jm = metric.split("_").head
    //println(s"trec_eval -m $jm $qRels $runs"!)
    (s"trec_eval -m $jm $qRels $runs" #| s"grep ^$metric\\s" !!).trim.split("\t").last.toDouble
  }

  def makeDir(name: String) = {
    val dirF = new File(name)
    if (!dirF.exists) dirF.mkdir
    dirF.getName
  }

  def deleteFile(path: String) =
    Files.delete(Paths.get(path))

  def clearFolder(name: String) = {
    val listFiles = (new File(name)).listFiles
    if (listFiles != null) listFiles.map(_.delete)
  }

  private def getRandomString = 1 to 10 map (i => Random.nextInt(10)) mkString ("")

}
