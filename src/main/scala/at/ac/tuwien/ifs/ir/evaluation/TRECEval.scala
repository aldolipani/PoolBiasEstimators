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
    vs.sum / vs.size

  def round(num: Double) = Math.round(num * 10000).toDouble / 10000

  def num_ret_rel(run:Run, qRel: QRel):Int =
    run.runRecords.map(rR => if (qRel.getRel(rR.document.id) > 0) 1 else 0).sum

  def num_ret_rels(runs:Runs, qRels: QRels):List[Int] =
    runs.runs.withFilter(_ != null).map(run =>
      num_ret_rel(run, qRels.topicQRels.getOrElse(run.id, new QRel(run.id, Nil))))

  def p(n: Int, run: Run, qRel: QRel): Double =
    num_ret_rel(run, qRel).toDouble / n

/*  def p_n(n: Int, runs: Runs, qRels: QRels): Double = round(avg(
    runs.runs.filter(_ != null).map(run =>
      p(n, new Run(run.id, run.runRecords.filterNot(_.rank > n)),
        qRels.topicQRels.getOrElse(run.id, new QRel(run.id, Nil))))))
*/

  def cut(n:Int, runs:Runs):Runs =
    new Runs(runs.id, runs.runs.withFilter(_ != null).map(run => new Run(run.id, run.runRecords.take(n))))

  def p_n(n:Int, runs:Runs, qRels:QRels): Double =
    round(avgInt(
      num_ret_rels(cut(n, runs), qRels)
    )) / n

  def computeMetric(metric: String, runs: Runs, qRels: QRels): Double = {
    if (metric.startsWith("P_")) {
      val n = metric.split("_").last.toInt
      Math.round(p_n(n, runs, qRels) * 10000).toDouble / 10000
    } else
      computeUnsupportedMetric(metric: String, runs: Runs, qRels: QRels)
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

  def computeAntiMetric(metric: String, runs: Runs, qRels: QRels) = computeMetric(metric, runs, qRels.inverse)

  def computeMAP(runs: Runs, qRels: QRels) = computeMetric("map", runs, qRels)

  def computeAntiMAP(runs: Runs, qRels: QRels) = computeMetric("map", runs, qRels.inverse)

  def computeP5(runs: Runs, qRels: QRels) = computeMetric("P_5", runs, qRels)

  def computeP10(runs: Runs, qRels: QRels) = computeMetric("P_10", runs, qRels)

  def computeP15(runs: Runs, qRels: QRels) = computeMetric("P_15", runs, qRels)

  def computeP20(runs: Runs, qRels: QRels) = computeMetric("P_20", runs, qRels)

  def computeP30(runs: Runs, qRels: QRels) = computeMetric("P_30", runs, qRels)

  def computeP100(runs: Runs, qRels: QRels) = computeMetric("P_100", runs, qRels)

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
      new Score(run.id, this.computeMetric(metric, run, qRels))
    })
}

object TRECEval {

  def apply(tempDir: String = ".") = new TRECEval(tempDir)

  def computeMetric(metric: String, runs: String, qRels: String): Double = {
    (s"trec_eval $qRels $runs" #| s"grep ^$metric\\s" !!).trim.split("\t").last.toDouble
  }

  private def getRandomString = 1 to 10 map (i => Random.nextInt(10)) mkString ("")

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

}
