package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model.Score
import at.ac.tuwien.ifs.r.Stats._
import org.apache.commons.math3.stat.correlation.KendallsCorrelation
import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYPlotStyle

import scala.io.Source



/**
 * Created by aldo on 15/02/15.
 */
class ScoresError(trueScores:List[Score], trueScoresPerQueries: List[List[(Int, Score)]], pValuesDir: File =
null, metric: String = null) {

  if (trueScores.size != trueScores.map(_.runId).toSet.size) println("Warning: found duplicate runId in trueScores")

  lazy val trueScoresMap = trueScores.map(s => (s.runId -> s)).toMap
  lazy val trueScoresPerQueryMap = trueScoresPerQueries.map(ss => (ss.head._2.runId -> ss.toMap)).toMap

  lazy val trueScoresWithRankMap = withRank(trueScores).map(sr => (sr._1.runId -> sr)).toMap

  def meanAbsoluteError(testScores: List[Score]): (Double, Double) = {
    val xs = testScores.map(ts => Math.abs(trueScoresMap.get(ts.runId).get.score - ts.score))
    (avg(xs), avgCI(xs))
  }

  def error(testScoresPerQueries: List[List[(Int, Score)]]): List[(String, List[(Int, Double)])] = {
    val xs =
      testScoresPerQueries.map(run =>
        (run.head._2.runId, run.map(ts => (ts._1, trueScoresPerQueryMap(run.head._2.runId)(ts._1).score - ts._2.score))))
    xs
  }

  private def findRank(score: Score, scores: List[Score]): Int = {
    def findRank(scores: List[Score], r: Int): Int = {
      if (scores.isEmpty || score.score >= scores.head.score)
        r
      else
        findRank(scores.tail, r + 1)
    }
    findRank(scores.sortBy(-_.score), 1)
  }

  def systemRankError(testScores: List[Score]): Int = {
    testScores.map(s => {
      val oR = findRank(trueScoresMap.get(s.runId).get, trueScores)
      val nR = findRank(s, trueScores.filter(_.runId != s.runId))
      Math.abs(oR - nR)
    }).sum
  }

  def systemRankError(testScores: List[Score], pValues: Map[String, Double]): Int = {
    def getPValue(runId1: String, runId2: String) = {
      pValues.getOrElse(runId1 + runId2, pValues.get(runId2 + runId1).getOrElse(
        pValues.get(runId2.toLowerCase() + runId1.toLowerCase()).getOrElse(
          pValues.get(runId1.toLowerCase() + runId2.toLowerCase()).getOrElse({
        println("Warning in systemRankError: " + runId1 + " " + runId2 + " doesn't exist!")
        1d
      }))))
    }
    testScores.map(s => {
      val oR = findRank(trueScoresMap.get(s.runId).get, trueScores)
      val nR = findRank(s, trueScores.filter(_.runId != s.runId))
      val nTrueScoresWithRank = withRank(trueScores).filter(e => e._1.runId != s.runId)
      nTrueScoresWithRank.filter(swr =>
        (nR <= swr._2 && swr._2 < oR) ||
          (oR < swr._2 && swr._2 <= nR))
        .filter(r => getPValue(s.runId, r._1.runId) < 0.05d).size
    }).sum
  }

  // Tau-b
  def kendallsCorrelation(testScores: List[Score]): Double = {
    val kc = new KendallsCorrelation()
    val py = withRank(testScores)
    val x = py.map(t => trueScoresWithRankMap.get(t._1.runId).get._1.score).toArray
    val y = py.map(_._1.score).toArray
    kc.correlation(x, y)
  }

  def printReportErrors(scoreEstimator: ScoreEstimator, l1xo: L1xo = L1xo.run) {
    def printReportError(metric:String, score:String) = System.out.format("\t%-6s\t%s\n", metric, score)

    val scores = scoreEstimator.getAllScores(l1xo)
    println(scoreEstimator.getName + ":")
    val mae = this.meanAbsoluteError(scores)
    printReportError("MAE", ("%1.4f" format round(mae._1)) + " " +  ("±%1.4f" format round(mae._2)))
    printReportError("SRE", this.systemRankError(scores).toString)
    if (pValuesDir != null) {
      val pValuesFile = new File(pValuesDir, "pValues." + metric + ".csv")
      if (pValuesFile.exists()) {
        val pValues: Map[String, Double] = Source.fromFile(pValuesFile).getLines().map(_.split(",")).filter(_.size == 3).map(a => (a(0).replace("input.", "") + a(1).replace("input.", "") -> a(2).toDouble)).toMap
        printReportError("SRE*", this.systemRankError(scores, pValues) + "\tp<0.05")
      }
    }
    printReportError("KTauB", ("%1.4f" format round(this.kendallsCorrelation(scores))))
    println("")
  }

  def transplose(l:List[(String, List[(Int, Double)])]):List[(Int, List[(String, Double)])] =
    l.head._2.map(tId =>
      (tId._1,
        l.flatMap(e => e._2.filter(_._1 == tId._1).map(a => (e._1, a._2)))))

  def printReportAnalysis(scoreEstimator: ScoreEstimator, l1xo: L1xo = L1xo.run) {
    def printReportError(metric:String, score:String) = System.out.format("\t%-6s\t%s\n", metric, score)
    def f(d: Double) = "%1.5f" format d

    val scores = scoreEstimator.getAllScoresPerQuery(l1xo)
    println(scoreEstimator.getName + ":")

    // Stats and Error Distribution
    println("\tStats"); {
      val er = this.error(scores)
      val et = transplose(er).sortBy(_._1)
      val etm = et.map(e => (e._1, e._2.toMap)).toMap

      val xsr = (-10 to 9 by 1).map(e => (e * 0.005d / 2 - 0.0025d / 2, (e + 1) * 0.005d / 2 - 0.0025d / 2))
      val xsp = xsr.map(e => (e._1 + e._2) / 2d)
      //println(xsr)
      val es = et.flatMap(_._2.map(_._2))
      val qs = quantile(es)
      //println(qs)
      println("\tmin: " + f(es.min) + "\t1st: " + f(qs._2) + "\tavg: " + f(mean(es)) + "\t3rd: " + f(qs._4) + "\tmax: " + f(es.max) + "\tstd: " + f(sd(es)) + "\tn: " + es.size)
      //* Sample Correlation with number of relevant documents
      val ssrs = scores.flatMap(r => r.map(t => (t._2.score, scoreEstimator.pool.qRels.topicQRels(t._1).sizeRel))).unzip
      val corRe = cor(ssrs._1, ssrs._2)
      println("\tcor_R: " + f(corRe))
      //* Sample Correlation with run performance
      val sses = scores.flatMap(r => r.map(t => (t._2.score, etm(t._1)(t._2.runId)))).unzip
      val corSe = cor(sses._1, sses._2)
      println("\tcor_S: " + f(corSe))

      val a = et.map(t => {
        val ys = xsr.map(x =>
          t._2.count(e => x._1 < e._2 && e._2 <= x._2).toDouble)
        ys
      })
      val ys = a.tail.foldRight(a.head)((b, c) => b.zip(c).map(e => e._1 + e._2))
      println("\n\tError Distribution Plot")
      output(PNG("plots/", "ed-"  + scoreEstimator.metric + "-" + scoreEstimator.getName), xyChart(xsp -> Y(ys, style = XYPlotStyle.LinesPoints)))
      println(output(ASCII, xyChart(xsp -> Y(ys, style = XYPlotStyle.LinesPoints))))
    }
    println("\n\t Q-Q Plot"); {
      val et = scores.flatMap(r => r.map(t => (t._2.runId, t._1, t._2.score)))
      val ys = et.map(_._3)
      val xs = et.map(e => trueScoresPerQueryMap.get(e._1).get(e._2).score)
      output(PNG("plots/", "qq-" + scoreEstimator.metric + "-" + scoreEstimator.getName), xyChart(xs -> Y(ys, style = XYPlotStyle.Points)))
      println(output(ASCII, xyChart(xs -> Y(ys, style = XYPlotStyle.Points),
        x = Axis(label="True", range = (0d, 1d)),
        y = Axis(label="Predicted",range = (0d, 1d))
      )))
    }
  }

  def avg(xs: Seq[Double]) = xs.sum / xs.size

  def first(xs: Seq[Double]) = xs.sum / xs.size

  def avgCI(xs:Seq[Double]) = 1.96d * Math.sqrt((avg(xs.map(x => x*x)) - Math.pow(avg(xs),2)) / xs.size)

  def round(num: Double) = Math.round(num * 10000).toDouble / 10000

  private def withRank(scores: List[Score]): List[(Score, Int)] = {
    val ss = scores.sortBy(_.runId).reverse.sortBy(-_.score)
    ss.zip(1 to ss.size).toList
  }
}
