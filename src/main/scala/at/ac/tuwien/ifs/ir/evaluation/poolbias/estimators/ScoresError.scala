package at.ac.tuwien.ifs.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.poolbias.estimators.Analysis.L1xo
import at.ac.tuwien.ifs.poolbias.estimators.Analysis.L1xo.L1xo
import at.ac.tuwien.ifs.poolbias.estimators.Analysis.L1xo.L1xo
import at.ac.tuwien.ir.model.Score
import org.apache.commons.math3.stat.correlation.KendallsCorrelation

import scala.io.Source

/**
 * Created by aldo on 15/02/15.
 */
class ScoresError(trueScores: List[Score], pValuesDir: File, metric: String) {

  if (trueScores.size != trueScores.map(_.runId).toSet.size) println("Warning: found duplicate runId in trueScores")

  lazy val trueScoresMap = trueScores.map(s => (s.runId -> s)).toMap
  lazy val trueScoresWithRankMap = withRank(trueScores).map(sr => (sr._1.runId -> sr)).toMap

  def meanAbsoluteError(testScores: List[Score]): Double = avg {
    testScores.map(ts => Math.abs(trueScoresMap.get(ts.runId).get.score - ts.score))
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
      pValues.getOrElse(runId1 + runId2, pValues.get(runId2 + runId1).getOrElse({
        println("Warning in systemRankError: " + runId1 + " " + runId2 + " doesn't exist!")
        1d
      }))
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
    printReportError("MAE", ("%1.4f" format round(this.meanAbsoluteError(scores))))
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

  def avg(x: Seq[Double]) = x.sum / x.size

  def round(num: Double) = Math.round(num * 10000).toDouble / 10000

  private def withRank(scores: List[Score]): List[(Score, Int)] = {
    val ss = scores.sortBy(_.runId).reverse.sortBy(-_.score)
    ss.zip(1 to ss.size).toList
  }
}
