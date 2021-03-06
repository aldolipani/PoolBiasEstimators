package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.model.Score

/**
 * Created by aldo on 05/09/16.
 */
class BiasAnalyser(trueScoreEstimator: ScoreEstimator, l1xo: L1xo, pValuesDir: File = null, metric: String = null, rounding: Boolean = false) {

  lazy val trueScoresPerQuery: List[List[(Int, Score)]] =
    trueScoreEstimator.getAllScoresPerQuery()

  lazy val trueScoresPerQueryPerRuns: Map[String, List[(Int, Score)]] =
    trueScoresPerQuery.map(l => (l.head._2.runId -> l)).toMap

  def differenceError(runScorePerQuery0: Map[Int, Score], runScorePerQuery1: Map[Int, Score]): Map[Int, Double] = {
    runScorePerQuery0.map(e => (e._1,
      e._2.score - {
        if (runScorePerQuery1.contains(e._1)) runScorePerQuery1(e._1).score else 0d
      }))
  }

  def printReportAnalysis(poolEstimator: PoolEstimator, scoreEstimator: ScoreEstimator, l1xo: L1xo) {
    val scoresPoolEstimator = poolEstimator.getAllScoresPerQuery(l1xo)
    val scoresEstimator = scoreEstimator.getAllScoresPerQuery(l1xo)
    println(scoreEstimator.getName + ":")
    for (rScores <- scoresPoolEstimator) {
      print(rScores.head._2.runId.split("@").head + ",")
      val diff = differenceError(rScores.toMap, scoresEstimator.find(_.head._2.runId == rScores.head._2.runId).get.toMap)
      println(diff.toList.sortBy(_._1).map(_._2).mkString(","))
    }
    println()
  }

  def avg(xs: Seq[Double]) = xs.sum / xs.size

  def avgCI(xs: Seq[Double]) = 1.96d * Math.sqrt((avg(xs.map(x => x * x)) - Math.pow(avg(xs), 2)) / xs.size)

  def round(num: Double) = if (rounding) Math.round(num * 10000).toDouble / 10000 else num

  private def withRank(scores: List[Score]): List[(Score, Int)] = {
    val ss = scores.sortBy(_.runId).reverse.sortBy(-_.score)
    ss.zip(1 to ss.size)
  }
}

