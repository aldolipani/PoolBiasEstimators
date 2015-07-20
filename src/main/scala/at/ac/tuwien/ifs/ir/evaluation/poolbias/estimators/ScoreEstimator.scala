package at.ac.tuwien.ifs.poolbias.estimators

import at.ac.tuwien.ifs.poolbias.estimators.Analysis.L1xo
import at.ac.tuwien.ifs.poolbias.estimators.Analysis.L1xo
import at.ac.tuwien.ifs.poolbias.estimators.Analysis.L1xo.L1xo
import at.ac.tuwien.ir.evaluation.{PoolAnalyser, TRECEval}
import at.ac.tuwien.ir.model._

/**
 * Created by aldo on 17/02/15.
 */
abstract class ScoreEstimator(qRels: QRels, lRuns: List[Runs] = Nil, metric: String, descs: Descs = null) {

  def getName: String = ???

  def getScore(runs: Runs): Score = ???

  def getScore(runs: Runs, nQRels: QRels): Score = getNewInstance(nQRels, lRuns.filterNot(_.id == runs.id), metric).getScore(runs)

  def getAllScores(l1xo: L1xo = L1xo.run): List[Score] = l1xo match {
    case L1xo.organization => scoresL1OO
    case L1xo.run => scoresL1RO
    case L1xo.both => throw new Exception("L1xo.both is not allowed in ScoreEstimator")
  }

  lazy val scoresL1OO = getAllScoresL1OO()
  lazy val scoresL1RO = getAllScoresL1RO()

  protected def getAllScoresL1RO(): List[Score] = {
    val poolAnalyser = new PoolAnalyser(lRuns, qRels)
    lRuns.map(runs => {
      val nlRuns = lRuns.filterNot(_.id == runs.id)
      val nQRels = poolAnalyser.repoolWith(nlRuns)
      getScore(runs, nQRels)
    })
  }

  protected def getAllScoresL1OO(): List[Score] = {
    val nlRuns = descs.getRunsPerOrganization(lRuns)
    val poolAnalyser = new PoolAnalyser(nlRuns.flatten, qRels)
    nlRuns.map(slRuns => {
      val llRuns = ScoreEstimator.excludeRuns(slRuns, nlRuns.flatten)
      val nQRels = poolAnalyser.repoolWith(llRuns)
      slRuns.map { runs =>
        getScore(runs, nQRels)
      }
    }).flatten
  }

  def getNewInstance(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null): ScoreEstimator = ???

  def printReportScores(l1xo: L1xo = L1xo.run) {
    println(getName + ":")
    val scores = this.getAllScores(l1xo)
    println("\t" + scores.mkString("\n\t"))
    println("")
  }

  def printScore(runs:Runs) {
    System.out.format("%-12s\t%s\n", getName, getScore(runs))
  }

  def avg(x: Seq[Double]) = x.sum / x.size

  def round(num: Double) = Math.round(num * 10000).toDouble / 10000

  def M(ru: Runs, qRels: QRels = qRels) =
    TRECEval().computeMetric(metric, ru, qRels)

  def AM(ru: Runs, qRels: QRels = qRels) = {
    TRECEval().computeAntiMetric(metric, ru, qRels)
  }
}

object ScoreEstimator {

  def excludeRuns(listToEsclude: List[Runs], runs: List[Runs]): List[Runs] =
    excludeRunsByIDs(listToEsclude.map(_.id), runs)

  def excludeRunsByIDs(listToEsclude: List[String], runs: List[Runs]): List[Runs] =
    runs.filterNot(run => listToEsclude.contains(run.id))

}
