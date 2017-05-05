package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo.L1xo
import at.ac.tuwien.ifs.ir.model._

/**
  * Created by aldo on 17/02/15.
  */
abstract class ScoreEstimator(val pool: Pool, val metric: String, descs: Descs = null) {

  def isMetricSupported(metric: String): Boolean

  def getName: String = ???

  def getScore(runs: Runs): Score = ???

  def getScoresPerQuery(ru: Runs, getScore: (Runs, Pool) => Score = getScore): List[(Int, Score)] = {
    def getRunsOnly(ru: Runs, tId: Int): Runs = {
      val sru: Run = ru.runsMapByTopicId.getOrElse(tId, new Run(tId, List()))
      new Runs(ru.id + "@" + tId, List(sru))
    }

    for (tId <- pool.qRels.topicIds.toList.sorted if (ru.selectByTopicId(tId) != null)) yield {
      val nRu = getRunsOnly(ru, tId)
      val nLRuns = pool.lRuns.map(r => getRunsOnly(r, tId))
      val nPool = pool.getNewInstance(nLRuns) //TODO: to check
      (tId, getScore(nRu, nPool))
    }
  }

  def getScore(runs: Runs, pool: Pool): Score =
    getNewInstance(pool).getScore(runs)

  def getScoresPerQuery(runs: Runs, pool: Pool): List[(Int, Score)] =
    getNewInstance(pool).getScoresPerQuery(runs)

  def getAllScores(l1xo: L1xo = L1xo.run): List[Score] = l1xo match {
    case L1xo.organization => scoresL1OO
    case L1xo.run => scoresL1RO
    case L1xo.both => throw new Exception("L1xo.both is not allowed in ScoreEstimator")
  }

  lazy val scoresL1OO = getAllScoresL1OO()
  lazy val scoresL1RO = getAllScoresL1RO()

  def getAllScoresPerQuery(l1xo: L1xo = L1xo.run): List[List[(Int, Score)]] = l1xo match {
    case L1xo.organization => scoresPerQueryL1OO
    case L1xo.run => scoresPerQueryL1RO
    case L1xo.both => throw new Exception("L1xo.both is not allowed in ScoreEstimator")
  }

  lazy val scoresPerQueryL1OO = getAllScoresPerQueryL1OO()
  lazy val scoresPerQueryL1RO = getAllScoresPerQueryL1RO()

  protected def getAllScoresL1RO(): List[Score] = {
    pool.lRuns./*par.*/map(runs => {
      val nlRuns = ScoreEstimator.excludeRuns(runs, pool.lRuns)
      val nPool = pool.getNewInstance(nlRuns)
      getScore(runs, nPool)
    }).seq.toList
  }

  protected def getAllScoresPerQueryL1RO(): List[List[(Int, Score)]] = {
    pool.lRuns.map(runs => {
      val nlRuns = ScoreEstimator.excludeRuns(runs, pool.lRuns)
      val nPool = pool.getNewInstance(nlRuns)
      getScoresPerQuery(runs, nPool)
    })
  }

  protected def getAllScoresL1OO(): List[Score] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns) // wrong
    olRuns.flatMap(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      val nPool = pool.getNewInstance(nlRuns)
      slRuns.map(runs => {
        getScore(runs, nPool)
      })
    }).seq.toList
  }

  protected def getAllScoresPerQueryL1OO(): List[List[(Int, Score)]] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns) // wrong
    olRuns.map(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      val nPool = pool.getNewInstance(nlRuns)
      slRuns.map(runs => {
        getScoresPerQuery(runs, nPool)
      })
    }).flatten
  }

  def getNewInstance(pool: Pool): ScoreEstimator = ???

  def printReportScores(l1xo: L1xo = L1xo.run) {
    println(getName + ":")
    val scores = this.getAllScores(l1xo)
    println("\t" + scores.mkString("\n\t"))
    println("")
  }

  def printScore(runs: Runs) {
    System.out.format("%-12s\t%s\n", getName, getScore(runs))
  }

  def avg(x: Seq[Double]) = x.sum / x.size

  def avg(x: Seq[Double], den: Double) = x.sum / den

  def round(num: Double) = Math.round(num * 10000).toDouble / 10000

  def M(ru: Runs, qRels: QRels = pool.qRels) =
    TRECEval().computeMetric(metric, ru, qRels)

  def AM(ru: Runs, qRels: QRels = pool.qRels) =
    TRECEval().computeAntiMetric(metric, ru, qRels)

}

object ScoreEstimator {

  def excludeRuns(listToEsclude: List[Runs], runs: List[Runs]): List[Runs] =
    excludeRunsByIDs(listToEsclude.map(_.id), runs)

  def excludeRuns(itemToEsclude: Runs, runs: List[Runs]): List[Runs] =
    excludeRuns(List(itemToEsclude), runs)

  def excludeRunsByIDs(listToEsclude: List[String], runs: List[Runs]): List[Runs] =
    runs.filterNot(run => listToEsclude.contains(run.id))

}
