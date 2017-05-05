package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.pool.{DepthNPool, Pool}
import at.ac.tuwien.ifs.ir.model._
import at.ac.tuwien.ifs.utils.Exporter

/**
 * Created by aldo on 02/05/15.
 */


class TrueEstimator(pool:Pool, metric: String, descs: Descs = null) extends ScoreEstimator(pool, metric, descs){

  override def isMetricSupported(metric:String) = true

  override def getScore(ru: Runs): Score = {
    new Score(ru.id, M(ru))
  }

  override def getScoresPerQuery(ru: Runs, getScore: (Runs, Pool) => Score = getScore): List[(Int, Score)] = {
    def getRunsOnly(ru: Runs, tId: Int): Runs = {
      val sru: Run = ru.runsMapByTopicId.getOrElse(tId, new Run(tId, List()))
      new Runs(ru.id + "@" + tId, List(sru))
    }

    for (tId <- pool.qRels.topicIds.toList.sorted if (ru.selectByTopicId(tId) != null)) yield {
      val nRu = getRunsOnly(ru, tId)
      (tId, getScore(nRu, pool))
    }
  }


  override def getAllScoresL1RO: List[Score] =
    pool.lRuns.map(runs => {
      getScore(runs)
    })

  override def getAllScoresL1OO(): List[Score] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns) // wrong
    olRuns.map(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      slRuns.map(runs => {
        val nPool = pool.getNewInstance(nlRuns :+ runs)
        getScore(runs, nPool)})
    }).flatten
  }

  override def getAllScoresPerQueryL1RO: List[List[(Int, Score)]] = {
    pool.lRuns.map(runs => {
      getScoresPerQuery(runs)
    })
  }

  override def getAllScoresPerQueryL1OO(): List[List[(Int, Score)]] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)//wrong
    olRuns.map(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      slRuns.map(runs => {
        val nPool = pool.getNewInstance(nlRuns :+ runs)
        getScoresPerQuery(runs, nPool)})
    }).flatten
  }

  override def getName = "True"

  override def getNewInstance(pool:Pool) = new TrueEstimator(pool, metric, descs)

}

object TrueEstimator {

  def apply(pool:DepthNPool, metric: String, descs: Descs = null) = new TrueEstimator(pool, metric, descs)

}