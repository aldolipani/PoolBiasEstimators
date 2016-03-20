package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
 * Created by aldo on 02/05/15.
 */


class TrueEstimator(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) extends ScoreEstimator(qRels, lRuns, metric, descs) {

  override def getScore(ru: Runs): Score = {
    if(lRuns.map(_.id).contains(ru.id))
      new Score(ru.id, M(ru))
    else
      new Score(ru.id, Double.NaN)
  }

  override def getAllScoresL1RO: List[Score] = {
    lRuns.map(runs => {
      getScore(runs)
    })
  }

  override def getAllScoresL1OO: List[Score] = {
    val nRp = descs.getRunsPerOrganization(lRuns)
    nRp.map(sRp => {
      sRp.map { runs =>
        getScore(runs)
      }
    }).flatten
  }
  override def getName = "True"

  override def getNewInstance(qRels: QRels, Rp: List[Runs], metric: String, descs: Descs = null) = new TrueEstimator(qRels, Rp, metric, descs)

}

object TrueEstimator {

  def apply(qRels: QRels, Rp: List[Runs], metric: String, descs: Descs = null) = new TrueEstimator(qRels, Rp, metric, descs)

}