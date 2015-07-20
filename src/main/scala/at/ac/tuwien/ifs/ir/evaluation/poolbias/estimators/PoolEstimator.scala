package at.ac.tuwien.ifs.poolbias.estimators

import at.ac.tuwien.ir.model.{Descs, QRels, Runs, Score}

/**
 * Created by aldo on 02/05/15.
 */

class PoolEstimator(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) extends ScoreEstimator(qRels, lRuns, metric, descs) {

  override def getScore(runs: Runs): Score = {
    new Score(runs.id, M(runs))
  }

  override def getName = "Pool"

  override def getNewInstance(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) = new PoolEstimator(qRels, lRuns, metric, descs)
}

object PoolEstimator {

  def apply(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) = new PoolEstimator(qRels, lRuns, metric, descs)

}