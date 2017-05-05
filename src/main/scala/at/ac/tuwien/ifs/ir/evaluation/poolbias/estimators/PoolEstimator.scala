package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
 * Created by aldo on 02/05/15.
 */

class PoolEstimator(pool:Pool, metric: String, descs: Descs = null) extends ScoreEstimator(pool, metric, descs) {

  override def isMetricSupported(metric:String) = true

  override def getScore(runs: Runs): Score = {
    new Score(runs.id, M(runs))
  }

  override def getName = "Pool"

  override def getNewInstance(pool:Pool) = PoolEstimator(pool, metric, descs)
}

object PoolEstimator {

  def apply(pool:Pool, metric: String, descs: Descs = null) = new PoolEstimator(pool, metric, descs)

}