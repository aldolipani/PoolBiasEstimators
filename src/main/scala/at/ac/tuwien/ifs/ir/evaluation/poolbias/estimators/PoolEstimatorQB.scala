package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.Pool
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs, Score}

/**
 * Created by aldo on 02/05/15.
 */

class PoolEstimatorQB(pool: Pool, metric: String, descs: Descs = null) extends ScoreEstimator(pool, metric, descs) {

  override def isMetricSupported(metric: String) = true

  override def getScore(ru: Runs): Score =
    new Score(ru.id, new TRECEval().round(
      avg(getScoresPerQuery(ru, (ru, pool) =>
        new Score(ru.id, M(ru), metric, pool.qRels)).map(_._2.score))),
      metric, pool.qRels)

  override def getName = "PoolQB"

  override def getNewInstance(pool: Pool) = PoolEstimatorQB(pool, metric, descs)
}

object PoolEstimatorQB {

  def apply(pool: Pool, metric: String, descs: Descs = null) = new PoolEstimatorQB(pool, metric, descs)

}
