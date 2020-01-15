package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.{DepthNPool, Pool}
import at.ac.tuwien.ifs.ir.model.{Descs, Runs, Score}
import at.ac.tuwien.ifs.utils.Exporter

/**
 * Created by aldo on 09/11/16.
 */

class TrueEstimatorQB(pool: Pool, metric: String, descs: Descs = null) extends TrueEstimator(pool, metric, descs) with Exporter {

  override def getScore(ru: Runs): Score = {
    new Score(ru.id,
      new TRECEval().round(avg(getScoresPerQuery(ru, getScoreWithPool _).map(_._2.score))),
      metric, pool.qRels)
  }

  def getScoreWithPool(ru: Runs, pool: Pool = this.pool): Score = {
    //def round(num: Double): Double = Math.round(num * 10000).toDouble / 10000
    selectPage(getName)
    addRow(Map("ru" -> ru.id, "ru.sru" -> TRECEval().round(M(ru, pool.qRels)).toString))
    //println("#" + ru.id + "," + M(ru, pool.qRels))
    new Score(ru.id, M(ru, pool.qRels), metric, pool.qRels)
  }

  override def getName = "TrueQB"

  override def getNewInstance(pool: Pool) = new TrueEstimatorQB(pool, metric, descs)

}

object TrueEstimatorQB {

  def apply(pool: DepthNPool, metric: String, descs: Descs = null) = new TrueEstimatorQB(pool, metric, descs)

}