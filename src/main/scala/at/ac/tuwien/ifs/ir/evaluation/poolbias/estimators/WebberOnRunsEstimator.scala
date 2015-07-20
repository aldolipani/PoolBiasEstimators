package at.ac.tuwien.ifs.poolbias.estimators

import at.ac.tuwien.ir.evaluation.PoolAnalyser
import at.ac.tuwien.ir.model.{Descs, QRels, Runs, Score}

/**
 * Created by aldo on 02/05/15.
 */

class WebberOnRunsEstimator(qRels: QRels, Rp: List[Runs], metric: String, descs: Descs = null) extends ScoreEstimator(qRels, Rp, metric, descs) {

  override def getScore(ru: Runs): Score = {
    val sru = M(ru)
    val poolAnalyser = new PoolAnalyser(Rp, qRels)
    val as = Rp.par.map(nRun => {
      val nRp = Rp.filterNot(_.id == nRun.id)
      val nQRels = poolAnalyser.repoolWith(nRp)
      M(nRun) - M(nRun, nQRels)
    }).seq
    val a = avg(as)
    new Score(ru.id, sru + a)
  }

  override def getName = "WebberOnRuns"

  override def getNewInstance(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs = null) = new WebberOnRunsEstimator(qRels, lRuns, metric, descs)

}

object WebberOnRunsEstimator {

  def apply(qRels: QRels, lRuns: List[Runs], metric: String, descs: Descs) = new WebberOnRunsEstimator(qRels, lRuns, metric, descs)

}