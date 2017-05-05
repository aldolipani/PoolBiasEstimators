package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.annotation.tailrec

/**
  * Multi-Armed Bandits Based Pool
  * Created by aldo on 25/07/16.
  */
class HedgeBasedPool(beta: Double, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = HedgeBasedPool.getName(beta, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToHedgeBased(beta, poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = HedgeBasedPool.getPooledDocuments(beta, topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = HedgeBasedPool(beta, poolSize, lRuns, gT)

}

object HedgeBasedPool {

  def apply(beta: Double, poolSize: Int, lRuns: List[Runs], gT: QRels) = new HedgeBasedPool(beta, poolSize, lRuns, gT)

  def getName(beta:Double, poolSize:Int) = "hedgebased_" + beta + ":" + poolSize

  def getPooledDocuments(beta: Double, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    // runId# -> documents
    lazy val lrs: Map[Int, List[Document]] =
      pRuns.filter(_.selectByTopicId(topicId) != null).zipWithIndex.map(run =>
        run._2 -> run._1.selectByTopicId(topicId).runRecords.map(_.document)).toMap

    lazy val srs: Map[Int, Map[Document, Int]] =
      pRuns.filter(_.selectByTopicId(topicId) != null).zipWithIndex.map(run =>
        run._2 -> run._1.selectByTopicId(topicId).runRecords.zipWithIndex.map(e => (e._1.document, e._2 + 1)).toMap).toMap

    //initialization
    val prs: Map[Int, (Double, Double)] = lrs.map(e => e._1 ->(1d, 1d / pRuns.size))
    val allDs: Set[Document] = pRuns.filter(_.selectByTopicId(topicId) != null)
      .flatMap(_.selectByTopicId(topicId).runRecords.map(_.document)).toSet
    // update
    val rLoss = lrs.map(r =>
      (r._1,
        Math.log(allDs.size) / 2 - (r._2.size until allDs.size).map(i => Math.log(i)).sum / (allDs.size - r._2.size) / 2))

    val loss =
      for (r <- prs; d <- allDs) yield {
        (r._1, d,
          if (srs(r._1).contains(d))
            Math.log(allDs.size) / 2 - Math.log(srs(r._1)(d)) / 2
          else
            rLoss(r._1))
      }

    val lossDr = loss.groupBy(_._2)

    def getDocument(prs: Map[Int, (Double, Double)], docs: Set[Document], acc: Set[Document]): Set[Document] = {
      if (acc.size == nDs(topicId))
        acc
      else {
        // selection
        val dmax = docs.map(d => (d, lossDr(d).map(e => prs(e._1)._2 * e._3).sum)).maxBy(_._2)._1

        val drel = if (qRels.getRel(topicId, dmax.id) > 0) 1 else 0
        // update
        val ndocs = docs - dmax
        val nacc = acc + dmax

        val lossR =
          for (r <- prs) yield {
            (r._1,
              if (srs(r._1).contains(dmax))
                0.5d * Math.pow(-1d, drel) * (Math.log(allDs.size) - Math.log(srs(r._1)(dmax)))
              else
                Math.pow(-1d, drel) * rLoss(r._1))
          }

        val wprs = prs.map(r => r._1 ->(
          r._2._1 * Math.pow(beta, lossR(r._1)),
          r._2._2))

        val sWs = wprs.map(_._2._1).sum
        val mWs = wprs.map(_._2._1).max
        val nprs = wprs.map(r => r._1 ->(
          r._2._1 / mWs,
          r._2._1 / sWs))

        getDocument(nprs, ndocs, nacc)
      }
    }

    getDocument(prs, allDs, Set())
  }

}
