package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 10/09/15.
 */
class DepthNPool(val n: Int, lRuns: List[Runs], gT: QRels) extends Pool(lRuns, gT) {

  override def getName = DepthNPool.getName(n)

  override lazy val qRels:QRels = PoolConverter.repoolToDepthN(n, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = DepthNPool.getPooledDocuments(n, lRuns)(topicId)

  override def getNewInstance(lRuns: List[Runs]):Pool = DepthNPool(n, lRuns, gT)
}

object DepthNPool {

  def getName(n:Int) = "depth_" + n

  def apply(n:Int, lRuns:List[Runs], qRels:QRels) = new DepthNPool(n, lRuns, qRels)

  def getPooledDocuments(n: Int, lRuns: List[Runs])(topicId: Int): Set[Document] =
    lRuns.flatMap(l => {
      if (l.selectByTopicId(topicId) == null)
        Nil
      else
        l.selectByTopicId(topicId).runRecords.take(n).map(_.document)
    }).toSet
}