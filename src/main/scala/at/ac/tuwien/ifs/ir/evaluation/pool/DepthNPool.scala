package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 10/09/15.
 */
class DepthNPool(val n:Int, lRuns: List[Runs], qRels: QRels) extends Pool(lRuns, qRels) {

  def getPooledDocuments(topicId: Int): Set[Document] = DepthNPool.getPooledDocuments(topicId)(n, lRuns)

}

object DepthNPool{

  def getPooledDocuments(topicId:Int)(n:Int, lRuns:List[Runs]):Set[Document] =
    lRuns.flatMap(l => {
      if (l.selectByTopicId(topicId) == null)
        Nil
      else
        l.selectByTopicId(topicId).runRecords.take(n).map(_.document)
    }).toSet

}