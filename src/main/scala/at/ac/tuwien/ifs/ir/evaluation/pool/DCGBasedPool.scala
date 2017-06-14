package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

import scala.collection.mutable.MultiMap
import scala.util.Random

/**
  * Created by aldo on 03/09/16.
  */
class DCGBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = DCGBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToDCGBased(poolSize, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = DCGBasedPool.getPooledDocuments(estimatedNDs, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = DCGBasedPool(poolSize, lRuns, gT)

}

object DCGBasedPool {

  def getName(poolSize:Int) = "DCGBased_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new DCGBasedPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def dcgw(rr: RunRecord): Float =
      (Math.log(2)/Math.log(rr.rank + 1)).toFloat

    NonAdaptiveBasedPool.getPooledDocumentsWithSum(dcgw, nDs, pRuns, qRels)(topicId)
  }

}
