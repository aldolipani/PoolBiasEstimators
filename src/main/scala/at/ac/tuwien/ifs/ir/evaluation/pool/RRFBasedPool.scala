package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

import scala.annotation.tailrec

/**
  * Created by aldo on 29/03/16.
  */

class RRFBasedPool(k: Int, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = RRFBasedPool.getName(k, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToRRFBased(k, poolSize, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = RRFBasedPool.getPooledDocuments(k, estimatedNDs, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = RRFBasedPool(k, poolSize, lRuns, gT)

}

object RRFBasedPool {

  def getName(k:Int, poolSize:Int) = "RRFBased_" + k + ":" + poolSize

  def apply(k: Int, pD: Int, lRuns: List[Runs], gT: QRels) = new RRFBasedPool(k, pD, lRuns, gT)

  def getPooledDocuments(k: Int, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def rrfw(rr: RunRecord): Float = (1d/(k + rr.rank)).toFloat

    NonAdaptiveBasedPool.getPooledDocumentsWithSum(rrfw, nDs, pRuns, qRels)(topicId)
  }

}
