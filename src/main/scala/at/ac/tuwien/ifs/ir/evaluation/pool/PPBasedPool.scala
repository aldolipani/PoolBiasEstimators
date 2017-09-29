package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

/**
  * Created by aldo on 29/03/16.
  */

class PPBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName: String = PPBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToPPBased(poolSize, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]): Pool = PPBasedPool(poolSize, lRuns, gT)

}

object PPBasedPool {

  def getName(poolSize: Int): String = "PPBased_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new PPBasedPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def pp(rr: RunRecord): Float = 1f

    NonAdaptiveBasedPool.getPooledDocumentsWithSum(pp, nDs, pRuns, qRels)(topicId)
  }

}
