package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

class FairTakeNPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName: String = FairTakeNPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToFairTakeN(poolSize, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]): Pool = FairTakeNPool(poolSize, lRuns, gT)

}

object FairTakeNPool {

  def getName(poolSize: Int): String = "FairTake_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new TakeNPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def rank(rr: RunRecord): Float = -rr.rank

    NonAdaptiveBasedPool.getPooledDocumentsWithMax(rank, nDs, lRuns, qRels)(topicId)
  }

}
