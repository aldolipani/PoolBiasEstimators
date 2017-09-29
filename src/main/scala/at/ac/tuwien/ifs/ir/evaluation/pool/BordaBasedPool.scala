package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

class BordaBasedPool(collectionSize:Int, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override lazy val qRels: QRels = PoolConverter.repoolToBordaBased(collectionSize, poolSize, lRuns, gT)

  override def getName: String = BordaBasedPool.getName(poolSize)

  override def getNewInstance(lRuns: List[Runs]): Pool = BordaBasedPool(collectionSize, poolSize, lRuns, gT)

}

object BordaBasedPool {

  def getName(poolSize: Int): String = "bordabased_" + poolSize

  def apply(collectionSize:Int, pD: Int, lRuns: List[Runs], gT: QRels) = new BordaBasedPool(collectionSize,pD, lRuns, gT)

  def getPooledDocuments(collectionSize:Int = 0, nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val lRun = lRuns.map(_.selectByTopicIdOrNil(topicId))

    val nDocs =
      if(collectionSize == 0)
        lRun.flatMap(_.runRecords.map(_.document)).size
      else
        collectionSize

    def count(rr: RunRecord, runSize: Int): Float =
      if (rr != null)
        - rr.rank
      else
        - nDocs/2f - (runSize + 1f)/2f

    NonAdaptiveBasedPool.getPooledAlsoNullDocumentsWithSum(count, nDs, lRuns, qRels)(topicId)
  }

}