package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}
import at.ac.tuwien.ifs.utils.Profiler

import scala.annotation.tailrec

/**
  * Multi-Armed Bandits Based Pool
  * Created by aldo on 25/07/16.
  */
class BordaBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize,lRuns, gT) {

  override def getName = BordaBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToBordaBased(poolSize, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = BordaBasedPool.getPooledDocuments(estimatedNDs, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = BordaBasedPool(poolSize, lRuns, gT)

}

object BordaBasedPool {

  def getName(poolSize:Int) = "bordabased_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new BordaBasedPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], lRuns: List[Runs], qRels:QRels)(topicId: Int): Set[Document] = {

    def rank(rr:RunRecord, length:Int):Float = if(rr != null) -rr.rank else -length

    NonAdaptiveBasedPool.getPooledAlsoNullDocumentsWithSum(rank, nDs, lRuns, qRels)(topicId)
  }

}
