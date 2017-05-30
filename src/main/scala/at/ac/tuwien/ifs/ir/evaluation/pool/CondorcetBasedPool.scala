package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by aldo on 04/08/16.
  */
class CondorcetBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = CondorcetBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToCondorcetBased(poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = CondorcetBasedPool.getPooledDocuments(topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = CondorcetBasedPool(poolSize, lRuns, gT)

}

object CondorcetBasedPool {

  val rnd = new Random(1234)

  def getName(poolSize:Int) = "condorcetbased:" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new CondorcetBasedPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], rlRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val lRuns: List[Runs] = FixedSizePool.getNonNullLRuns(topicId, rlRuns)

    val sDocs: Set[Document] = lRuns.flatMap(_.selectByTopicId(topicId).runRecords.map(_.document)).toSet

    def compare(d_0: Document, d_1: Document): Int =
      lRuns.map(runs => {
        val rr_0 = runs.selectByTopicId(topicId).getByDocument(d_0)
        val rr_1 = runs.selectByTopicId(topicId).getByDocument(d_1)
        if(rr_0 != null && rr_1 == null || (rr_0 != null && rr_1 != null &&
          rr_0.rank < rr_1.rank))
          +1
        else if(rr_0 == null && rr_1 != null || (rr_0 != null && rr_1 != null &&
          rr_0.rank > rr_1.rank))
          -1
        else
          0
        }).sum

    def counting(d_0: Document): Int = {
      sDocs.count(d_1 => compare(d_0, d_1) > 0)
    }

    sDocs.map(d => (d, counting(d) + rnd.nextDouble()/lRuns.size))
      .toList.sortBy(- _._2).take(nDs(topicId)).map(_._1).toSet
  }

}
