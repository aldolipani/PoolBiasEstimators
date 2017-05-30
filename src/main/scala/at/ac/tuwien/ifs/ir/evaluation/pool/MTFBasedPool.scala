package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by aldo on 29/03/16.
  */

class MTFBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = MTFBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToMTFBased(poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = MTFBasedPool.getPooledDocuments(topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = MTFBasedPool(poolSize, lRuns, gT)

}

object MTFBasedPool {

  def apply(poolSize: Int, lRuns: List[Runs], gT: QRels) = new MTFBasedPool(poolSize, lRuns, gT)

  def getName(poolSize: Int) = "mtfbased_" + poolSize

  def getPooledDocuments(nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val oRs: Map[Int, List[Document]] = FixedSizePool.getSimplifiedLRuns(topicId, pRuns)

    @tailrec def getDocument(onr: Int, cQRel: QRel, rs: Map[Int, List[Document]], acc: Set[Document] = Set()): Set[Document] = {
      if (acc.size == nDs(topicId) || rs.isEmpty) {
        acc
      } else {
        //select arm
        val nr =
          if (onr < 0)
            FixedSizePool.getNonDeterministicMaxObject(
              oRs.filter(r => rs.contains(r._1)).map(r => (r._1,
                -r._2.take(oRs(r._1).size - rs(r._1).size).count(d => cQRel.getRel(d) == 0)
              )).toList)
          else
            onr

        //judge document
        val doc = rs(nr).head
        val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

        getDocument(
          if (rel > 0 && rs(nr).size > 1) nr else -1,
          if (cQRel.getRel(doc) < 0)
            new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
          else
            cQRel,
          FixedSizePool.updateSimplifiedLRuns(rs, nr),
          acc + doc
        )
      }
    }

    getDocument(-1, new QRel(topicId, List()), oRs)
  }

}
