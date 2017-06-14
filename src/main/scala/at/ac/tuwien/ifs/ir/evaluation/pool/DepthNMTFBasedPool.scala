package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._

import scala.annotation.tailrec

/**
  * Created by aldo on 29/03/16.
  */

class DepthNMTFBasedPool(depth: Int, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = DepthNMTFBasedPool.getName(depth, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToDepthNMTFBased(depth, poolSize, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = DepthNMTFBasedPool.getPooledDocuments(depth, estimatedNDs, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = DepthNMTFBasedPool(depth, poolSize, lRuns, gT)

}

object DepthNMTFBasedPool {

  def apply(depth: Int, poolSize: Int, lRuns: List[Runs], gT: QRels) = new DepthNMTFBasedPool(depth, poolSize, lRuns, gT)

  def getName(depth: Int, poolSize: Int) = "depthnmtfbased_" + depth + ":" + poolSize

  def getPooledDocuments(depth: Int, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val oRs = FixedSizePool.getSimplifiedLRuns(topicId, pRuns)

    @tailrec def getDocument(onr: Int, onQRel: QRel, rs: Map[Int, List[Document]], acc: Set[Document] = Set()): Set[Document] = {
      if (acc.size == nDs(topicId) || rs.isEmpty) {
        acc
      } else {
        //select arm
        val nr:Int =
          if (onr < 0)
            FixedSizePool.getNonDeterministicMaxObject(
              oRs.filter(r => rs.contains(r._1)).map(e => (e._1,
                -e._2.take(oRs(e._1).size - rs(e._1).size).count(d => onQRel.getRel(d) == 0)
              )).toList)
          else
            onr

        //judge document
        val doc = rs(nr).head
        val rel = if (onQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else onQRel.getRel(doc)

        getDocument(
          if (rel > 0 && rs(nr).size > 1) nr else -1,
          new QRel(onQRel.id, onQRel.qrelRecords :+ QRelRecord("Q0", doc, rel)),
          FixedSizePool.updateSimplifiedLRuns(rs, nr),
          acc + doc
        )
      }
    }

    val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
    if (docs.size > nDs(topicId)) {
      throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
    }
    val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
    val nlrs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
    getDocument(-1, qRel, nlrs, docs)
  }

}
