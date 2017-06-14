package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._

import scala.util.Random

/**
  * Created by aldo on 25/05/2017.
  */
class DepthNHedgeBasedPool(depth: Int, beta: Double, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = DepthNHedgeBasedPool.getName(depth, beta, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToDepthNHedgeBased(depth, beta, poolSize, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = DepthNHedgeBasedPool.getPooledDocuments(depth, beta, estimatedNDs, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = DepthNHedgeBasedPool(depth, beta, poolSize, lRuns, gT)

}

object DepthNHedgeBasedPool {

  val rnd = new Random(1234);

  def apply(depth: Int, beta: Double, poolSize: Int, lRuns: List[Runs], gT: QRels) = new DepthNHedgeBasedPool(depth, beta, poolSize, lRuns, gT)

  def getName(depth: Int, beta: Double, poolSize: Int) = "hedgebased_" + depth + ":" + beta + ":" + poolSize

  def getPooledDocuments(depth: Int, beta: Double, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val oRs: Map[Int, Map[Document, RunRecord]] = FixedSizePool.getNonNullLRuns(topicId, pRuns)
      .zipWithIndex.map(r => (r._2 ->
      r._1.selectByTopicId(topicId).runRecords.map(rr => (rr.document ->
        rr)).toMap)).toMap

    val oDocs: Set[Document] = FixedSizePool.getNonNullLRuns(topicId, pRuns)
      .flatMap(_.selectByTopicId(topicId).runRecords.map(_.document)).toSet

    //average run loss for shorter runs
    val aRLs: Map[Int, Double] = oRs.map(r => (r._1, {
      Math.log(oDocs.size) - (r._2.size until oDocs.size).map(i => Math.log(i)).sum / (oDocs.size - r._2.size)
    }))

    def g(d: Document, r: Int) =
      if (oRs(r).contains(d))
        Math.log(oDocs.size) - Math.log(oRs(r)(d).rank)
      else
        aRLs(r)

    def getDocuments(docs: Set[Document], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set()): Set[Document] = {
      if (acc.size == nDs(topicId) || docs.isEmpty)
        acc
      else {
        // selection doc
        val rLs: Map[Int, Double] = // run losses
          oRs.map(r => (r._1, acc.withFilter(d => cQRel.getRel(d) >= 0).map(d => {
            val rel = if (cQRel.getRel(d) > 0) 1 else 0
            0.5d * Math.pow(-1d, rel) * g(d, r._1)
          }).sum))

        val nRLsNum = rLs.map(r => (r._1, { // normalize run losses
          Math.pow(beta, r._2)
        }))
        val nRLsDen = nRLsNum.values.sum
        val nRLs = nRLsNum.map(r => (r._1, {
          r._2 / nRLsDen
        }))

        // select document
        val doc = FixedSizePool.getNonDeterministicMaxObject(
          docs.map(d => (d, {
            oRs.map(r => nRLs(r._1) * g(d, r._1)).sum
          })).toList)

        val rel = if (qRels.getRel(topicId, doc) > 0) qRels.getRel(topicId, doc) else 0

        getDocuments(
          docs - doc,
          new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel)),
          acc + doc)
      }
    }

    val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
    if (docs.size > nDs(topicId)) {
      throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
    }
    val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
    getDocuments(oDocs -- docs, qRel, docs)
  }
}

