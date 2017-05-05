package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.annotation.tailrec

/**
  * Created by aldo on 29/03/16.
  */

class RRFBasedPool(method: String, k: Int, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = RRFBasedPool.getName(method, k, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToRRFBased(method, k, poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = RRFBasedPool.getPooledDocuments(method, k, topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = RRFBasedPool(method, k, poolSize, lRuns, gT)

}

object RRFBasedPool {

  def getName(method:String, k:Int, poolSize:Int) = "RRFBased_" + method + ":" + k + ":" + poolSize

  def apply(m: String, k: Int, pD: Int, lRuns: List[Runs], gT: QRels) = new RRFBasedPool(m, k, pD, lRuns, gT)

  def getPooledDocuments(m: String, k: Int, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    lazy val lds: Map[String, Map[String, Double]] =
      qRuns.flatMap(r =>
        r.runs.head.runRecords.map(d =>
          (d.document.id, (r.id, d.rank)))).groupBy(_._1).map(e =>
        (e._1, e._2.map(_._2).toMap.mapValues(rrfw(_))))

    def rrfBasedPooling(): Set[Document] =
      lds.mapValues(_.values.max).toList.sortBy(-_._2).take(nDs(topicId)).map(d => new Document(d._1)).toSet

    def rrfBasedA(): Set[Document] =
      lds.mapValues(_.values.sum).toList.sortBy(-_._2).take(nDs(topicId)).map(d => new Document(d._1)).toSet

    def rrfw(rank: Int): Double = 1d/(k + rank)

    if (m == "pooling")
      rrfBasedPooling()
    else if (m == "a")
      rrfBasedA()
    else
      throw new Exception("Method not found")
  }

}
