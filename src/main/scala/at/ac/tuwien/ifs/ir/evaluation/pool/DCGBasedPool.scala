package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
  * Created by aldo on 03/09/16.
  */


class DCGBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = DCGBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToDCGBased(poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = DCGBasedPool.getPooledDocuments(topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = DCGBasedPool(poolSize, lRuns, gT)

}

object DCGBasedPool {

  def getName(poolSize:Int) = "DCGBased_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new DCGBasedPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    lazy val lds: Map[String, Map[String, Double]] =
      qRuns.flatMap(r =>
        r.runs.head.runRecords.map(d =>
          (d.document.id, (r.id, d.rank)))).groupBy(_._1).map(e =>
        (e._1, e._2.map(_._2).toMap.mapValues(dcgw(_))))

    def dcgBasedA(): Set[Document] =
      lds.mapValues(_.values.sum).toList.sortBy(-_._2).take(nDs(topicId)).map(d => new Document(d._1)).toSet

    def dcgw(rank: Int): Double =
      if(rank == 0)
        1d
      else
        Math.log(2)/Math.log(rank)

    dcgBasedA()
  }

}
