package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}
import at.ac.tuwien.ifs.utils.Profiler

import scala.annotation.tailrec

/**
  * Multi-Armed Bandits Based Pool
  * Created by aldo on 25/07/16.
  */
class BordaBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize,lRuns, gT) {

  override def getName = BordaBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToBordaBased(poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = BordaBasedPool.getPooledDocuments(topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = BordaBasedPool(poolSize, lRuns, gT)

}

object BordaBasedPool {

  def getName(poolSize:Int) = "bordabased_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new BordaBasedPool(pD, lRuns, gT)

  def getPooledDocuments(topicSizes: Map[Int, Int], pRuns: List[Runs], qRels:QRels)(topicId: Int): Set[Document] = {
    val unknownDocument = new Document("unknown")

    // runId# -> documents
    val lrs: Map[String, List[Document]] =
      pRuns.filter(_.selectByTopicId(topicId) != null).map(run =>
        run.id -> run.selectByTopicId(topicId).runRecords.map(_.document)).toMap
    // documents
    val allDs: Set[Document] = lrs.values.flatten.toSet

    def sumInt(n: Int): Int = (n * (n + 1)) / 2

    def sumIntFromInt(n1: Int, n0: Int): Int = sumInt(n1) - sumInt(n0)

    val nlrs: Map[String, Map[Document, Double]] = lrs.map(rs =>
      (rs._1 -> {
        rs._2.zipWithIndex.map(d => d._1 -> (allDs.size - d._2).toDouble).toMap +
          (unknownDocument -> sumIntFromInt(allDs.size, allDs.size - rs._2.size).toDouble / (allDs.size - rs._2.size))}))

    allDs.map(d => (d ->
      nlrs.map(r => r._2.getOrElse(d, r._2(unknownDocument))).sum))
      .toList.sortBy(e => -e._2).take(topicSizes(topicId)).map(_._1).toSet
    }

}
