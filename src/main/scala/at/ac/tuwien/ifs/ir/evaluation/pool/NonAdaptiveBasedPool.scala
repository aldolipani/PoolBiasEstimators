package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

import scala.util.Random

/**
  * Created by aldo on 22/05/2017.
  */
object NonAdaptiveBasedPool {

  val rnd = new Random(1234)

  def getPooledDocumentsWithSum(weight:(RunRecord) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    def sum(seq:Seq[Float]) = seq.sum

    getPooledDocumentsWithStat(weight, sum, nDs, pRuns, qRels)(topicId)
  }

  def getPooledDocumentsWithStat(weight:(RunRecord) => Float, stat:(Seq[Float]) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    pRuns.filter(_.selectByTopicId(topicId) != null).flatMap(r =>{
      r.selectByTopicId(topicId).runRecords.map(rr => (rr.document, rr))
    }).groupBy(_._1).mapValues(vs => stat(vs.map(e => weight(e._2))) + rnd.nextDouble()/(pRuns.size * qRels.size))
      .toList.sortBy(- _._2).take(nDs(topicId)).map(_._1).toSet
  }

  def getPooledAlsoNullDocumentsWithSum(weight:(RunRecord, Int) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    def sum(seq:Seq[Float]) = seq.sum

    getPooledAlsoNullDocumentsWithStat(weight, sum, nDs, pRuns, qRels)(topicId)
  }

  def getPooledAlsoNullDocumentsWithStat(weight:(RunRecord, Int) => Float, stat:(Seq[Float]) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val docs = pRuns.filter(_.selectByTopicId(topicId) != null).flatMap(r =>{
      r.selectByTopicId(topicId).runRecords.map(rr => rr.document)})

    pRuns.filter(_.selectByTopicId(topicId) != null).flatMap(r =>{
      val rrs = r.selectByTopicId(topicId).runRecords
      docs.map(doc => (doc -> (null, rrs.size))).toMap ++ rrs.map(rr => (rr.document -> (rr, rrs.size))).toMap
    }).groupBy(_._1).mapValues(vs => stat(vs.map(e => weight(e._2._1, e._2._2))) + rnd.nextDouble()/(pRuns.size * qRels.size))
      .toList.sortBy(- _._2).take(nDs(topicId)).map(_._1).toSet
  }

}
