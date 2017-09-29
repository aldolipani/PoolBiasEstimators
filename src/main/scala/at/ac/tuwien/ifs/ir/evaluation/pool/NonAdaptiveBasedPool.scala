package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

import scala.util.Random

/**
  * Created by aldo on 22/05/2017.
  */
object NonAdaptiveBasedPool {

  val rnd = new Random(1234)

  def getPooledDocumentsWithSum(weight: (RunRecord) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    def sum(seq: Seq[Float]) = seq.sum

    getPooledDocumentsWithStat(weight, sum, nDs, pRuns, qRels)(topicId)
  }

  def getPooledDocumentsWithMax(weight: (RunRecord) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    def sum(seq: Seq[Float]) = seq.max

    getPooledDocumentsWithStat(weight, sum, nDs, pRuns, qRels)(topicId)
  }

  def getPooledDocumentsWithStat(weight: (RunRecord) => Float, stat: (Seq[Float]) => Float, nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val lRun = lRuns.map(_.selectByTopicIdOrNil(topicId))

    lRun.flatMap(run => {
      run.runRecords.map(rr => (rr.document, rr))
    }).groupBy(_._1).mapValues(vs => stat(vs.map(e => weight(e._2))) + rnd.nextDouble() / (lRuns.size * qRels.size))
      .toList.sortBy(-_._2).take(nDs(topicId)).map(_._1).toSet
  }

  /**
    * Implements getPooledAlsoNullDocumentsWithStat with Sum
    *
    * @param weight
    * @param nDs
    * @param pRuns
    * @param qRels
    * @param topicId
    * @return
    */
  def getPooledAlsoNullDocumentsWithSum(weight: (RunRecord, Int) => Float, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    def sum(seq: Seq[Float]) = seq.sum

    getPooledAlsoNullDocumentsWithStat(weight, sum, nDs, pRuns, qRels)(topicId)
  }

  /**
    *
    * @param weight
    * @param stat
    * @param nDs
    * @param lRuns
    * @param qRels
    * @param topicId
    * @return
    */
  def getPooledAlsoNullDocumentsWithStat(weight: (RunRecord, Int) => Float, stat: (Seq[Float]) => Float, nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val lRun = lRuns.map(_.selectByTopicIdOrNil(topicId))

    val docs = lRun.flatMap(_.runRecords.map(_.document))

    lRun.flatMap(run => {
      val runSize = run.runRecords.size
      docs.map(doc => doc -> (null, runSize)).toMap ++
        run.runRecords.map(rr => rr.document -> (rr, runSize))
    }).groupBy(_._1).mapValues(vs => {
      stat(vs.map(e => weight(e._2._1, e._2._2))) +
        rnd.nextDouble() / (lRuns.size * qRels.size)
    }).toList.sortBy(-_._2).take(nDs(topicId)).map(_._1).toSet
  }

}
