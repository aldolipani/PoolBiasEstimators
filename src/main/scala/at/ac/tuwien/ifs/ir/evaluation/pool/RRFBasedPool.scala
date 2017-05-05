package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
  * Created by aldo on 29/03/16.
  */

class RRFBasedPool(m: String, k: Int, pD: Int, lRuns: List[Runs], gT: QRels) extends Pool(lRuns, gT) {

  override lazy val qRels: QRels = PoolConverter.repoolToRBPBased(m, k, pD, lRuns, gT)

  def findNDs(nDs: Map[Int, Int], n: Int = 0): Map[Int, Int] = {
    //println(n)
    if (n > pD) {
      val nNDs = (1 to n - pD).foldRight(nDs)((n, tDs) => tDs + {
        val nD = tDs.toList.sortBy(_._2).head
        (nD._1, nD._2 - 1)
      })
      nNDs
    } else if (n == 0){
      val avgPD = pD/gT.topicIds.size
      val mPD = nDs.keys.map(tId => (
        Math.min(lRuns.flatMap(run =>
          if (run.selectByTopicId(tId) != null)
            run.selectByTopicId(tId).runRecords.map(_.document.id)
          else
            Set()).size, avgPD))).min
      val nNDs =
        nDs.map(e => e._1 -> mPD)
      findNDs(
        nNDs,
        nNDs.values.sum
      )
    } else {
      val nNDs =
        nDs.map(e => e._1 -> (
          if (lRuns.flatMap(run =>
            if (run.selectByTopicId(e._1) != null)
              run.selectByTopicId(e._1).runRecords.map(_.document.id)
            else
              Set()
          ).toSet.size > e._2)
            e._2 + 1
          else
            e._2))
      findNDs(
        nNDs,
        nNDs.values.sum
      )
    }
  }

  lazy val nDs = findNDs(gT.topicIds.map(_ -> 0).toMap)

  override def getPooledDocuments(topicId: Int): Set[Document] = RRFBasedPool.getPooledDocuments(topicId)(m, k, nDs, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]): Pool = RRFBasedPool(m, k, pD, lRuns, gT)

}

object RRFBasedPool {

  def apply(m: String, k: Int, pD: Int, lRuns: List[Runs], gT: QRels) = new RRFBasedPool(m, k, pD, lRuns, gT)

  def getPooledDocuments(topicId: Int)(m: String, k: Int, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels): Set[Document] = {
    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    lazy val dt: Map[String, Set[Int]] =
      qRuns.flatMap(_.runs.flatMap(t =>
        t.runRecords.map(d => (d.document.id, t.id)))).groupBy(_._1).map(e => (e._1, e._2.map(_._2).toSet))

    lazy val lds: Map[String, Map[String, Double]] =
      qRuns.flatMap(r =>
        r.runs.head.runRecords.map(d =>
          (d.document.id, (r.id, d.rank)))).groupBy(_._1).map(e =>
        (e._1, e._2.map(_._2).toMap.mapValues(rffw(_))))

    lazy val mRuns = qRuns.map(runs => runs.id -> runs).toMap

    def rbpBasedPooling(): Set[Document] =
      lds.mapValues(_.values.max).toList.sortBy(-_._2).take(nDs(topicId)).map(d => new Document(d._1)).toSet

    def rbpBasedA(): Set[Document] =
      lds.mapValues(_.values.sum).toList.sortBy(-_._2).take(nDs(topicId)).map(d => new Document(d._1)).toSet

    def rffw(rank: Int): Double = 1d/(k + rank)

    def rff(runId: String, topicId: Int, acc: Set[Document]) = {
      val run = mRuns(runId).selectByTopicId(topicId)
      run.runRecords.filter(rr => acc.contains(rr.document)).map(rr =>
        if(qRels.getRel(topicId, rr.document.id) > 0) rffw(rr.rank) else 0d).sum
    }

    if (m == "pooling")
      rbpBasedPooling()
    else if (m == "a")
      rbpBasedA()
    else
      throw new Exception("Method not found")
  }

}
