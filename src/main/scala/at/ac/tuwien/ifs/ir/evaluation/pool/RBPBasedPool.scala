package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._

import scala.annotation.tailrec

/**
  * Created by aldo on 29/03/16.
  * Implementation of the pooling stragtegies studied in the following paper:
  * Moffat, Alistair, William Webber, and Justin Zobel. "Strategic system
  * comparisons via targeted relevance judgments." SIGIR. ACM, 2007.
  */

class RBPBasedPool(method: String, p: Double, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName: String = RBPBasedPool.getName(method, p, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToRBPBased(method, p, poolSize, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]): Pool = RBPBasedPool(method, p, poolSize, lRuns, gT)

}

object RBPBasedPool {

  def apply(m: String, p: Double, pD: Int, lRuns: List[Runs], gT: QRels) = new RBPBasedPool(m, p, pD, lRuns, gT)

  def getName(method: String, p: Double, poolSize: Int): String = "rbpbased_" + method + ":" + p + ":" + poolSize

  def getPooledDocuments(m: String, p: Double, nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val qRuns = lRuns.map(runs => runs.getRunsTopic(topicId))

    def rbpW(rr: RunRecord): Float =
      if (p == 1d)
        1f
      else
        ((1d - p) * Math.pow(p, rr.rank - 1)).toFloat

    def rbpB(runs: Runs, acc: Set[Document]) = {
      val run = runs.selectByTopicIdOrNil(topicId)
      run.runRecords.filter(rr => acc.contains(rr.document)).map(rr =>
        if (qRels.getRel(topicId, rr.document) > 0)
          rbpW(rr)
        else
          0d).sum
    }

    def rbpR(runs: Runs, acc: Set[Document]) = 1d - {
      val run = runs.selectByTopicIdOrNil(topicId)
      run.runRecords.filter(rr => acc.contains(rr.document)).map(rr => rbpW(rr)).sum
    }

    // documentID -> run -> rbpW
    lazy val lds: Map[Document, Map[Runs, Float]] =
      qRuns.flatMap(runs =>
        runs.runs.head.runRecords.map(runRecord => (runRecord.document, (runs, runRecord))))
        .groupBy(_._1).map(e0 => (e0._1, e0._2.map(e1 => e1._2._1 -> rbpW(e1._2._2)).toMap))

    def rbpBasedA(): Set[Document] =
      NonAdaptiveBasedPool.getPooledDocumentsWithSum(rbpW, nDs, lRuns, qRels)(topicId)

    def rbpBasedB(cds: Map[Document, Map[Runs, Float]] = lds, acc: Set[Document] = Set()): Set[Document] = {
      if (acc.size == nDs(topicId))
        acc
      else {
        val d =
          cds.map(d =>
            (d._1, d._2.map(e => e._2 * rbpR(e._1, acc)).sum)).maxBy(_._2)._1

        rbpBasedB(cds - d, acc + d)
      }
    }

    def rbpBasedC(cds: Map[Document, Map[Runs, Float]] = lds, acc: Set[Document] = Set()): Set[Document] = {
      if (acc.size == nDs(topicId))
        acc
      else {
        val d =
          cds.map(d =>
            (d._1, d._2.map(e => {
              val residual = rbpR(e._1, acc)
              e._2 * residual * Math.pow(rbpB(e._1, acc) + residual / 2f, 3)
            }).sum)).maxBy(_._2)._1

        rbpBasedC(cds - d, acc + d)
      }
    }

    if (m == "a")
      rbpBasedA()
    else if (m == "b") {
      rbpBasedB()
    } else if (m == "c")
      rbpBasedC()
    else
      throw new Exception("Method not found")
  }

}
