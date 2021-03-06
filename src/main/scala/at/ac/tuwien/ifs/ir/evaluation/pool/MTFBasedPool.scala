package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by aldo on 29/03/16.
  */

class MTFBasedPool(m: String, par: Map[String, Int], poolSize: Int, lRuns: List[Runs], gT: QRels, nDs: Map[Int, Int]) extends FixedSizePool(poolSize, lRuns, gT, nDs) {

  override def getName: String = MTFBasedPool.getName(m, par, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToMTFBased(m, par, poolSize, lRuns, gT, nDs)

  override def getNewInstance(lRuns: List[Runs]): Pool =
    MTFBasedPool(m, par, poolSize, lRuns, gT,
      FixedSizePool.findTopicSizes(nDs.values.sum, lRuns, qRels))

}

object MTFBasedPool {

  def apply(m: String, par: Map[String, Int], poolSize: Int, lRuns: List[Runs], gT: QRels, nDs: Map[Int, Int]) = new MTFBasedPool(m, par, poolSize, lRuns, gT, nDs)

  def getName(m: String, par: Map[String, Int], poolSize: Int): String = "mtfbased_" + m + ":" + par.toList.sortBy(_._1).map(_._2).mkString(":") + ":" + poolSize

  def getPooledDocuments(m: String, par: Map[String, Int], nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val oRs: Map[Int, List[Document]] = FixedSizePool.getSimplifiedLRuns(topicId, pRuns)

    def standard(): Set[Document] = {
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

    def maxMean(maxObservedDepth: Int = -1): Set[Document] = {

      @tailrec def getDocument(onr: Int, cQRel: QRel, rs: Map[Int, List[Document]], acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty) {
          acc
        } else {
          //select arm
          val nr =
            if (onr < 0)
              FixedSizePool.getNonDeterministicMaxObject(
                oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
                  if (maxObservedDepth < 0) {
                    val wins = 1d + r._2.count(d => cQRel.getRel(d) > 0)
                    val all = 2d + r._2.count(d => cQRel.getRel(d) >= 0)
                    wins / all
                  } else {
                    val wins = 1d + r._2.take(maxObservedDepth).count(d => cQRel.getRel(d) > 0)
                    val all = 2d + r._2.take(maxObservedDepth).count(d => cQRel.getRel(d) >= 0)
                    wins / all
                  }
                })).toList)
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

    if (m == "standard") {
      standard()
    } else if (m == "maxmean") {
      val maxObservedDepth =
        par.getOrElse("maxObservedDepth", -1)
      maxMean(maxObservedDepth)
    } else {
      throw new Exception("Method not found")
    }

  }

}
