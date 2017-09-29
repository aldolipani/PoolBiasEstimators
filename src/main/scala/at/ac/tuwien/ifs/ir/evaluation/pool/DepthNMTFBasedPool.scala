package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._

import scala.annotation.tailrec

/**
  * Created by aldo on 29/03/16.
  */

class DepthNMTFBasedPool(m:String, par:Map[String, Int], depth: Int, poolSize: Int, lRuns: List[Runs], gT: QRels, nDs:Map[Int, Int]) extends FixedSizePool(poolSize, lRuns, gT, nDs) {

  override lazy val qRels: QRels = PoolConverter.repoolToDepthNMTFBased(m, par, depth, poolSize, lRuns, gT, nDs)

  override def getName = DepthNMTFBasedPool.getName(m, par, depth, poolSize)

  override def getNewInstance(lRuns: List[Runs]): Pool =
    DepthNMTFBasedPool(m, par, depth, poolSize, lRuns, gT,
      FixedSizePool.findTopicSizes(nDs.values.sum, lRuns, qRels))

}

object DepthNMTFBasedPool {

  def apply(m:String, par:Map[String, Int], depth: Int, poolSize: Int, lRuns: List[Runs], gT: QRels, nDs:Map[Int, Int]) = new DepthNMTFBasedPool(m, par, depth, poolSize, lRuns, gT, nDs)

  def getName(m:String, par:Map[String, Int], depth: Int, poolSize: Int) = "depthnmtfbased_" + m + ":" + par.toList.sortBy(_._1).map(_._2).mkString(":") + depth + ":" + poolSize

  def getPooledDocuments(m: String, par: Map[String, Int], depth: Int, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val oRs = FixedSizePool.getSimplifiedLRuns(topicId, pRuns)

    def standard(): Set[Document] = {
      @tailrec def getDocument(onr: Int, onQRel: QRel, rs: Map[Int, List[Document]], acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty) {
          acc
        } else {
          //select arm
          val nr: Int =
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

    def maxMean(maxObservedDepth: Int = -1): Set[Document] = {

      @tailrec def getDocument(onr: Int, cQRel: QRel, rs: Map[Int, List[Document]], acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size >= nDs(topicId) || rs.isEmpty) {
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
              new QRel(cQRel.id, cQRel.qrelRecords.filter(e => e.document != doc) :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            acc + doc
          )
        }
      }

      val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
      if (docs.size > nDs(topicId)) {
        throw new InstantiationException("The selected poolSize (" + docs.size + " > " + nDs(topicId) + " for topic " + topicId + ") is not sufficient for this pooling strategy")
      }
      val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
      val nlrs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
      getDocument(-1, qRel, nlrs, docs)
    }

    if (m == "standard") {
      standard()
    } else if (m == "maxmean") {
      val maxObservedDepth = par.getOrElse("maxObservedDepth", -1)
      maxMean(maxObservedDepth)
    } else {
      throw new Exception("Method not found")
    }
  }

}
