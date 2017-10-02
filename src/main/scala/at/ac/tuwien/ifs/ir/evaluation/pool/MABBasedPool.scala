package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._
import org.apache.commons.math3.distribution.BetaDistribution

import scala.annotation.tailrec
import scala.util.Random

/**
  * Multi-Armed Bandits Based Pool
  * Created by aldo on 25/07/16.
  */
class MABBasedPool(m: String, c1: Double, c2: Double, sizePool: Int, lRuns: List[Runs], gT: QRels, restore: Boolean, nDs: Map[Int, Int]) extends FixedSizePool(sizePool, lRuns, gT, nDs) {

  override lazy val qRels: QRels = PoolConverter.repoolToMABBased(m, c1, c2, sizePool, lRuns, gT, nDs, restore)

  override def getName: String = MABBasedPool.getName(m, c1, c2, sizePool)

  override def getNewInstance(lRuns: List[Runs]): Pool = MABBasedPool(m, c1, c2, sizePool, lRuns, gT, restore,
    FixedSizePool.findTopicSizes(nDs.values.sum, lRuns, qRels))
}

object MABBasedPool {

  val rnd = new Random(1234)

  def getName(m: String, c1: Double, c2: Double, sizePool: Int): String = "MABBased_" + m + ":" + c1 + ":" + c2 + ":" + ":" + sizePool

  def apply(m: String, c1: Double, c2: Double, sizePool: Int, lRuns: List[Runs], gT: QRels, restore: Boolean, nDs: Map[Int, Int]) = new MABBasedPool(m, c1, c2, sizePool, lRuns, gT, restore, nDs)

  def getPooledDocuments(m: String, c1: Double, c2: Double, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels, restore: Boolean)(topicId: Int): Set[Document] = {

    val oRs: Map[Int, List[Document]] = FixedSizePool.getSimplifiedLRuns(topicId, pRuns)

    def random(): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr = rnd.shuffle(rs.keys.toList).head
          val doc = rs(nr).head

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            acc + doc)
        }
      }

      getDocuments(oRs)
    }

    def p(r: List[Document], qRel: QRel): Double = {
      if (r.isEmpty)
        0.5d
      else
        r.count(d => qRel.getRel(d) >= 1) / r.size
    }

    def greedy(c1: Double, c2: Double): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set(), n: Int = 1): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr =
            if (rnd.nextDouble() < Math.min(1d, (c1 * oRs.size) / (c2 * c2 * n)))
              rnd.shuffle(rs.keys.toList).head
            else
              FixedSizePool.getNonDeterministicMaxObject(
                oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
                  val ds = r._2.take(oRs(r._1).size - rs(r._1).size)
                  p(ds, cQRel)
                })).toList)

          // judge doc
          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if (cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc,
            n + 1)
        }
      }

      getDocuments(oRs)
    }

    def ucb1Tuned(): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set(), n: Int = 0): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr =
            if (!rs.forall(r => oRs(r._1).size - r._2.size > 0))
              rs.filter(r => oRs(r._1).size - r._2.size == 0).head._1
            else
              FixedSizePool.getNonDeterministicMaxObject(
                oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
                  // get played documents
                  val ds = r._2.take(oRs(r._1).size - rs(r._1).size)
                  // get mean and variance
                  val mu = p(ds, cQRel)
                  val va = mu * (1d - mu)
                  // compute and return ucb1-tuned weight
                  mu + Math.sqrt(Math.log(n) / ds.size * Math.min(0.25d, va + Math.sqrt(2d * Math.log(n) / ds.size)))
                })).toList)

          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if (cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc,
            n + 1)
        }
      }

      getDocuments(oRs)
    }

    def beta(): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr = FixedSizePool.getNonDeterministicMaxObject(
            oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
              val ds = r._2
              new BetaDistribution(
                1d + ds.count(d => cQRel.getRel(d) > 0),
                1d + ds.count(d => cQRel.getRel(d) == 0)).sample()
            })).toList)

          // judge doc
          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if (cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc)
        }
      }

      getDocuments(oRs)
    }

    def maxMean(): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size >= nDs(topicId) || rs.isEmpty) {
          acc
        } else {
          // select arm
          val nr = FixedSizePool.getNonDeterministicMaxObject(
            oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
              val ds = r._2
              (1d + ds.count(d => cQRel.getRel(d) > 0)) / (2d + ds.count(d => cQRel.getRel(d) >= 0))
            })).toList)

          // judge doc
          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if (cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc)
        }
      }

      if (restore) {
        val nQRel = qRels.getTopicQRels(topicId).qRels.head
        val docs = oRs.flatMap(e => oRs(e._1).takeWhile(doc => nQRel.getRel(doc) >= 0)).toSet
        val nORs = oRs.map(e => e._1 -> oRs(e._1).dropWhile(doc => nQRel.getRel(doc) >= 0)).filter(_._2.nonEmpty)
        getDocuments(nORs, nQRel, docs)
      } else {
        getDocuments(oRs)
      }
    }

    if (m == "random")
      random()
    else if (m == "greedy")
      greedy(c1, c2)
    else if (m == "ucb1-tuned")
      ucb1Tuned()
    else if (m == "beta")
      beta()
    else if (m == "maxmean")
      maxMean()
    else
      throw new Exception("Method not found")
  }

}
