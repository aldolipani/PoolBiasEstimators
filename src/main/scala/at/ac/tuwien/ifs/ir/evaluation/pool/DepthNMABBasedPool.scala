package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model._
import org.apache.commons.math3.distribution.BetaDistribution

import scala.annotation.tailrec
import scala.util.Random

/**
  * Multi-Armed Bandits Based Pool
  * Created by aldo on 23/05/17.
  */
class DepthNMABBasedPool(m: String, depth:Int, c1: Double, c2: Double, sizePool: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(sizePool, lRuns, gT) {

  override lazy val qRels: QRels = PoolConverter.repoolToDepthNMABBased(m, depth, c1, c2, sizePool, lRuns, gT)

  override def getName = DepthNMABBasedPool.getName(m, depth, c1, c2, sizePool)

  override def getPooledDocuments(topicId: Int): Set[Document] = DepthNMABBasedPool.getPooledDocuments(m, depth, c1, c2, topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = MABBasedPool(m, c1, c2, sizePool, lRuns, gT)

}

object DepthNMABBasedPool {

  val rnd = new Random(1234)

  def getName(m: String, depth:Int, c1: Double, c2: Double, sizePool: Int) = "MABBased_" + m + ":" + depth + ":" + c1 + ":" + c2 + ":" + sizePool

  def apply(m: String, depth:Int, c1: Double, c2: Double, pD: Int, lRuns: List[Runs], gT: QRels) = new DepthNMABBasedPool(m, depth, c1, c2, pD, lRuns, gT)

  def getPooledDocuments(m: String, depth:Int, c1: Double, c2: Double, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

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

      val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
      if(docs.size > nDs(topicId)){
        throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
      }
      val nORs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
      getDocuments(nORs, docs)
    }

    def greedy(c1: Double, c2: Double): Set[Document] = {

      def p(r: List[Document], qRel: QRel): Double = {
        if (r.isEmpty)
          0.5d
        else
          r.count(d => qRel.getRel(d) >= 1) / r.size
      }

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set(), n: Int = 1): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr =
            if (rnd.nextDouble() < Math.min(1d, (c1 * oRs.size) / (c2 * c2 * n)))
              rnd.shuffle(rs.keys.toList).head
            else
              oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
                val ds = r._2.take(oRs(r._1).size - rs(r._1).size)
                p(ds, cQRel) + rnd.nextDouble() / (cQRel.size * oRs.size)
              })).maxBy(_._2)._1

          // judge doc
          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if(cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc,
            n + 1)
        }
      }

      val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
      if(docs.size > nDs(topicId)){
        throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
      }
      val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
      val nORs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
      getDocuments(nORs, qRel, docs)
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
              oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
                // get played documents
                val ds = r._2.take(oRs(r._1).size - rs(r._1).size)
                // get sample rewards
                val xs: List[Double] = ds.tail.foldLeft(List(cQRel.getRel(ds.head) / 1d))((acc, d) => {
                  acc :+ (acc.last * acc.size + cQRel.getRel(d)) / (acc.size + 1)
                })
                // compute average
                val avg = xs.sum / xs.size
                // compute variance
                val _var = xs.map(x => x * x).sum / xs.size - avg * avg
                // compute and return ucb1-tuned weight
                avg + Math.sqrt(Math.log(n) / xs.size * Math.min(0.25d, _var + Math.sqrt(2 * n / xs.size))) + rnd.nextDouble() / (cQRel.size * oRs.size)
              })).maxBy(_._2)._1

          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if(cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc,
            n + 1)
        }
      }

      val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
      if(docs.size > nDs(topicId)){
        throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
      }
      val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
      val nORs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
      getDocuments(nORs, qRel, docs)
    }

    def beta(): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr =
            oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
              val ds = r._2.take(oRs(r._1).size - rs(r._1).size)
              new BetaDistribution(1d + ds.count(d => cQRel.getRel(d) > 0), 1d + ds.count(d => cQRel.getRel(d) == 0)).sample()
            })).maxBy(_._2)._1

          // judge doc
          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if(cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc)
        }
      }

      val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
      if(docs.size > nDs(topicId)){
        throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
      }
      val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
      val nORs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
      getDocuments(nORs, qRel, docs)
    }

    def maxMean(): Set[Document] = {

      @tailrec def getDocuments(rs: Map[Int, List[Document]], cQRel: QRel = new QRel(topicId, Nil), acc: Set[Document] = Set()): Set[Document] = {
        if (acc.size == nDs(topicId) || rs.isEmpty)
          acc
        else {
          // select arm
          val nr =
            oRs.filter(r => rs.contains(r._1)).map(r => (r._1, {
              val ds = r._2.take(oRs(r._1).size - rs(r._1).size)
              (1d + ds.count(d => cQRel.getRel(d) > 0)) / (2d + ds.count(d => cQRel.getRel(d) == 0)) + rnd.nextDouble() / (cQRel.size * oRs.size)
            })).maxBy(_._2)._1

          // judge doc
          val doc = rs(nr).head
          val rel = if (cQRel.getRel(doc) < 0) qRels.getRel(topicId, doc) else cQRel.getRel(doc)

          getDocuments(
            FixedSizePool.updateSimplifiedLRuns(rs, nr),
            if(cQRel.getRel(doc) < 0)
              new QRel(cQRel.id, cQRel.qrelRecords :+ QRelRecord("Q0", doc, rel))
            else
              cQRel,
            acc + doc)
        }
      }

      val docs = DepthNPool.getPooledDocuments(depth, pRuns, qRels)(topicId)
      if(docs.size > nDs(topicId)){
        throw new InstantiationException("The selected poolSize (" + nDs.values.sum + ") is not sufficient for this pooling strategy")
      }
      val qRel = new QRel(topicId, docs.map(doc => QRelRecord("Q0", doc, qRels.getRel(topicId, doc))).toList)
      val nORs = oRs.map(e => e._1 -> oRs(e._1).drop(depth)).filter(_._2.nonEmpty)
      getDocuments(nORs, qRel, docs)
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
