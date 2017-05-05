package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.util.Random

/**
  * Multi-Armed Bandits Based Pool
  * Created by aldo on 25/07/16.
  */
class MABBasedPool(m: String, c1: Double, c2: Double, sizePool: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(sizePool, lRuns, gT) {

  override lazy val qRels: QRels = PoolConverter.repoolToMABBased(m, c1, c2, sizePool, lRuns, gT)

  override def getName = MABBasedPool.getName(m, c1, c2, sizePool)

  override def getPooledDocuments(topicId: Int): Set[Document] = MABBasedPool.getPooledDocuments(m, c1, c2, topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = MABBasedPool(m, c1, c2, sizePool, lRuns, gT)

}

object MABBasedPool {

  val rnd = new Random(1234)

  def getName(m: String, c1: Double, c2: Double, sizePool: Int) = "MABBased_" + m + ":" + c1 + ":" + c2 + ":" + sizePool

  def apply(m: String, c1: Double, c2: Double, pD: Int, lRuns: List[Runs], gT: QRels) = new MABBasedPool(m, c1, c2, pD, lRuns, gT)

  def getPooledDocuments(m: String, c1: Double, c2: Double, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    // runId# -> documents
    lazy val lrs: Map[Int, List[Document]] =
      pRuns.filter(_.selectByTopicId(topicId) != null).zipWithIndex.map(run =>
        run._2 -> run._1.selectByTopicId(topicId).runRecords.map(_.document)).filter(_._2.nonEmpty).toMap

    def randomPooling(): Set[Document] = {
      def selectArm(lrs: Map[Int, List[Document]], acc: Set[Document]): Set[Document] = {
        if (acc.size == nDs(topicId) || lrs.isEmpty)
          acc
        else {
          val i = rnd.nextInt(lrs.size)
          val k = lrs.keys.toList.sorted.toList(i)
          val d = lrs(k).head
          if (lrs(k).tail.nonEmpty) {
            selectArm(lrs + (k -> lrs(k).tail), acc + d)
          } else {
            selectArm(lrs - k, acc + d)
          }
        }
      }
      selectArm(lrs, Set())
    }

    def greedyPooling(c1: Double, c2: Double): Set[Document] = {
      // initialization
      val prs: Map[Int, (Double, Int)] = lrs.mapValues(_ => (0.5d, 1))

      def getEpsilon(n: Int) = Math.min(1d, (c1 * lrs.size) / (c2 * c2 * n))

      def selectArm(epsilon: Double, lrs: Map[Int, List[Document]], prs: Map[Int, (Double, Int)], acc: Set[Document], n: Int = 1): Set[Document] = {
        if (acc.size == nDs(topicId) || lrs.isEmpty)
          acc
        else {
          // selection
          val i =
            if (rnd.nextDouble() > epsilon)
              prs.maxBy(_._2._1)._1
            else {
              val i = rnd.nextInt(prs.size)
              lrs.keys.toList.sorted.toList(i)
            }

          // update
          val d = lrs(i).head
          val nlrs =
            if (lrs(i).tail.nonEmpty)
              lrs + (i -> lrs(i).tail)
            else
              lrs - i

          val ds = if (qRels.getRel(topicId, d.id) > 0) 1 else 0
          val nprs =
            if (lrs(i).tail.nonEmpty)
              prs + (i ->(
                incAvg(ds, prs(i)._1, prs(i)._2),
                prs(i)._2 + 1))
            else
              prs - i

          val nEpsilon = getEpsilon(n + 1)
          selectArm(nEpsilon, nlrs, nprs, acc + d, n + 1)
        }
      }

      selectArm(getEpsilon(1), lrs, prs, Set())
    }

    def incAvg(x1: Double, avg0: Double, n: Int): Double = (avg0 * n) / (n + 1) + x1 / (n + 1)

    def incSd(x1: Double, avg0: Double, sd0: Double, n: Int): Double = sd0 * (n - 2) / (n - 1) + Math.pow(x1 - avg0, 2) / n

    def ucb1TunedPooling(): Set[Document] = {
      //initialization
      val prs: Map[Int, (Double, Double, Int)] = lrs.map(e => {
        e._1 ->(if (qRels.getRel(topicId, lrs(e._1).head.id) > 0) 1d else 0d, 0d, 1)
      }).filter(e => lrs(e._1).tail.nonEmpty)
      // update
      val nlrs = lrs.mapValues(_.drop(1)).filter(_._2.nonEmpty)

      val acc = lrs.map(_._2.head).toSet

      def selectArm(lrs: Map[Int, List[Document]], prs: Map[Int, (Double, Double, Int)], acc: Set[Document], n: Int = 2): Set[Document] = {
        if (acc.size == nDs(topicId) || lrs.isEmpty)
          acc
        else {
          // selection
          val i =
            prs.maxBy(e => {
              val avg = e._2._1
              val sd = e._2._2
              val ns = e._2._3
              val f = Math.log(n) / ns
              avg + Math.sqrt(f * Math.min(0.25d, sd + Math.sqrt(2 * f)))
            })._1

          // update
          val d = lrs(i).head

          val nlrs =
            if (lrs(i).tail.nonEmpty)
              lrs + (i -> lrs(i).tail)
            else
              lrs - i

          val ds = if (qRels.getRel(topicId, d.id) > 0) 1 else 0
          val nprs =
            if (lrs(i).tail.nonEmpty)
              prs + (i ->(
                incAvg(ds, prs(i)._1, prs(i)._3),
                incSd(ds, prs(i)._1, prs(i)._2, prs(i)._3), prs(i)._3 + 1))
            else
              prs - i //(i -> (-1d, 0d, prs(i)._3 + 1))}

          selectArm(nlrs, nprs, acc + d, n + 1)
        }
      }

      selectArm(nlrs, prs, acc, acc.size)
    }

    def blaPooling(): Set[Document] = {
      //initialization
      val prs: Map[Int, (Double, Double)] = lrs.mapValues(_ => (1d, 1d))

      def selectArm(lrs: Map[Int, List[Document]], prs: Map[Int, (Double, Double)], acc: Set[Document]): Set[Document] = {
        if (acc.size == nDs(topicId) || lrs.isEmpty)
          acc
        else {
          // selection
          val i = prs.maxBy(e => e._2._1 / (e._2._1 + e._2._2))._1

          // update
          val d = lrs(i).head
          val nlrs =
            if (lrs(i).tail.nonEmpty)
              lrs + (i -> lrs(i).tail)
            else
              lrs - i

          val ds = if (qRels.getRel(topicId, d.id) > 0) 1 else 0
          val nprs =
            if (lrs(i).tail.nonEmpty)
              prs + (i ->(
                prs(i)._1 + ds,
                prs(i)._1 + 1d - ds))
            else
              prs - i

          selectArm(nlrs, nprs, acc + d)
        }
      }

      selectArm(lrs, prs, Set())
    }

    if (m == "random")
      randomPooling()
    else if (m == "greedy")
      greedyPooling(c1, c2)
    else if (m == "ucb")
      ucb1TunedPooling()
    else if (m == "bla")
      blaPooling()
    else
      throw new Exception("Method not found")
  }

}
