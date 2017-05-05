package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by aldo on 29/03/16.
  */

class MTFBasedPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = MTFBasedPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToMTFBased(poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = MTFBasedPool.getPooledDocuments(topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = MTFBasedPool(poolSize, lRuns, gT)

}

object MTFBasedPool {

  val rnd = new Random(1234)

  def apply(poolSize: Int, lRuns: List[Runs], gT: QRels) = new MTFBasedPool(poolSize, lRuns, gT)

  def getName(poolSize: Int) = "mtfbased_" + poolSize

  def getPooledDocuments(nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId:Int): Set[Document] = {

    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    // runId# -> documents
    lazy val lrs: Map[Int, List[Document]] =
      pRuns
        .filter(_.selectByTopicId(topicId) != null).zipWithIndex
        .map(run => run._2 -> run._1.selectByTopicId(topicId).runRecords.map(_.document))
        .filter(_._2.nonEmpty).toMap

    val maxSizeRanks = lrs.maxBy(_._2.size)._2.size

    // initialization
    val prs: Map[Int, Int] = lrs.mapValues(_ => maxSizeRanks)

    def getDocument(i: Int, lrs: Map[Int, List[Document]], prs: Map[Int, Int], acc: Set[Document]): Set[Document] = {
      if (acc.size == nDs(topicId) || lrs.isEmpty)
        acc
      else {
        // selection
        val ni =
          if (i == -1) {
            val maxPrVal = prs.maxBy(_._2)._2 // find max priority
            val maxPrs = prs.filter(_._2 == maxPrVal) // get all items with max priority
            val keysMaxPrs = maxPrs.keys.toList.sorted // get their keys and sort them
            keysMaxPrs(rnd.nextInt(keysMaxPrs.size)) // this is my new index
          } else i

        // update
        val doc = lrs(ni).head
        val nlrs = lrs + (ni -> lrs(ni).drop(1))

        if (qRels.getRel(topicId, doc) > 0) {
          if(nlrs(ni).nonEmpty)
            getDocument(ni, nlrs, prs, acc + doc)
          else
            getDocument(-1, nlrs - ni, prs - ni, acc + doc)
        } else {
          if(nlrs(ni).nonEmpty) {
            val nprs = prs + (ni -> (prs(ni) - 1))
            getDocument(-1, nlrs, nprs, acc + doc)
          } else
            getDocument(-1, nlrs - ni, prs - ni, acc + doc)
        }
      }
    }
    getDocument(-1, lrs, prs, Set())

  }

}
