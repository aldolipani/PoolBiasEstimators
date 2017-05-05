package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{QRels, Runs}

import scala.annotation.tailrec

/**
  * Created by aldo on 21/08/16.
  */
class FixedSizePool(poolSize:Int, lRuns:List[Runs], gT:QRels) extends Pool(lRuns, gT) {

  protected lazy val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, gT)

}

object FixedSizePool {

  def findTopicSizes(poolSize:Int, lRuns:List[Runs], qRels: QRels) : Map[Int, Int] = {
    lazy val maxTopicSizes = qRels.topicIds.map(tId =>
      tId -> lRuns.withFilter(_.selectByTopicId(tId) != null).flatMap(
        _.selectByTopicId(tId).runRecords.map(_.document.id)).toSet.size).toMap

    @tailrec
    def findTopicSizes(topicSizes: Map[Int, Int], n: Int = -1): Map[Int, Int] = {
      if (n >= poolSize) {
        val nNDs = (1 to n - poolSize).foldRight(topicSizes)((n, tDs) => tDs + {
          val nD = tDs.toList.sortBy(_._2).head
          (nD._1, nD._2 - 1)
        })
        nNDs
      } else if (n == -1) {
        // initialize with the average or the minimum possible
        val avgPD = poolSize / qRels.topicIds.size
        val nNDs = topicSizes.map(e => e._1 -> Math.min(maxTopicSizes(e._1), avgPD))
        findTopicSizes(nNDs, nNDs.values.sum)
      } else {
        val nNDs = topicSizes.map(e => e._1 -> (if (maxTopicSizes(e._1) > e._2) e._2 + 1 else e._2))
        findTopicSizes(nNDs, nNDs.values.sum)
      }
    }

    findTopicSizes(Map[Int, Int]())
  }
}