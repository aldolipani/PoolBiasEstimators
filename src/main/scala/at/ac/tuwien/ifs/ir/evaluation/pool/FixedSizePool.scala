package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.evaluation.pool.HedgeBasedPool.rnd
import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}
import at.ac.tuwien.ifs.utils.Profiler

import scala.annotation.tailrec

/**
  * Created by aldo on 21/08/16.
  */
class FixedSizePool(poolSize:Int, lRuns:List[Runs], gT:QRels) extends Pool(lRuns, gT) {

  protected lazy val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, gT)

}

object FixedSizePool {

  def findTopicSizes(poolSize:Int, lRuns:List[Runs], qRels: QRels) : Map[Int, Int] = {
    val maxTopicSizes:Map[Int, Int] = qRels.topicIds.map(tId =>
      tId -> lRuns.withFilter(_.selectByTopicId(tId) != null).flatMap(
        _.selectByTopicId(tId).runRecords.map(_.document)).toSet.size).toMap

    //System.out.println("**************************************")
    //System.out.println(poolSize)
    //System.out.println(maxTopicSizes)

    val nM = maxTopicSizes.values.sum

    @tailrec
    def findTopicSizes(topicSizes: Map[Int, Int], n: Int = -1): Map[Int, Int] = {
      if (n >= poolSize || n == nM) {
        (1 to n - poolSize).foldRight(topicSizes)((n, tDs) => tDs + {
          val nD = tDs.toList.sortBy(_._2).head
          (nD._1, nD._2 - 1)
        })
      } else if (n == -1) {
        // initialize with the average or the minimum possible
        val avgPD = poolSize / maxTopicSizes.values.filter(_>0).size
        val nNDs = maxTopicSizes.map(e => e._1 -> Math.min(maxTopicSizes(e._1), avgPD))
        //System.out.println("*"+nNDs)
        findTopicSizes(nNDs, nNDs.values.sum)
      } else {
        val nNDs = topicSizes.map(e => e._1 -> (if (maxTopicSizes(e._1) > e._2) e._2 + 1 else e._2))
        //System.out.println("*"+nNDs)
        findTopicSizes(nNDs, nNDs.values.sum)
      }
    }
    findTopicSizes(Map[Int, Int]())
  }

  def getSimplifiedLRuns(topicId:Int, lRuns:List[Runs]): Map[Int, List[Document]] =
    getNonNullLRuns(topicId, lRuns).zipWithIndex
      .map(run => run._2 -> run._1.selectByTopicId(topicId).runRecords.map(_.document))
      .filter(_._2.nonEmpty).toMap

  def updateSimplifiedLRuns(rs: Map[Int, List[Document]], nr: Int) =
      if (rs(nr).tail.isEmpty)
        rs - nr
      else
        rs + (nr -> rs(nr).tail)

  def getNonNullLRuns(topicId:Int, lRuns:List[Runs]): List[Runs] =
    lRuns.filter(_.selectByTopicId(topicId) != null)

  def getNonDeterministicMaxObject[A, B : Numeric](docs:List[(A, B)]) = {
    val num = implicitly[Numeric[B]]
    import num._
    val sortedDocs = docs.sortBy(- _._2)
    val selectedDocs = sortedDocs.takeWhile(e => e._2 == sortedDocs.head._2)
    rnd.shuffle(selectedDocs).head._1
  }
}