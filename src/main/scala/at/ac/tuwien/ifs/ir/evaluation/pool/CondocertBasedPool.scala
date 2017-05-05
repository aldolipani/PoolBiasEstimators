package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.annotation.tailrec

/**
  * Created by aldo on 04/08/16.
  */
class CondocertBasedPool(method: String, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = CondocertBasedPool.getName(method, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToCondocertBased(method, poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = CondocertBasedPool.getPooledDocuments(method, topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = CondocertBasedPool(method, poolSize, lRuns, gT)

}

object CondocertBasedPool {

  def getName(m:String, poolSize:Int) = "condocertbased:"+ m + "_" + poolSize

  def apply(method:String, pD: Int, lRuns: List[Runs], gT: QRels) = new CondocertBasedPool(method, pD, lRuns, gT)

  def getPooledDocuments(m: String, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    // runId# -> documents
    lazy val lrs: Map[Int, List[Document]] =
      pRuns.filter(_.selectByTopicId(topicId) != null).zipWithIndex.map(run =>
        run._2 -> run._1.selectByTopicId(topicId).runRecords.map(_.document)).toMap

    lazy val lrds: List[Map[Document, Int]] =
      lrs.values.toList.map(ld => ld.zipWithIndex.toMap)

    val allDs: Set[Document] = pRuns.filter(_.selectByTopicId(topicId) != null)
      .flatMap(_.selectByTopicId(topicId).runRecords.map(_.document)).toSet

    def simpleMajority(d1: Document, d2: Document): Boolean = {
      @tailrec
      def simpleMajority(lrds: List[Map[Document, Int]], acc: Int): Int = {
        if (lrds.isEmpty)
          acc
        else {
          val lrd = lrds.head
          if (lrd.getOrElse(d1, lrd.size) < lrd.getOrElse(d2, lrd.size))
            simpleMajority(lrds.tail, acc + 1)
          else if (lrd.getOrElse(d1, lrd.size) > lrd.getOrElse(d2, lrd.size))
            simpleMajority(lrds.tail, acc - 1)
          else
            simpleMajority(lrds.tail, acc)
        }
      }
      simpleMajority(lrds, 0) > 0
    }

    @tailrec
    def simpleSort(allDs:List[Document], acc:List[Document]): List[Document] ={
      if(allDs.isEmpty)
        acc
      else {
        val d = allDs.head
        val i = acc.indexWhere(d0 => simpleMajority(d, d0))
        simpleSort(allDs.tail, (acc.take(i) :+ d) ::: acc.drop(i))
      }
    }

    def countingSort(allDs:List[Document]): List[Document] = {
      allDs.map(d0 => (d0, allDs.count(d1 => simpleMajority(d0, d1)))).sortBy(-_._2).map(_._1)
    }

    if (m == "a")
      simpleSort(allDs.toList, List()).take(nDs(topicId)).toSet
    else if (m == "b")
      countingSort(allDs.toList).take(nDs(topicId)).toSet
    else
      throw new Exception("Method not found")
  }

}
