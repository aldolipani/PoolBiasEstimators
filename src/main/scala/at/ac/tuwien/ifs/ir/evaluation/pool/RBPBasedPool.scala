package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.annotation.tailrec

/**
  * Created by aldo on 29/03/16.
  */

class RBPBasedPool(method: String, p: Double, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = RBPBasedPool.getName(method, p, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToRBPBased(method, p, poolSize, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = RBPBasedPool.getPooledDocuments(method, p, estimatedNDs, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = RBPBasedPool(method, p, poolSize, lRuns, gT)

}

object RBPBasedPool {

  def apply(m: String, p: Double, pD: Int, lRuns: List[Runs], gT: QRels) = new RBPBasedPool(m, p, pD, lRuns, gT)

  def getName(method: String, p: Double, poolSize: Int) =  "rbpbased_" + method + ":" + p + ":" + poolSize

  def getPooledDocuments(m: String, p: Double, nDs: Map[Int, Int], pRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {
    val qRuns = pRuns.filter(_.selectByTopicId(topicId) != null).map(r => new Runs(r.id, List(r.selectByTopicId(topicId))))

    // documentID -> run -> rbpw
    lazy val lds: Map[Document, Map[String, Double]] =
      qRuns
        .flatMap(r =>
        r.runs.head.runRecords.map(d =>
          (d.document, (r.id, d.rank))))
        .groupBy(_._1).map(e =>
        (e._1, e._2.map(_._2).toMap.mapValues(rbpw(_))))

    lazy val mRuns = qRuns.map(runs => runs.id -> runs).toMap

    def rbpBasedPooling(): Set[Document] = {
      val scores = lds.mapValues(_.maxBy(_._2)).toList.sortBy(e => (-e._2._2, e._2._1))
      /*for(score <- scores){
        Console.out.println("scoring -> " + score._1.id + " " + score._2._2 + "")
      }*/
      val selections = scores.take(nDs(topicId)).map(_._1)
      /*for(selection <- selections){
        Console.out.println("selecting -> " + selection.id)
      }
      for(selection <- selections){
        Console.out.println("assessing -> " + selection.id + " " + qRels.getRel(topicId, selection.id))
      }*/
      selections.toSet
    }

    def rbpBasedA(): Set[Document] = {
      val scores = lds.mapValues(e => (e.maxBy(_._2)._1 -> e.values.sum)).toList.sortBy(e => (-e._2._2, e._2._1))
      /*for(score <- scores){
              Console.out.println("scoring -> " + score._1.id + " " + score._2._2 + "")
            }*/
      val selections = scores.take(nDs(topicId)).map(e => e._1)
      /*for(selection <- selections){
              Console.out.println("selecting -> " + selection.id)
            }
            for(selection <- selections){
              Console.out.println("assessing -> " + selection.id + " " + qRels.getRel(topicId, selection.id))
            }*/
      selections.toSet
    }

    def rbpw(rank: Int): Double =
      if(p == 1d)
        1d // constant
      else
        (1d - p) * Math.pow(p, rank - 1)

    def rbp(runId: String, topicId: Int, acc: Set[Document]) = {
      val run = mRuns(runId).selectByTopicId(topicId)
      run.runRecords.filter(rr => acc.contains(rr.document)).map(rr =>
        if (qRels.getRel(topicId, rr.document) > 0) rbpw(rr.rank) else 0d).sum
    }

    def rbpResidual(runId: String, topicId: Int, acc: Set[Document]) = 1d - {
      val run = mRuns(runId).selectByTopicId(topicId)
      run.runRecords.filter(rr => acc.contains(rr.document)).map(rr => rbpw(rr.rank)).sum
    }

    // reduce ds
    def rbpBasedB(ids: Map[Document, Map[String, Double]] = lds, acc: Set[Document] = Set(), nd: Document = null, cv: Map[String, Double] = Map()): Set[Document] = {
      if (acc.size == nDs(topicId))
        acc
      else {
        val ncv =
          if (nd == null) {
            qRuns.map(r => r.id -> rbpResidual(r.id, topicId, acc)).toMap
          } else {
            cv ++ lds(nd).keys.map(k => k -> rbpResidual(k, topicId, acc))
          }

        val scores =
          ids.map(d =>
            (d._1,
              d._2.map(e =>
                e._2 * ncv(e._1)).sum)).toList.sortBy(-_._2)

        //for(score <- scores){
        //      Console.out.println("scoring -> " + score._1.id + " " + score._2)
        //    }

        val d = scores.take(1).head._1

        //Console.out.println("selecting -> " + d.id)

        rbpBasedB(ids - d, acc + d, d, ncv)
      }
    }

    def rbpBasedC(ids: Map[Document, Map[String, Double]] = lds, acc: Set[Document] = Set(), nd: Document = null, cv: Map[String, Double] = Map()): Set[Document] =
      if (acc.size == nDs(topicId))
        acc
      else {
        val ncv =
          if (nd == null) {
            qRuns.map(r => r.id -> {
              val res = rbpResidual(r.id, topicId, acc)
              (res * Math.pow(rbp(r.id, topicId, acc) + res / 2, 3))
            }).toMap
          } else {
            cv ++ lds(nd).keys.map(k => k -> {
              val res = rbpResidual(k, topicId, acc)
              (res * Math.pow(rbp(k, topicId, acc) + res / 2, 3))
            })
          }
        val scores =
          ids.map(d => (d._1, d._2.map(e =>
            e._2 * ncv(e._1)).sum)).toList.sortBy(-_._2)

        //for(score <- scores){
        //     Console.out.println("scoring -> " + score._1.id + " " + score._2)
        //    }

        val d = scores.take(1).head._1

        //Console.out.println("selecting -> " + d.id)
        //Console.out.println("assessing -> " + d.id + " " + qRels.getRel(topicId, d.id))

        rbpBasedC(ids - d, acc + d, d, ncv)
      }

    if (m == "pooling")
      rbpBasedPooling()
    else if (m == "a")
      rbpBasedA()
    else if (m == "b") {
      val documents = rbpBasedB()
      //documents.map(d => Console.out.println("assessing -> " + d.id + " " + qRels.getRel(topicId, d.id))).toList
      documents
    }else if (m == "c")
      rbpBasedC()
    else
      throw new Exception("Method not found")
  }

}
