package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.evaluation.pool.HedgeBasedPool.rnd
import at.ac.tuwien.ifs.ir.model._

import scala.collection.parallel.{ParMap, ParSeq}

/**
  * Created by aldo on 31/08/16.
  */
class FusionBasedPool(method: String, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName:String = FusionBasedPool.getName(method, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToFusionBased(method, poolSize, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]): Pool = FusionBasedPool(method, poolSize, lRuns, gT)

}

object FusionBasedPool {

  def apply(method: String, poolSize: Int, lRuns: List[Runs], gT: QRels) = new FusionBasedPool(method, poolSize, lRuns, gT)

  def getName(method: String, poolSize: Int):String = "fusionbased_" + method + ":" + poolSize

  def getPooledDocuments(method: String, nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def normalize(rr: List[RunRecord]): List[RunRecord] = {
      if (rr.nonEmpty) {
        val max = rr.maxBy(_.score).score
        val min = rr.minBy(_.score).score
        if (max != min) {
          rr.map(e => RunRecord(e.iteration, e.document, e.rank, (e.score - min) / (max - min)))
        } else {
          rr
        }
      } else {
        rr
      }
    }

    val nLRuns = lRuns.filter(_.selectByTopicIdOrNil(topicId) != Nil).map(runs => {
      val run = runs.selectByTopicIdOrNil(topicId)
      new Runs(runs.id, List(new Run(run.id, normalize(run.runRecords))))
    })

    def w(rr: RunRecord, sizeRun: Int): Float =
      if(rr == null) 0f else rr.score

    def min(l: Seq[Float]): Float =
      l.min

    def max(l: Seq[Float]): Float =
      l.max

    def sum(l: Seq[Float]): Float =
      l.sum

    def anz(l: Seq[Float]): Float =
      sum(l) / l.count(_ > 0f)

    def mnz(l: Seq[Float]): Float =
      sum(l) * l.count(_ > 0f)

    def med(l: Seq[Float]) = {
      val sl = l.sorted
      if (sl.size % 2 == 0) {
        val i = sl.size / 2
        sl(i - 1) / 2f + sl(i) / 2f
      } else
        sl((sl.size - 1) / 2)
    }

    NonAdaptiveBasedPool.getPooledAlsoNullDocumentsWithStat(w,
      method match {
        case "combmin" => min
        case "combmax" => max
        case "combsum" => sum
        case "combanz" => anz
        case "combmnz" => mnz
        case "combmed" => med
        case _ => throw new Exception("Method " + method + " not recognized!")
      }, nDs, nLRuns, qRels)(topicId)
  }

}

