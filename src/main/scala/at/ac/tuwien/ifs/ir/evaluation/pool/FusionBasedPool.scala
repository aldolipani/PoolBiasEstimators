package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.collection.parallel.{ParSeq, ParMap}

/**
  * Created by aldo on 31/08/16.
  */
class FusionBasedPool(method:String, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName = FusionBasedPool.getName(method, poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToFusionBased(method, poolSize, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = FusionBasedPool.getPooledDocuments(method, topicSizes, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = FusionBasedPool(method, poolSize, lRuns, gT)

}

object FusionBasedPool {

  def apply(method: String, poolSize: Int, lRuns: List[Runs], gT: QRels) = new FusionBasedPool(method, poolSize, lRuns, gT)

  def getName(method:String, poolSize:Int) = "fusionbased_" + method + ":" + poolSize

  def getPooledDocuments(method: String, topicsSizes: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def normalize(ls:List[(Document, Float)]): List[(Document, Float)] ={
      if(ls.nonEmpty){
        val max = ls.maxBy(_._2)._2
        val min = ls.minBy(_._2)._2
        if(max != min) {
          return ls.map(e => (e._1, (e._2 - min) / (max - min)))
        }
      }
      ls
    }

    val ldvs: Map[Document, Seq[Float]] =
      lRuns.flatMap(rs => {
        val dss = rs.selectByTopicIdOrNil(topicId).runRecords.map(e => (e.document, e.score))
        normalize(dss)
      }).groupBy(_._1).mapValues(vs => vs.map(_._2))

    def min(l:Seq[Float]):Float =
      if(l.isEmpty)
        0f
      else
        l.min

    def max(l:Seq[Float]):Float =
      if(l.isEmpty)
        0f
      else
        l.max

    def sum(l:Seq[Float]):Float =
      if(l.isEmpty)
        0f
      else
        l.sum

    def anz(l:Seq[Float]):Float =
      if(l.size == 0)
        0f
      else
        sum(l) / l.size

    def mnz(l:Seq[Float]) = sum(l) * l.size

    def med(l:Seq[Float]) = {
      if(l.size == 0)
        0f
      else if(l.size % 2 == 0) {
        val i = l.size / 2
        l(i-1)/2 + l(i)/2
      }else
        l((l.size - 1)/2)
    }

    def getDocuments(f:(Seq[Float]) => Float): Set[Document] =
      ldvs.mapValues(vs => f(vs)).toList.sortBy(- _._2).take(topicsSizes(topicId)).map(_._1).toSet

    if(method == "combmin")
      getDocuments(min)
    else if(method == "combmax")
      getDocuments(max)
    else if(method == "combsum")
      getDocuments(sum)
    else if(method == "combanz")
      getDocuments(anz)
    else if(method == "combmnz")
      getDocuments(mnz)
    else if(method == "combmed")
      getDocuments(med)
    else
      throw new Exception("Method " + method + " not recognized!")
  }

}

