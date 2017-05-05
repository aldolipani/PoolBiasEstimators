package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 14/09/15.
 */
class StratifiedPool(stratas:List[Strata], lRuns: List[Runs], gT: QRels) extends Pool(lRuns, gT) {

  override def getName = StratifiedPool.getName(stratas)

  override lazy val qRels:QRels = PoolConverter.repoolToStratified(stratas, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]):Pool = new StratifiedPool(stratas, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = StratifiedPool.getPooledDocuments(stratas, lRuns)(topicId)

}

object StratifiedPool{

  def getName(stratas:List[Strata]) = "Stratified_" + stratas.map(s => s.depth + ":" + s.rate).mkString("_")

  val rnd = new scala.util.Random(1234)

  def isSampled(p:Float):Boolean = rnd.nextFloat() <= p

  def getPooledDocuments(stratas:List[Strata], lRuns:List[Runs])(topicId:Int):Set[Document] = {
    val depths = stratas.foldLeft(List(0))((acc, s) => acc:+(acc.last + s.depth))

    val stratifiedPool:List[Set[Document]] = {
      val drs = lRuns.map(runs => {
        if(runs.selectByTopicId(topicId)!=null)
          runs.selectByTopicId(topicId).runRecords.take(depths.last)
        else
          List()
      }).flatten
      val dss = drs.groupBy(_.document.id).map(e => (e._2.head.document, e._2.map(_.rank).min))

      depths.tail.zipWithIndex.map(e => {
        val or = depths(e._2); val nr = depths(e._2+1)
        dss.filter(ds => ds._2 > or && ds._2 <= nr).map(_._1).toSet
      })
    }

    val stratifiedSampledPool:Set[Document] = stratifiedPool.zip(stratas).map(sPSs => {
      val temp = sPSs._1.filter(e => isSampled(sPSs._2.rate/100f))
      temp
    }).flatten.toSet

    stratifiedSampledPool
  }

  def apply(stratas:List[Strata], lRuns: List[Runs], qRels: QRels) = new StratifiedPool(stratas, lRuns, qRels)
}
