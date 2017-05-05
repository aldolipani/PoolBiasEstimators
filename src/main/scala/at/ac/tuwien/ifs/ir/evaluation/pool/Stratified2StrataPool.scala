package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 30/03/16.
 */
class Stratified2StrataPool(stratas:List[Strata], lRuns: List[Runs], qRels: QRels) extends Pool(lRuns, qRels) {

  def getPooledDocuments(topicId: Int): Set[Document] = StratifiedPool.getPooledDocuments(topicId)(stratas, lRuns)

}

object StratifiedPool{

  val rnd = new scala.util.Random(1234)

  def isSampled(p:Float):Boolean = rnd.nextFloat() <= p
  /*
    def getPooledDocuments(topicId:Int)(stratas:List[Strata], lRuns:List[Runs]):Set[Document] = {
      def getPooledDocuments(stratas:List[Strata], lRun:List[Run], acc:Set[Document]):Set[Document] = {
        if(stratas.isEmpty)
          acc
        else {
          val strata = stratas.head
          getPooledDocuments(
            stratas.tail,
            lRun.map(r => new Run(r.id, r.runRecords.drop(strata.depth))),
            UniformSampledPool.getPooledDocuments(strata.depth, strata.rate, lRun))
        }
      }
      getPooledDocuments(stratas, lRuns.map(_.selectByTopicId(topicId)), Set())
    }*/

  def getPooledDocuments(topicId:Int)(stratas:List[Strata], lRuns:List[Runs]):Set[Document] = {
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
      sPSs._1.filter(e => isSampled(sPSs._2.rate/100f))
    }).flatten.toSet

    stratifiedSampledPool
  }
}

