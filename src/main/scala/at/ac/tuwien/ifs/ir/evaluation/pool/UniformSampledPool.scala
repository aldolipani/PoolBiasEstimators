package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 12/09/15.
 */
class UniformSampledPool(val n:Int, val rate:Float, lRuns: List[Runs], qRels: QRels) extends Pool(lRuns, qRels) {

  def getPooledDocuments(topicId: Int): Set[Document] = UniformSampledPool.getPooledDocuments(topicId)(n, rate, lRuns)

}

object UniformSampledPool{
/*
  lazy val rnd = new scala.util.Random(1234)

  def tossIntFromDist(b:Int) = rnd.nextInt(b)

  def indexSample(n:Int, rate:Float):Set[Int] =
    (0 until Math.round(n/rate)).map(_ => tossIntFromDist(n)).toSet

  def getPooledDocuments(topicId:Int)(n:Int, rate:Float, lRuns:List[Runs]):Set[Document] =
    getPooledDocuments(n, rate, lRuns.map(_.selectByTopicId(topicId)).filter(_!=null))

  def getPooledDocuments(n:Int, rate:Float, lRun:List[Run]):Set[Document] =
    lRun.flatMap(l => {
      val is = indexSample(n, rate)
      l.runRecords.take(n).zipWithIndex.filter(e => is.contains(e._2)).map(_._1.document)
    }).toSet
*/
  def getPooledDocuments(topicId:Int)(n:Int, rate:Float, lRuns:List[Runs]):Set[Document] =
    StratifiedSampledPool.getPooledDocuments(topicId)(List(new Strata(n, rate)), lRuns)
}
