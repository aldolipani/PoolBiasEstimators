package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 12/09/15.
 */
class UniformSampledPool(val n:Int, val rate:Float, lRuns: List[Runs], gT: QRels) extends Pool(lRuns, gT) {

  override lazy val qRels:QRels = PoolConverter.repoolToUniformSampled(n, rate, lRuns, gT)

  override def getName = UniformSampledPool.getName(n, rate)

  //override def getPooledDocuments(topicId: Int): Set[Document] = UniformSampledPool.getPooledDocuments(n, rate, lRuns)(topicId)

  override def getNewInstance(lRuns: List[Runs]):Pool = UniformSampledPool(n, rate, lRuns, gT)

}

object UniformSampledPool{

  def getName(n:Int, rate:Float) = "UniformSampled_" + n + ":" + rate

  def getPooledDocuments(n:Int, rate:Float, lRuns:List[Runs])(topicId:Int):Set[Document] =
    StratifiedPool.getPooledDocuments(List(new Strata(n, rate)), lRuns)(topicId)

  def apply(n:Int, rate:Float, lRuns: List[Runs], gT: QRels) = new UniformSampledPool(n, rate, lRuns, gT)
}
