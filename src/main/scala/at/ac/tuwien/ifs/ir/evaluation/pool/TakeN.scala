package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, RunRecord, Runs}

class TakeNPool(poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override def getName: String = TakeNPool.getName(poolSize)

  override lazy val qRels: QRels = PoolConverter.repoolToTakeN(poolSize, lRuns, gT)

  override def getNewInstance(lRuns: List[Runs]): Pool = TakeNPool(poolSize, lRuns, gT)

}

object TakeNPool {

  def getName(poolSize: Int): String = "Take_" + poolSize

  def apply(pD: Int, lRuns: List[Runs], gT: QRels) = new TakeNPool(pD, lRuns, gT)

  def getPooledDocuments(nDs: Map[Int, Int], lRuns: List[Runs], qRels: QRels)(topicId: Int): Set[Document] = {

    def stat(vs: List[Float]): Float = vs.max

    def rank(rr: RunRecord): Float = -rr.rank

    val lRun = lRuns.map(_.selectByTopicIdOrNil(topicId))

    lRun.flatMap(run => {
      run.runRecords.map(rr => (rr.document, rr))
    }).groupBy(_._1).mapValues(vs => stat(vs.map(e => rank(e._2))))
      .toList.sortBy(-_._2).take(nDs(topicId)).map(_._1).toSet
  }

}
