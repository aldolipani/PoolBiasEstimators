package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 10/09/15.
 */
class Pool(val lRuns: List[Runs], gT: QRels) {

  def getName : String = ???

  lazy val pool: Map[Int, Set[Document]] = ???

  lazy val qRels:QRels = gT

  def getPooledDocuments(topicId: Int): Set[Document] = ???

  def getNewInstance(lRuns: List[Runs]): Pool = Pool(lRuns, gT)

}

object Pool {
  def apply(lRuns: List[Runs], gT: QRels) = new Pool(lRuns, gT)
}
