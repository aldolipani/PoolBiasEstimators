package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

/**
 * Created by aldo on 30/03/16.
 */
class Stratified2StrataPool(poolSize:Int, n:Int, lRuns: List[Runs], gT: QRels) extends Pool(lRuns, gT) {

  override def getName = Stratified2StrataPool.getName(poolSize, n)

  override lazy val qRels: QRels = PoolConverter.repoolToStratified2Strata(poolSize, n, lRuns, gT)

  lazy val fdSr = Stratified2StrataPool.getFdSr(poolSize, n, lRuns, gT)

  //override def getPooledDocuments(topicId: Int): Set[Document] = Stratified2StrataPool.getPooledDocuments(fdSr, poolSize, n, lRuns, gT)(topicId)

  override def getNewInstance(lRuns: List[Runs]): Pool = Stratified2StrataPool(poolSize, n, lRuns, gT)

}

object Stratified2StrataPool{

  def getName(poolSize:Int, n:Int) = "Stratified2Strata_" + poolSize + ":" + n

  def apply(nD:Int, n:Int, lRuns: List[Runs], gT: QRels) = new Stratified2StrataPool(nD, n, lRuns, gT)

  val rnd = new scala.util.Random(1234)

  def isSampled(p:Float):Boolean = rnd.nextFloat() <= p

  def getFdSr(nD:Int, n:Int, lRuns: List[Runs], qRels: QRels):(Int, Float) = {
    val ls =
      qRels.topicIds.map(tId => {
        lRuns.filter(_.selectByTopicId(tId)!=null).flatMap(r =>
          r.selectByTopicId(tId).runRecords.map(rr => (rr.rank, rr.document.id))).groupBy(_._2).
            map(e => (e._1, e._2.map(_._1).min)).groupBy(_._2).map(e => (e._1, e._2.size)).toList.sortBy(_._1).map(_._2) // list 0:n0, 1:n2 ...
      }).reduce((a, b) => a.zip(b).map(e => e._1+e._2)).foldLeft(List(0))((acc, e) => acc:+(e+acc.last)).drop(1)
    //println(ls.filter(e => e < nD).size, ls.filter(e => e < nD))

    val fd = ls.filter(e => e < nD).size

    val a = if(fd >= n)
      (n, 0f)
    else {
      val sFd = ls(fd-1)
      val sSd = nD - sFd
      val aSd = ls(n-1) - sFd
      (fd, 100f * sSd/aSd)
    }
    //println(a)
    a
  }

  def getPooledDocuments(fdSr:(Int, Float), nD:Int, n:Int, lRuns: List[Runs], gT: QRels)(topicId:Int): Set[Document] = {
    StratifiedPool.getPooledDocuments(List(new Strata(fdSr._1, 100f), new Strata(n - fdSr._1, fdSr._2)), lRuns)(topicId)
  }
}

