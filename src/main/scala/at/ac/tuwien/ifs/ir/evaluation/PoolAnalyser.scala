package at.ac.tuwien.ir.evaluation

import at.ac.tuwien.ir.model._

/**
 * Created by aldo on 10/10/14.
 */
class PoolAnalyser(lRuns: List[Runs], qRels: QRels) {

  lazy val d: Int = computePoolDepth

  private def computePoolDepth() = {
    val qRel = qRels.qRels.head
    val l = for (runs <- lRuns) yield {
      for (qRel <- qRels.qRels) yield {
        val run = runs.selectByTopicId(qRel.id)
        if (run != null)
          getApproximateSize(run, qRel)
        else 0
      }
    }

    mode(l.flatten.filter(_ != 0))
  }

  private def getApproximateSize(run: Run, qRel: QRel, an: Int = 0): Int = {

    def getApproximateSize(run: List[RunRecord], n: Int, an: Int): Int =
      if (run.isEmpty)
        n
      else if (qRel.containsDocumentId(run.head.document.id))
        getApproximateSize(run.tail, n + 1, an)
      else if (an > 0)
        getApproximateSize(run.tail, n, an - 1)
      else
        n

    getApproximateSize(run.runRecords.sortBy(_.rank), 0, an)
  }

  private def mode[A](l: Seq[A]) = l.groupBy(i => i).mapValues(_.size).maxBy(_._2)._1

  lazy val pooledRuns = getPooledRuns

  private def getPooledRuns(): List[Runs] = getShallowPooledRuns()

  private def getShallowPooledRuns(): List[Runs] = {

    def getPooledRuns(lRuns: List[Runs], pooledRuns: List[Runs]): List[Runs] = {
      if (lRuns.isEmpty)
        pooledRuns
      else {
        val sRuns = lRuns.head
        if ((sRuns.topicIds.intersect(qRels.topicIds)).forall(tId => {
          var run = sRuns.selectByTopicId(tId)
          val aSize = getApproximateSize(run, qRels.topicQRels.get(tId).get, (d.toDouble / 10).toInt)
          run == null || aSize >= d || run.runRecords.size < aSize + d / 10
        })) {
          getPooledRuns(lRuns.tail, pooledRuns :+ lRuns.head)
        } else
          getPooledRuns(lRuns.tail, pooledRuns)
      }
    }

    getPooledRuns(lRuns, Nil)
  }

  def repoolWith(lRuns: List[Runs]): QRels = {
    val nQRels =
      for (qRel <- qRels.qRels.par if (lRuns.head.selectByTopicId(qRel.id) != null)) yield {
        val docs =
          lRuns.flatMap(l => {
            if (l.selectByTopicId(qRel.id) == null)
              Nil
            else
              l.selectByTopicId(qRel.id).runRecords.take(d).map(_.document)
          }).toSet
        new QRel(qRel.id, qRel.qrelRecord.filter(r => docs.contains(r.document)))
      }

    new QRels(qRels.id, nQRels.seq)
  }
}
