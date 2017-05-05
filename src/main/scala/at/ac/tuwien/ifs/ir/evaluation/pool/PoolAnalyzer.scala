package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.PoolAnalyzerType.PoolAnalyzerType
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.ScoreEstimator
import at.ac.tuwien.ifs.ir.model._

/**
  * Created by aldo on 10/10/14.
  */
object PoolAnalyzerType extends Enumeration {
  type PoolAnalyzerType = Value
  val MODE, MAX_JUDGED = Value
}

class PoolAnalyzer(val pool: Pool, poolAnalyzerType:PoolAnalyzerType = PoolAnalyzerType.MODE) {


  lazy val d: Int = computePoolDepth
  lazy val pooledRuns = getPooledRuns

  private def computePoolDepthMode() = {
    val qRel = pool.qRels.qRels.head
    val l = for (runs <- pool.lRuns) yield {
      for (qRel <- pool.qRels.qRels) yield {
        val run = runs.selectByTopicId(qRel.id)
        if (run != null)
          getApproximateSize(run, qRel)
        else 0
      }
    }

    val ls = l.flatten.filter(_ != 0)
    if (ls.nonEmpty)
      mode(l.flatten.filter(_ != 0))
    else
      0
  }

  private def computePoolDepthMAXJudged() = {
    val qRel = pool.qRels.qRels.head
    val l = for (runs <- pool.lRuns) yield {
      for (qRel <- pool.qRels.qRels) yield {
        val run = runs.selectByTopicId(qRel.id)
        if (run != null)
          getApproximateSize(run, qRel)
        else 0
      }
    }

    val ls = l.flatten.filter(_ != 0).groupBy(i => i).mapValues(_.size).toList.sortBy(-_._2).map(_._1).take(3)
    if (ls.nonEmpty) {
      val ns = for (n <- ls) yield {
        (n, DepthNPool(n, pool.lRuns, pool.qRels).qRels.size)
      }
      ns.maxBy(_._2)._1
    } else
      0
  }

  private def computePoolDepth() = {
    if(poolAnalyzerType == PoolAnalyzerType.MAX_JUDGED)
      this.computePoolDepthMAXJudged()
    else
      this.computePoolDepthMode()
  }

  private def getApproximateSize(run: Run, qRel: QRel, an: Int = 0): Int = {

    def getApproximateSize(run: List[RunRecord], n: Int, an: Int): Int =
      if (run.isEmpty)
        n
      else if (qRel.containsDocumentId(run.head.document.id))
        getApproximateSize(run.tail, n + 1, an)
      else if (an > 0)
        getApproximateSize(run.tail, n + 1, an - 1)
      else
        n

    getApproximateSize(run.runRecords.sortBy(_.rank), 0, an)
  }

  private def getPooledRuns: List[Runs] = getShallowPooledRuns

  private def getShallowPooledRuns: List[Runs] = {
    def getPooledRuns(lRuns: List[Runs], pooledRuns: List[Runs]): List[Runs] = {
      if (lRuns.isEmpty)
        pooledRuns
      else {
        val sRuns = lRuns.head
        if (
          sRuns.topicIds.intersect(pool.qRels.topicIds).forall(tId => {
            val run = sRuns.selectByTopicId(tId)
            val qRel = pool.qRels.topicQRels.get(tId).get
            val aSize = getApproximateSize(run, qRel, d/10)
            //println(sRuns.id, tId, aSize, run == null || aSize >= d || run.runRecords.size == aSize)
            //println(sRuns.id, tId, aSize, d, run.runRecords.size)
            run == null || aSize >= d || run.runRecords.size == aSize
          }))
          getPooledRuns(lRuns.tail, pooledRuns :+ sRuns)
        else
          getPooledRuns(lRuns.tail, pooledRuns)
      }
    }

    getPooledRuns(pool.lRuns, Nil)
  }

  @deprecated
  def repoolWith(lRuns: List[Runs]): QRels = {
    val nQRels =
      for (qRel <- pool.qRels.qRels.par if (lRuns.head.selectByTopicId(qRel.id) != null)) yield {
        val docs =
          lRuns.flatMap(l => {
            if (l.selectByTopicId(qRel.id) == null)
              Nil
            else
              l.selectByTopicId(qRel.id).runRecords.take(d).map(_.document)
          }).toSet
        new QRel(qRel.id, qRel.qrelRecords.filter(r => docs.contains(r.document)))
      }

    new QRels(pool.qRels.id, nQRels.seq)
  }

  def getRunsOnly(ru: Runs, tId: Int): Runs = {
    val sru: Run = ru.runsMapByTopicId.getOrElse(tId, new Run(tId, List()))
    new Runs(ru.id, List(sru))
  }


  def getNumRelDocuments: List[Int] =
    pool.lRuns.map(run => pool.qRels.sizeRel - pool.getNewInstance(pool.lRuns.filter(_.id != run.id)).qRels.sizeRel).sorted

  def getNumRelDocumentsPerQuery: List[(Int, List[Int])] =
    pool.qRels.topicIds.map(tId => {
      val nlRuns = pool.lRuns.map(rs => getRunsOnly(rs, tId));
      val nPool = pool.getNewInstance(nlRuns)
      (tId, nPool.lRuns.map(run => nPool.qRels.sizeRel - nPool.getNewInstance(nPool.lRuns.filter(_.id != run.id)).qRels.sizeRel).sorted)
    }).toList.sortBy(_._1)

  def getRatioRelOverAllDocumentsPerQuery: List[(Int, List[(Int, Double)])] =
    pool.qRels.topicIds.map(tId => {
      val nlRuns = pool.lRuns.map(rs => getRunsOnly(rs, tId));
      val nPool = pool.getNewInstance(nlRuns)
      (tId,
        nPool.lRuns.map(run => {
          val nnPool = nPool.getNewInstance(nPool.lRuns.filter(_.id != run.id))
          if (nPool.qRels.size - nnPool.qRels.size == 0)
            (nPool.qRels.sizeRel, Double.NaN)
          else
            (nPool.qRels.sizeRel, (nPool.qRels.sizeRel - nnPool.qRels.sizeRel).toDouble / (nPool.qRels.size - nnPool.qRels.size))
        }).filter(e => e._2 != Double.NaN))
    }).toList.sortBy(_._1)

  def getNumNotRelDocuments: List[Int] =
    pool.lRuns.map(run => pool.qRels.sizeNotRel - pool.getNewInstance(pool.lRuns.filter(_.id != run.id)).qRels.sizeNotRel).sorted

  def getNumNotRelDocumentsPerQuery: List[(Int, List[Int])] =
    pool.qRels.topicIds.map(tId => {
      val nlRuns = pool.lRuns.map(rs => getRunsOnly(rs, tId));
      val nPool = pool.getNewInstance(nlRuns)
      (tId, nPool.lRuns.map(run => nPool.qRels.sizeNotRel - nPool.getNewInstance(nPool.lRuns.filter(_.id != run.id)).qRels.sizeNotRel).sorted)
    }).toList.sortBy(_._1)

  def getRatioNotRelOverAllDocumentsPerQuery: List[(Int, List[Double])] =
    pool.qRels.topicIds.map(tId => {
      val nlRuns = pool.lRuns.map(rs => getRunsOnly(rs, tId));
      val nPool = pool.getNewInstance(nlRuns)
      (tId,
        nPool.lRuns.map(run => {
          val nnPool = nPool.getNewInstance(nPool.lRuns.filter(_.id != run.id))
          if (nPool.qRels.size - nnPool.qRels.size == 0)
            0d
          else
            (nPool.qRels.sizeNotRel - nnPool.qRels.sizeNotRel).toDouble / (nPool.qRels.size - nnPool.qRels.size)
        }).filter(_ != 0d).sorted)
    }).toList.sortBy(_._1)

  def getNumDocuments: List[Int] =
    pool.lRuns.map(run => pool.qRels.size - pool.getNewInstance(pool.lRuns.filter(_.id != run.id)).qRels.size).sorted

  def getRatioNotRelOverAllDocuments: List[Double] =
    pool.lRuns.map(run => {
      val nnPool = pool.getNewInstance(pool.lRuns.filter(_.id != run.id))
      if (pool.qRels.size - nnPool.qRels.size == 0d)
        0d
      else
        (pool.qRels.sizeNotRel - nnPool.qRels.sizeNotRel).toDouble / (pool.qRels.size - nnPool.qRels.size)
    }).sorted

  /*def getRatioRelOverAllDocuments: List[Double] =
    pool.lRuns.map(run => {
      val nnPool = pool.getNewInstance(pool.lRuns.filter(_.id != run.id))
      if (pool.qRels.size - nnPool.qRels.size == 0d)
        0d
      else
        (pool.qRels.sizeNotRel - nnPool.qRels.sizeNotRel).toDouble / (pool.qRels.size - nnPool.qRels.size)
    }).sorted*/

  def getRatioRelOverAllDocuments(n:Int): List[Double] =
    pool.lRuns.map(run => {
      val nnPool = pool.getNewInstance(pool.lRuns.filter(_.id != run.id))
      if (pool.qRels.size - nnPool.qRels.size == 0d)
        0d
      else
        (TRECEval().computeMetric("P_"+n, run, pool.qRels) - TRECEval().computeMetric("P_"+n, run, nnPool.qRels)).toDouble /
          (TRECEval().computeMetric("P_"+n, run, nnPool.qRels) + TRECEval().computeMetric("P_"+n, run,  pool.qRels))
    }).sorted

  def getRatioRelOverAllDocuments2(descs: Descs): List[Double] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)
    olRuns.flatMap(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      slRuns.map(run => {
        val nPool = pool.getNewInstance(run :: nlRuns)
        val nnPool = pool.getNewInstance(nPool.lRuns.filter(_.id != run.id))
        if (nPool.qRels.size - nnPool.qRels.size == 0d)
          Double.NaN
        else
          (nPool.qRels.sizeRel - nnPool.qRels.sizeRel).toDouble / (nPool.qRels.size - nnPool.qRels.size)
      })
    }).sorted
  }

  def getRatioRelOverAllDocuments2(descs: Descs, n:Int): List[Double] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)
    olRuns.flatMap(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      slRuns.map(run => {
        val nPool = pool.getNewInstance(run :: nlRuns)
        val nnPool = pool.getNewInstance(nPool.lRuns.filter(_.id != run.id))
        if (nPool.qRels.size - nnPool.qRels.size == 0d)
          Double.NaN
        else
          (TRECEval().computeMetric("P_"+n, run, nPool.qRels) - TRECEval().computeMetric("P_"+n, run, nnPool.qRels)).toDouble /
            (TRECEval().computeMetric("P_"+n, run, nnPool.qRels) + TRECEval().computeMetric("P_"+n, run,  nPool.qRels))
      })
    }).sorted
  }

  def getEDocuments(descs: Descs): List[Double] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)
    olRuns.flatMap(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      slRuns.map(run => {
        val nPool = pool.getNewInstance(run :: nlRuns)
        val nnPool = pool.getNewInstance(nPool.lRuns.filter(_.id != run.id))
        TRECEval().computeP100(run, nPool.qRels) - TRECEval().computeP100(run, nnPool.qRels)
      })
    }).sorted
  }

  def getRatioRelOverAllDocuments(descs: Descs): List[Double] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)

    pool.lRuns.map(run => {
      val nnPool = pool.getNewInstance(pool.lRuns.filter(_.id != run.id))
      if (pool.qRels.size - nnPool.qRels.size == 0d)
        Double.NaN
      else
          /*TRECEval().computeP100(run, pool.qRels), */(pool.qRels.sizeRel - nnPool.qRels.sizeRel).toDouble / (pool.qRels.size - nnPool.qRels.size)
    })
  }

  def getRatioRelOverAll2DocumentsPerQuery(descs: Descs): List[(Int, List[Double])] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)
    olRuns.flatMap(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      val nPool = pool.getNewInstance(nlRuns)
      nPool.qRels.topicIds.map(tId => {
        val nlRuns = nPool.lRuns.map(rs => getRunsOnly(rs, tId))
        val nnPool = nPool.getNewInstance(nlRuns)
        (tId,
          nnPool.lRuns.map(run => {
            val nnnPool = nnPool.getNewInstance(nnPool.lRuns.filter(_.id != run.id))
            if (nnPool.qRels.size - nnnPool.qRels.size == 0)
              0d
            else
              (nnPool.qRels.sizeRel - nnnPool.qRels.sizeRel).toDouble / (nnPool.qRels.size - nnnPool.qRels.size)
          }))
      })
    }).groupBy(_._1).mapValues(_.map(_._2).reduce(_.zip(_).map(e => e._1 + e._2))).toList.sortBy(_._1)
  }

  def getRatioRelOverAll2Documents(descs: Descs): List[Double] = {
    val olRuns = descs.getRunsPerOrganization(pool.lRuns)
    olRuns.flatMap(slRuns => {
      val nlRuns = ScoreEstimator.excludeRuns(slRuns, pool.lRuns)
      val nPool = pool.getNewInstance(nlRuns)
      nlRuns.map(run => {
        val nnPool = pool.getNewInstance(nlRuns.filter(_.id != run.id))
        if (nPool.qRels.size - nnPool.qRels.size == 0d)
          0d
        else
          (nPool.qRels.sizeRel - nnPool.qRels.sizeRel).toDouble / (nPool.qRels.size - nnPool.qRels.size)
      }).sorted
    })
  }

  private def getNumDocumentsPerQuery: List[(Int, List[Int])] =
    pool.qRels.topicIds.map(tId => {
      val nlRuns = pool.lRuns.map(rs => getRunsOnly(rs, tId))
      val nPool = pool.getNewInstance(nlRuns)
      (tId, nPool.lRuns.map(run => nPool.qRels.size - nPool.getNewInstance(nPool.lRuns.filter(_.id != run.id)).qRels.size).sorted)
    }).toList.sortBy(_._1)

  private def mode[A](l: Seq[A]) =
      l.groupBy(i => i).mapValues(_.size).maxBy(_._2)._1


}

object PoolAnalyzer{

  def apply(pool: Pool, poolAnalyzerType:PoolAnalyzerType = PoolAnalyzerType.MODE):PoolAnalyzer = new PoolAnalyzer(pool, poolAnalyzerType)

  def apply(pool: Pool, poolAnalyzerType:String):PoolAnalyzer = new PoolAnalyzer(pool, parsePoolAnalyzerType(poolAnalyzerType))

  def parsePoolAnalyzerType(str: String):PoolAnalyzerType = {
    if(str == "max_judged")
      PoolAnalyzerType.MAX_JUDGED
    else
      PoolAnalyzerType.MODE
  }

}