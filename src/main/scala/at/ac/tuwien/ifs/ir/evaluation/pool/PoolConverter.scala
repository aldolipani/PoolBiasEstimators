package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.interactive.InteractiveQRels
import at.ac.tuwien.ifs.ir.model._
import com.google.common.cache.CacheBuilder
import scalacache._
import scalacache.guava.GuavaCache


/**
  * Created by aldo on 12/09/15.
  */

class PoolCommand

case class DepthNOption(n: Int) extends PoolCommand

case class UniformSampledOption(n: Int, rate: Float) extends PoolCommand

case class Strata(depth: Int, rate: Float)

case class StratifiedSampledOption(stratas: List[Strata]) extends PoolCommand

case class RBPBasedOption(method: String, p: Float, nD: Int) extends PoolCommand

case class DCGBasedOption(nD: Int) extends PoolCommand

case class MABBasedOption(method: String, c1: Float, c2: Float, nD: Int) extends PoolCommand

case class DetailedMABBasedOption(method: String, c1: Float, c2: Float, nDs: Map[Int, Int]) extends PoolCommand

case class DepthNMABBasedOption(method: String, depth: Int, c1: Float, c2: Float, nD: Int) extends PoolCommand

case class RRFBasedOption(k: Int, nD: Int) extends PoolCommand

case class FusionBasedOption(method: String, nD: Int) extends PoolCommand

case class HedgeBasedOption(beta: Double, nD: Int) extends PoolCommand

case class DepthNHedgeBasedOption(depth: Int, beta: Double, nD: Int) extends PoolCommand

case class MTFBasedOption(nD: Int) extends PoolCommand

case class DetailedMTFBasedOption(nDs: Map[Int, Int]) extends PoolCommand

case class DepthNMTFBasedOption(n: Int, nD: Int) extends PoolCommand

case class BordaBasedOption(nD: Int) extends PoolCommand

case class CondorcetBasedOption(nD: Int) extends PoolCommand

case class UnsupportedBasedOption(cmd: String) extends PoolCommand

case class UnsupportedFixedSizeBasedOption(cmd: String, poolSize: Int) extends PoolCommand

case class Stratified2StrataOption(n: Int, nD: Int) extends PoolCommand


object PoolConverter {

  val underlyingGuavaCache = CacheBuilder.newBuilder().maximumSize(10000L).build[String, Object]
  implicit val scalaCache = ScalaCache(GuavaCache(underlyingGuavaCache))

  def repoolToStratified(stratas: List[Strata], lRuns: List[Runs], qRels: QRels): QRels = {
    repoolTo(
      StratifiedPool.getPooledDocuments(stratas, lRuns),
      StratifiedPool.getName(stratas),
      lRuns,
      qRels)
  }

  def repoolToMTFBased(poolSize: Int, lRuns: List[Runs], qRels: QRels, nDs: Map[Int, Int]): QRels = {
    repoolTo(
      MTFBasedPool.getPooledDocuments(nDs, lRuns, qRels),
      MTFBasedPool.getName(poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthNMTFBased(depth: Int, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      DepthNMTFBasedPool.getPooledDocuments(depth, topicSizes, lRuns, qRels),
      DepthNMTFBasedPool.getName(depth, poolSize),
      lRuns,
      qRels)
  }

  def repoolToHedgeBased(beta: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      HedgeBasedPool.getPooledDocuments(beta, topicSizes, lRuns, qRels),
      HedgeBasedPool.getName(beta, poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthNHedgeBased(depth: Int, beta: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      DepthNHedgeBasedPool.getPooledDocuments(depth, beta, topicSizes, lRuns, qRels),
      DepthNHedgeBasedPool.getName(depth, beta, poolSize),
      lRuns,
      qRels)
  }

  def repoolToCondorcetBased(poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      CondorcetBasedPool.getPooledDocuments(topicSizes, lRuns, qRels),
      CondorcetBasedPool.getName(poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthN(n: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    repoolTo(
      DepthNPool.getPooledDocuments(n, lRuns, qRels),
      DepthNPool.getName(n),
      lRuns,
      qRels)
  }

  def repoolToBordaBased(poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      BordaBasedPool.getPooledDocuments(topicSizes, lRuns, qRels),
      BordaBasedPool.getName(poolSize),
      lRuns,
      qRels)
  }

  def repoolToRBPBased(method: String, p: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      RBPBasedPool.getPooledDocuments(method, p, topicSizes, lRuns, qRels),
      RBPBasedPool.getName(method, p, poolSize),
      lRuns,
      qRels)
  }

  def repoolToMABBased(method: String, c1: Double, c2: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels, nDs: Map[Int, Int]): QRels = {
    repoolTo(
      MABBasedPool.getPooledDocuments(method, c1, c2, nDs, lRuns, qRels),
      MABBasedPool.getName(method, c1, c2, poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthNMABBased(method: String, depth: Int, c1: Double, c2: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      DepthNMABBasedPool.getPooledDocuments(method, depth, c1, c2, topicSizes, lRuns, qRels),
      DepthNMABBasedPool.getName(method, depth, c1, c2, poolSize),
      lRuns,
      qRels)
  }

  def repoolToRRFBased(k: Int, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      RRFBasedPool.getPooledDocuments(k, topicSizes, lRuns, qRels),
      RRFBasedPool.getName(k, poolSize),
      lRuns,
      qRels)
  }

  def repoolToDCGBased(poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      DCGBasedPool.getPooledDocuments(topicSizes, lRuns, qRels),
      DCGBasedPool.getName(poolSize),
      lRuns,
      qRels)
  }

  def repoolToFusionBased(method: String, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      FusionBasedPool.getPooledDocuments(method, topicSizes, lRuns, qRels),
      FusionBasedPool.getName(method, poolSize),
      lRuns,
      qRels)
  }

  def repoolToStratified2Strata(n: Int, nD: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val fdSr = Stratified2StrataPool.getFdSr(nD, n, lRuns, qRels)
    repoolTo(
      Stratified2StrataPool.getPooledDocuments(fdSr, nD, n, lRuns, qRels),
      Stratified2StrataPool.getName(n, nD),
      lRuns,
      qRels)
  }

  def repoolToUnsupportedBased(cmd: String, lRuns: List[Runs], qRels: QRels): QRels = {
    val tDocs = UnsupportedBasedPool.getPooledDocuments(cmd, lRuns, qRels)
    repoolTo(
      tDocs,
      UnsupportedBasedPool.getName(cmd),
      lRuns,
      qRels)
  }

  def repoolToUnsupportedFixedSizeBased(cmd: String, topicSizes: Map[Int, Int], lRuns: List[Runs], qRels: QRels): QRels = {
    val tDocs = UnsupportedFixedSizeBasedPool.getPooledDocuments(cmd, topicSizes, lRuns, qRels)
    repoolTo(
      tDocs,
      UnsupportedBasedPool.getName(cmd),
      lRuns,
      qRels)
  }

  def repoolToUniformSampled(n: Int, rate: Float, lRuns: List[Runs], qRels: QRels): QRels = {
    repoolTo(
      UniformSampledPool.getPooledDocuments(n, rate, lRuns),
      UniformSampledPool.getName(n, rate),
      lRuns,
      qRels)
  }

  def repoolTo(getDocuments: (Int) => Set[Document], name: String, lRuns: List[Runs], qRels: QRels): QRels = {
    def md5(s: String) = {
      val m = java.security.MessageDigest.getInstance("MD5")
      val b = s.getBytes("UTF-8")
      m.update(b, 0, b.length)
      new java.math.BigInteger(1, m.digest()).toString(16)
    }

    //val qRelsKey = md5(qRels.topicIds.toList.sorted.mkString(","))
    //val lRunsKey = md5(lRuns.map(_.id).sorted.mkString(","))
    //val key = name + "@" + lRunsKey + "@" + qRelsKey
    //sync.caching(key) {
    val nQRels =
    for (qRel <- if (qRels.isInstanceOf[InteractiveQRels]) InteractiveQRels.par(qRels.qRels) else qRels.qRels.par) yield {
      val docs = getDocuments(qRel.id)
      qRels.complete(qRel.id)
      val iteration = qRel.qrelRecords.head.iteration
      val nullQRelRecords = docs.filterNot(d => qRel.qrelRecords.map(_.document).contains(d)).map(d => new QRelRecord(iteration, d, -1)).toList
      new QRel(qRel.id, qRel.qrelRecords.filter(r => docs.contains(r.document)) ::: nullQRelRecords)
    }
    new QRels(qRels.id + "@" + name, nQRels.seq)
    //}
  }

  def to(toPool: String, pool: Pool): Pool = parseToPool(toPool) match {
    case DepthNOption(n) => pool match {
      case pool: DepthNPool => DepthNPool(n, pool.lRuns, pool.qRels)
    }
    case UniformSampledOption(n, rate) => pool match {
      case pool: DepthNPool => UniformSampledPool(n, rate, pool.lRuns, pool.qRels)
    }
    case StratifiedSampledOption(stratas) => pool match {
      case pool: DepthNPool => StratifiedPool(stratas, pool.lRuns, pool.qRels)
    }
    case Stratified2StrataOption(n, nD) => pool match {
      case pool: DepthNPool => Stratified2StrataPool(n, nD, pool.lRuns, pool.qRels)
    }
    case RBPBasedOption(m, p, nD) => pool match {
      case pool: DepthNPool => RBPBasedPool(m, p, nD, pool.lRuns, pool.qRels)
    }
    case MABBasedOption(m, c1, c2, nD) => pool match {
      case pool: DepthNPool => MABBasedPool(m, c1, c2, nD, pool.lRuns, pool.qRels)
      case pool: Pool => MABBasedPool(m, c1, c2, nD, pool.lRuns, pool.qRels)
    }
    case DetailedMABBasedOption(m, c1, c2, nDs) => pool match {
      case pool: DepthNPool => new MABBasedPool(m, c1, c2, nDs.values.sum, pool.lRuns, pool.qRels, Option(nDs))
      case pool: Pool => new MABBasedPool(m, c1, c2, nDs.values.sum, pool.lRuns, pool.qRels, Option(nDs))
    }
    case DepthNMABBasedOption(m, depth, c1, c2, nD) => pool match {
      case pool: DepthNPool => DepthNMABBasedPool(m, depth, c1, c2, nD, pool.lRuns, pool.qRels)
      case pool: Pool => DepthNMABBasedPool(m, depth, c1, c2, nD, pool.lRuns, pool.qRels)
    }
    case RRFBasedOption(k, nD) => pool match {
      case pool: DepthNPool => RRFBasedPool(k, nD, pool.lRuns, pool.qRels)
    }
    case DCGBasedOption(nD) => pool match {
      case pool: DepthNPool => DCGBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case BordaBasedOption(nD) => pool match {
      case pool: DepthNPool => BordaBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case CondorcetBasedOption(nD) => pool match {
      case pool: DepthNPool => CondorcetBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case MTFBasedOption(nD) => pool match {
      case pool: DepthNPool => MTFBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case DetailedMTFBasedOption(nDs) => pool match {
      case pool: DepthNPool => new MTFBasedPool(nDs.values.sum, pool.lRuns, pool.qRels, Option(nDs))
      case pool: Pool => new MTFBasedPool(nDs.values.sum, pool.lRuns, pool.qRels, Option(nDs))
    }
    case DepthNMTFBasedOption(n, nD) => pool match {
      case pool: DepthNPool => DepthNMTFBasedPool(n, nD, pool.lRuns, pool.qRels)
    }
    case UnsupportedBasedOption(cmd) => pool match {
      case pool: DepthNPool => UnsupportedBasedPool(cmd, pool.lRuns, pool.qRels)
    }
    case UnsupportedFixedSizeBasedOption(cmd, poolSize) => pool match {
      case pool: DepthNPool => UnsupportedFixedSizeBasedPool(cmd, poolSize, pool.lRuns, pool.qRels)
    }
    case HedgeBasedOption(beta, nD) => pool match {
      case pool: DepthNPool => HedgeBasedPool(beta, nD, pool.lRuns, pool.qRels)
    }
    case DepthNHedgeBasedOption(depth, beta, nD) => pool match {
      case pool: DepthNPool => DepthNHedgeBasedPool(depth, beta, nD, pool.lRuns, pool.qRels)
    }
    case FusionBasedOption(m, nD) => pool match {
      case pool: DepthNPool => FusionBasedPool(m, nD, pool.lRuns, pool.qRels)
    }
    case null => pool
  }

  // [151->100,152->100,...]
  def parseTopicSizes(str: String): Map[Int, Int] = {
    str.dropRight(1).drop(1).split(",").map(e => {
      val es = e.split("->");
      es.head.toInt -> es.last.toInt
    }).toMap
  }

  /**
    * Commands:
    * depth_n
    * uniformsampled_n:rate
    * stratifiedsampled[_n:rate]+
    *
    * @param toPool
    */
  def parseToPool(toPool: String): PoolCommand = if (toPool.nonEmpty) {
    val sToPool = toPool.split("_")
    val cmd = sToPool.head.toLowerCase()
    if (cmd == "depth") {
      val n = sToPool(1).toInt
      DepthNOption(n)
    } else if (cmd == "sampleddepth") {
      val args = sToPool(1).split(":")
      val n = args(0).toInt
      val rate = args(1).toFloat
      UniformSampledOption(n, rate)
    } else if (cmd == "stratified") {
      val args = sToPool.drop(1).map(_.split(":"))
      val stratas = args.map(e => Strata(e(0).toInt, e(1).toFloat)).toList
      StratifiedSampledOption(stratas)
    } else if (cmd == "stratified2strata") {
      // 10:3
      val args = sToPool(1).split(":")
      val n = args(0).toInt
      val nD = args(1).toInt
      Stratified2StrataOption(n, nD)
    } else if (cmd == "rbpbased") {
      // rbpBased_A:0.8:3
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      val p = args(1).toFloat
      val nD = args(2).toInt
      RBPBasedOption(m, p, nD)
    } else if (cmd == "dcgbased") {
      // dcgBased_10000
      val nD = sToPool(1).toInt
      DCGBasedOption(nD)
    } else if (cmd == "mabbased") {
      // mbaBased_greedy:0:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase() // random,greedy,ucb,bla
      if (m == "greedy") {
        val c1 = args(1).toFloat
        val c2 = args(2).toFloat
        val nD = args(3).toInt
        MABBasedOption(m, c1, c2, nD)
      } else {
        if (args(1).startsWith("[")) {
          val nDs = parseTopicSizes(args(1))
          DetailedMABBasedOption(m, 0f, 0f, nDs)
        } else {
          val nD = args(1).toInt
          MABBasedOption(m, 0f, 0f, nD)
        }
      }
    } else if (cmd == "depthnmabbased") {
      // mbaBased_greedy:0:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase() // random,greedy,ucb,bla
      if (m == "greedy") {
        val depth = args(1).toInt
        val c1 = args(2).toFloat
        val c2 = args(3).toFloat
        val nD = args(4).toInt
        DepthNMABBasedOption(m, depth, c1, c2, nD)
      } else {
        val depth = args(1).toInt
        val nD = args(2).toInt
        DepthNMABBasedOption(m, depth, 0f, 0f, nD)
      }
    } else if (cmd == "rrfbased") {
      // rffBased_A:60:3
      val args = sToPool(1).split(":")
      val k = args(0).toInt
      val nD = args(1).toInt
      RRFBasedOption(k, nD)
    } else if (cmd == "fusionbased") {
      // fusionbased_combmed:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      val nD = args(1).toInt
      FusionBasedOption(m, nD)
    } else if (cmd == "bordabased") {
      // bordaBased:10000
      val nD = sToPool(1).toInt
      BordaBasedOption(nD)
    } else if (cmd == "condorcetbased") {
      // condorcetbased:10000
      val args = sToPool(1).split(":")
      val nD = args(0).toInt
      CondorcetBasedOption(nD)
    } else if (cmd == "hedgebased") {
      // hedgeBased_0.1:10000
      val args = sToPool(1).split(":")
      val beta = args(0).toDouble
      val nD = args(1).toInt
      HedgeBasedOption(beta, nD)
    } else if (cmd == "depthnhedgebased") {
      // depthnhedgebased_10:0.1:10000
      val args = sToPool(1).split(":")
      val depth = args(0).toInt
      val beta = args(1).toDouble
      val nD = args(2).toInt
      DepthNHedgeBasedOption(depth, beta, nD)
    } else if (cmd == "depthnmtfbased") {
      // hedgeBased_10:10000
      val args = sToPool(1).split(":")
      val depth = args(0).toInt
      val nD = args(1).toInt
      DepthNMTFBasedOption(depth, nD)
    } else if (cmd == "mtfbased") {
      // mtfBased:10000
      if (sToPool(1).startsWith("[")) {
        val nDs = parseTopicSizes(sToPool(1))
        DetailedMTFBasedOption(nDs)
      } else {
        val nD = sToPool(1).toInt
        MTFBasedOption(nD)
      }
    } else if (toPool.replaceAll("\"", "").trim().startsWith("$")) {
      if (toPool.replaceAll("\"", "").trim().startsWith("$(topicsizes:")) {
        // $ cmd
        val temp = toPool.replaceAll("\"", "").trim().drop(13).trim()
        val poolSize = temp.split(')').head.toInt
        val cmd = temp.split(')').last.trim()
        UnsupportedFixedSizeBasedOption(cmd, poolSize)
      } else {
        // $ cmd
        val cmd = toPool.replaceAll("\"", "").trim().drop(1).trim()
        UnsupportedBasedOption(cmd)
      }
    } else {
      throw new Exception("toPoolingStrategy: " + toPool + " command not recognized")
    }
  } else {
    null
  }
}

