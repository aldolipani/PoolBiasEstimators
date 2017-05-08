package at.ac.tuwien.ifs.ir.evaluation.pool

import java.security.MessageDigest

import at.ac.tuwien.ifs.ir.model._
import com.google.common.cache.CacheBuilder
import scala.collection.parallel.ParSeq
import scalacache._
import scalacache.guava.GuavaCache


/**
  * Created by aldo on 12/09/15.
  */

class PoolCommand
case class DepthNOption(val n: Int) extends PoolCommand
case class UniformSampledOption(val n: Int, val rate: Float) extends PoolCommand
case class Strata(val depth: Int, val rate: Float)
case class StratifiedSampledOption(val stratas: List[Strata]) extends PoolCommand
case class RBPBasedOption(val method: String, val p: Float, nD: Int) extends PoolCommand
case class DCGBasedOption(nD: Int) extends PoolCommand
case class MABBasedOption(val method: String, val c1: Float, c2: Float, nD: Int) extends PoolCommand
case class RRFBasedOption(val method: String, val k: Int, nD: Int) extends PoolCommand
case class FusionBasedOption(val method: String, nD: Int) extends PoolCommand
case class HedgeBasedOption(val beta: Double, nD: Int) extends PoolCommand
case class MTFBasedOption(nD: Int) extends PoolCommand
case class BordaBasedOption(nD: Int) extends PoolCommand
case class CondocertBasedOption(method:String, nD: Int) extends PoolCommand
case class UnsupportedBasedOption(cmd: String) extends PoolCommand
case class UnsupportedFixedSizeBasedOption(cmd: String, poolSize:Int) extends PoolCommand
case class Stratified2StrataOption(n: Int, nD: Int) extends PoolCommand

object PoolConverter {

  val underlyingGuavaCache = CacheBuilder.newBuilder().maximumSize(10000L).build[String, Object]
  implicit val scalaCache = ScalaCache(GuavaCache(underlyingGuavaCache))

  def repoolToStratified(stratas:List[Strata], lRuns:List[Runs], qRels: QRels): QRels = {
    repoolTo(
      StratifiedPool.getPooledDocuments(stratas, lRuns),
      StratifiedPool.getName(stratas),
      lRuns,
      qRels)
  }

  def repoolToMTFBased(poolSize:Int, lRuns:List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      MTFBasedPool.getPooledDocuments(topicSizes, lRuns, qRels),
      MTFBasedPool.getName(poolSize),
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

  def repoolToCondocertBased(method:String, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      CondocertBasedPool.getPooledDocuments(method, topicSizes, lRuns, qRels),
      CondocertBasedPool.getName(method, poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthN(n: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    repoolTo(
      DepthNPool.getPooledDocuments(n, lRuns),
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

  def repoolToMABBased(method: String, c1: Double, c2: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      MABBasedPool.getPooledDocuments(method, c1, c2, topicSizes, lRuns, qRels),
      MABBasedPool.getName(method, c1, c2, poolSize),
      lRuns,
      qRels)
  }

  def repoolToRRFBased(method: String, k: Int, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      RRFBasedPool.getPooledDocuments(method, k, topicSizes, lRuns, qRels),
      RRFBasedPool.getName(method, k, poolSize),
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

  def repoolToUnsupportedFixedSizeBased(cmd: String, topicSizes:Map[Int,Int], lRuns: List[Runs], qRels: QRels): QRels = {
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

  def repoolTo(getDocuments: (Int) => Set[Document], name:String, lRuns: List[Runs], qRels: QRels): QRels = {
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
        for (qRel <- qRels.qRels.par) yield {
          val docs = getDocuments(qRel.id)
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
    case RRFBasedOption(m, k, nD) => pool match {
      case pool: DepthNPool => RRFBasedPool(m, k, nD, pool.lRuns, pool.qRels)
    }
    case DCGBasedOption(nD) => pool match {
      case pool: DepthNPool => DCGBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case BordaBasedOption(nD) => pool match {
      case pool: DepthNPool => BordaBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case CondocertBasedOption(m, nD) => pool match {
      case pool: DepthNPool => CondocertBasedPool(m, nD, pool.lRuns, pool.qRels)
    }
    case MTFBasedOption(nD) => pool match {
      case pool: DepthNPool => MTFBasedPool(nD, pool.lRuns, pool.qRels)
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
    case FusionBasedOption(m, nD) => pool match {
      case pool: DepthNPool => FusionBasedPool(m, nD, pool.lRuns, pool.qRels)
    }
    case null => pool
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
      new DepthNOption(n)
    } else if (cmd == "sampleddepth") {
      val args = sToPool(1).split(":")
      val n = args(0).toInt
      val rate = args(1).toFloat
      new UniformSampledOption(n, rate)
    } else if (cmd == "stratified") {
      val args = sToPool.drop(1).map(_.split(":"))
      val stratas = args.map(e => new Strata(e(0).toInt, e(1).toFloat)).toList
      new StratifiedSampledOption(stratas)
    } else if (cmd == "stratified2strata") {
      // 10:3
      val args = sToPool(1).split(":")
      val n = args(0).toInt
      val nD = args(1).toInt
      new Stratified2StrataOption(n, nD)
    } else if (cmd == "rbpbased") {
      // rbpBased_A:0.8:3
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      val p = args(1).toFloat
      val nD = args(2).toInt
      new RBPBasedOption(m, p, nD)
    } else if (cmd == "dcgbased") {
      // dcgBased_10000
      val nD = sToPool(1).toInt
      new DCGBasedOption(nD)
    } else if (cmd == "mabbased") {
      // mbaBased_greedy:0:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase() // random,greedy,ucb,bla
      if(m == "greedy") {
        val c1 = args(1).toFloat
        val c2 = args(2).toFloat
        val nD = args(3).toInt
        new MABBasedOption(m, c1, c2, nD)
      }else{
        val nD = args(1).toInt
        new MABBasedOption(m, 0f, 0f, nD)
      }
    } else if (cmd == "rrfbased") {
      // rffBased_A:60:3
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      val k = args(1).toInt
      val nD = args(2).toInt
      new RRFBasedOption(m, k, nD)
    } else if (cmd == "fusionbased") {
      // fusionbased_combmed:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      val nD = args(1).toInt
      new FusionBasedOption(m, nD)
    } else if (cmd == "bordabased") {
      // bordaBased:10000
      val nD = sToPool(1).toInt
      new BordaBasedOption(nD)
    } else if (cmd == "condocertbased") {
      // condocertBased_a:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      val nD = args(1).toInt
      new CondocertBasedOption(m, nD)
    } else if (cmd == "hedgebased") {
      // hedgeBased_0.1:10000
      val args = sToPool(1).split(":")
      val beta = args(0).toDouble
      val nD = args(1).toInt
      new HedgeBasedOption(beta, nD)
    } else if (cmd == "mtfbased") {
      // mtfBased:10000
      val nD = sToPool(1).toInt
      new MTFBasedOption(nD)
    } else if (toPool.replaceAll("\"", "").trim().startsWith("$")) {
      if(toPool.replaceAll("\"", "").trim().startsWith("$(topicsizes:")) {
        // $ cmd
        val temp = toPool.replaceAll("\"", "").trim().drop(13).trim()
        val poolSize = temp.split(')').head.toInt
        val cmd = temp.split(')').last.trim()
        new UnsupportedFixedSizeBasedOption(cmd, poolSize)
      } else {
        // $ cmd
        val cmd = toPool.replaceAll("\"", "").trim().drop(1).trim()
        new UnsupportedBasedOption(cmd)
      }
    } else {
      throw new Exception("toPoolingStrategy: " + toPool + " command not recognized")
    }
  } else {
    null
  }
}

