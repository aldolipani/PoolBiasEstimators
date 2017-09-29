package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.interactive.InteractiveQRels
import at.ac.tuwien.ifs.ir.model._
import com.google.common.cache.CacheBuilder

import scalacache._
import scalacache.guava.GuavaCache


/**
  * Created by aldo on 12/09/15.
  */


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

  def repoolToMTFBased(m: String, par: Map[String, Int], poolSize: Int, lRuns: List[Runs], qRels: QRels, nDs: Map[Int, Int]): QRels = {
    repoolTo(
      MTFBasedPool.getPooledDocuments(m, par, nDs, lRuns, qRels),
      MTFBasedPool.getName(m, par, poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthNMTFBased(m: String, par: Map[String, Int], depth: Int, poolSize: Int, lRuns: List[Runs], qRels: QRels, nDs: Map[Int, Int]): QRels = {
    repoolTo(
      DepthNMTFBasedPool.getPooledDocuments(m, par, depth, nDs, lRuns, qRels),
      DepthNMTFBasedPool.getName(m, par, depth, poolSize),
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

  def repoolToTakeN(poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      TakeNPool.getPooledDocuments(topicSizes, lRuns, qRels),
      TakeNPool.getName(poolSize),
      lRuns,
      qRels)
  }

  def repoolToFairTakeN(poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      FairTakeNPool.getPooledDocuments(topicSizes, lRuns, qRels),
      FairTakeNPool.getName(poolSize),
      lRuns,
      qRels)
  }

  def repoolToBordaBased(collectionSize: Int, poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      BordaBasedPool.getPooledDocuments(collectionSize, topicSizes, lRuns, qRels),
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

  def repoolToMABBased(method: String, c1: Double, c2: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels, nDs: Map[Int, Int], restore: Boolean): QRels = {
    repoolTo(
      MABBasedPool.getPooledDocuments(method, c1, c2, nDs, lRuns, qRels, restore),
      MABBasedPool.getName(method, c1, c2, poolSize),
      lRuns,
      qRels)
  }

  def repoolToDepthNMABBased(method: String, depth: Int, c1: Double, c2: Double, poolSize: Int, lRuns: List[Runs], qRels: QRels, nDs: Map[Int, Int]): QRels = {
    repoolTo(
      DepthNMABBasedPool.getPooledDocuments(method, depth, c1, c2, nDs, lRuns, qRels),
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

  def repoolToPPBased(poolSize: Int, lRuns: List[Runs], qRels: QRels): QRels = {
    val topicSizes = FixedSizePool.findTopicSizes(poolSize, lRuns, qRels)
    repoolTo(
      PPBasedPool.getPooledDocuments(topicSizes, lRuns, qRels),
      PPBasedPool.getName(poolSize),
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
    /*def md5(s: String) = {
      val m = java.security.MessageDigest.getInstance("MD5")
      val b = s.getBytes("UTF-8")
      m.update(b, 0, b.length)
      new java.math.BigInteger(1, m.digest()).toString(16)
    }*/

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

  def to(toPool: String, pool: Pool, restore: Boolean = false): Pool = parseToPool(toPool) match {
    case DepthNOption(n) => pool match {
      case pool: DepthNPool => DepthNPool(n, pool.lRuns, pool.qRels)
    }
    case TakeNOption(n) => pool match {
      case pool: DepthNPool => TakeNPool(n, pool.lRuns, pool.qRels)
    }
    case FairTakeNOption(n) => pool match {
      case pool: DepthNPool => FairTakeNPool(n, pool.lRuns, pool.qRels)
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
      case pool: DepthNPool => MABBasedPool(m, c1, c2, nD, pool.lRuns, pool.qRels, restore, FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
      case pool: Pool => MABBasedPool(m, c1, c2, nD, pool.lRuns, pool.qRels, restore, FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
    }
    case DetailedMABBasedOption(m, c1, c2, nDs) => pool match {
      case pool: DepthNPool => new MABBasedPool(m, c1, c2, nDs.values.sum, pool.lRuns, pool.qRels, restore, nDs)
      case pool: Pool => new MABBasedPool(m, c1, c2, nDs.values.sum, pool.lRuns, pool.qRels, restore, nDs)
    }
    case DepthNMABBasedOption(m, depth, c1, c2, nD) => pool match {
      case pool: DepthNPool => DepthNMABBasedPool(m, depth, c1, c2, nD, pool.lRuns, pool.qRels, FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
      case pool: Pool => DepthNMABBasedPool(m, depth, c1, c2, nD, pool.lRuns, pool.qRels, FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
    }
    case DetailedDepthNMABBasedOption(m, depth, c1, c2, nDs) => pool match {
      case pool: DepthNPool => DepthNMABBasedPool(m, depth, c1, c2, nDs.values.sum, pool.lRuns, pool.qRels, nDs)
      case pool: Pool => DepthNMABBasedPool(m, depth, c1, c2, nDs.values.sum, pool.lRuns, pool.qRels, nDs)
    }
    case RRFBasedOption(k, nD) => pool match {
      case pool: DepthNPool => RRFBasedPool(k, nD, pool.lRuns, pool.qRels)
    }
    case DCGBasedOption(nD) => pool match {
      case pool: DepthNPool => DCGBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case BordaBasedOption(collectionSize, nD) => pool match {
      case pool: DepthNPool => BordaBasedPool(collectionSize, nD, pool.lRuns, pool.qRels)
    }
    case CondorcetBasedOption(nD) => pool match {
      case pool: DepthNPool => CondorcetBasedPool(nD, pool.lRuns, pool.qRels)
    }
    case MTFBasedOption(m, par, nD) => pool match {
      case pool: DepthNPool => MTFBasedPool(m, par, nD, pool.lRuns, pool.qRels,
        FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
    }
    case DetailedMTFBasedOption(m, par, nDs) => pool match {
      case pool: DepthNPool => MTFBasedPool(m, par, nDs.values.sum, pool.lRuns, pool.qRels, nDs)
      case pool: Pool => MTFBasedPool(m, par, nDs.values.sum, pool.lRuns, pool.qRels, nDs)
    }
    case DepthNMTFBasedOption(m, par, n, nD) => pool match {
      case pool: DepthNPool => DepthNMTFBasedPool(m, par, n, nD, pool.lRuns, pool.qRels,
        FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
      case pool: Pool => DepthNMTFBasedPool(m, par, n, nD, pool.lRuns, pool.qRels,
        FixedSizePool.findTopicSizes(nD, pool.lRuns, pool.qRels))
    }
    case DetailedDepthNMTFBasedOption(m, par, n, nDs) => pool match {
      case pool: DepthNPool => DepthNMTFBasedPool(m, par, n, nDs.values.sum, pool.lRuns, pool.qRels, nDs)
      case pool: Pool => DepthNMTFBasedPool(m, par, n, nDs.values.sum, pool.lRuns, pool.qRels, nDs)
    }
    case UnsupportedBasedOption(cmd) => pool match {
      case pool: DepthNPool => UnsupportedBasedPool(cmd, pool.lRuns, pool.qRels)
    }
    case UnsupportedFixedSizeBasedOption(cmd, poolSize) => pool match {
      case pool: DepthNPool => UnsupportedFixedSizeBasedPool(cmd, poolSize, pool.lRuns, pool.qRels,
        FixedSizePool.findTopicSizes(poolSize, pool.lRuns, pool.qRels))
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
    } else if (cmd == "take") {
      val n = sToPool(1).toInt
      TakeNOption(n)
    } else if (cmd == "fairtake") {
      val n = sToPool(1).toInt
      FairTakeNOption(n)
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
      } else if (m == "maxmean" || m == "beta") {
        if (args(1).startsWith("[")) {
          val nDs = parseTopicSizes(args(1))
          DetailedMABBasedOption(m, 0f, 0f, nDs)
        } else {
          val nD = args(1).toInt
          MABBasedOption(m, 0f, 0f, nD)
        }
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
        if (args(2).startsWith("[")) {
          val nDs = parseTopicSizes(args(1))
          DetailedDepthNMABBasedOption(m, depth, 0f, 0f, nDs)
        } else {
          val nD = args(2).toInt
          DepthNMABBasedOption(m, depth, 0f, 0f, nD)
        }
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
      // bordabased_0:10000
      val args = sToPool(1).split(":")
      val collectionSize = args(0).toInt
      val nD = args(1).toInt
      BordaBasedOption(collectionSize, nD)
    } else if (cmd == "condorcetbased") {
      // condorcetbased_10000
      val nD = sToPool(1).toInt
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
      // depthnmtfbased_standard:10:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      if (m == "standard") {
        val depth = args(1).toInt
        if (args(2).startsWith("[")) {
          val nDs = parseTopicSizes(args(2))
          DetailedDepthNMTFBasedOption(m, Map(), depth, nDs)
        } else {
          val nD = args(2).toInt
          DepthNMTFBasedOption(m, Map(), depth, nD)
        }
      } else { // maxmean
        if (args.size == 3) {
          val depth = args(1).toInt
          if (args(2).startsWith("[")) {
            val nDs = parseTopicSizes(args(2))
            DetailedDepthNMTFBasedOption(m, Map("maxObservedDepth" -> -1), depth, nDs)
          } else {
            val nD = args(2).toInt
            DepthNMTFBasedOption(m, Map("maxObservedDepth" -> -1), depth, nD)
          }
        } else {
          val depth = args(2).toInt
          val maxObservedDepth = args(1).toInt
          if (args(3).startsWith("[")) {
            val nDs = parseTopicSizes(args(3))
            DetailedDepthNMTFBasedOption(m, Map("maxObservedDepth" -> maxObservedDepth), depth, nDs)
          } else {
            val nD = args(3).toInt
            DepthNMTFBasedOption(m, Map("maxObservedDepth" -> maxObservedDepth), depth, nD)
          }
        }
      }
    } else if (cmd == "mtfbased") {
      // mtfBased_standard:10000, mtfbased_maxmean:100:10000
      val args = sToPool(1).split(":")
      val m = args(0).toLowerCase()
      if (m == "standard") {
        if (args(1).startsWith("[")) {
          val nDs = parseTopicSizes(args(1))
          DetailedMTFBasedOption(m, Map(), nDs)
        } else {
          val nD = args(1).toInt
          MTFBasedOption(m, Map(), nD)
        }
      } else { // maxmean
        if (args.size == 2) {
          if (args(1).startsWith("[")) {
            val nDs = parseTopicSizes(args(1))
            DetailedMTFBasedOption(m, Map("maxObservedDepth" -> -1), nDs)
          } else {
            val nD = args(1).toInt
            MTFBasedOption(m, Map("maxObservedDepth" -> -1), nD)
          }
        } else {
          val maxObservedDepth = args(1).toInt
          if (args(2).startsWith("[")) {
            val nDs = parseTopicSizes(args(2))
            DetailedMTFBasedOption(m, Map("maxObservedDepth" -> maxObservedDepth), nDs)
          } else {
            val nD = args(2).toInt
            MTFBasedOption(m, Map("maxObservedDepth" -> maxObservedDepth), nD)
          }
        }
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

