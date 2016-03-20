package at.ac.tuwien.ifs.ir.evaluation.pool

import at.ac.tuwien.ifs.ir.model.{QRel, QRels, Runs}

/**
 * Created by aldo on 12/09/15.
 */

class PoolCommand
case class DepthNOption(val n:Int) extends PoolCommand
case class UniformSampledOption(val n:Int, val rate:Float) extends PoolCommand
class Strata(val depth:Int, val rate:Float)
case class StratifiedSampledOption(val stratas:List[Strata]) extends PoolCommand

object PoolConverter {

  //lazy val rnd = new scala.util.Random(1234)

  def repoolToDepthN(n:Int, pool:DepthNPool):DepthNPool = {
    if(n > pool.n) throw new Exception("The depth required is grater then the actual depth")

    val nQRels =
      for (qRel <- pool.qRels.qRels.par if (pool.lRuns.head.selectByTopicId(qRel.id) != null)) yield {
        val docs = DepthNPool.getPooledDocuments(qRel.id)(n, pool.lRuns)
        new QRel(qRel.id, qRel.qrelRecord.filter(r => docs.contains(r.document)))
      }

    new DepthNPool(n, pool.lRuns, new QRels(pool.qRels.id+"@Depth_d@"+n, nQRels.seq))
  }

  def repoolToUniformSampled(n:Int, rate:Float, pool:DepthNPool):UniformSampledPool = {
    if(n > pool.n) throw new Exception("The depth required is grater then the actual depth")

    val nQRels =
      for (qRel <- pool.qRels.qRels.par if (pool.lRuns.head.selectByTopicId(qRel.id) != null)) yield {
        val docs = UniformSampledPool.getPooledDocuments(qRel.id)(n, rate, pool.lRuns)
        new QRel(qRel.id, qRel.qrelRecord.filter(r => docs.contains(r.document)))
      }

    new UniformSampledPool(n, rate, pool.lRuns, new QRels(pool.qRels.id+"@SampledDepth_d@"+n+":"+rate, nQRels.seq))
  }

  def repoolToStratifiedSampled(stratas:List[Strata], pool:DepthNPool):StratifiedSampledPool = {
    if(stratas.map(_.depth).sum > pool.n) throw new Exception("The depth required is grater then the actual depth")

    val nQRels =
      for (qRel <- pool.qRels.qRels.par if (pool.lRuns.head.selectByTopicId(qRel.id) != null)) yield {
        val docs = StratifiedSampledPool.getPooledDocuments(qRel.id)(stratas, pool.lRuns)
        new QRel(qRel.id, qRel.qrelRecord.filter(r => docs.contains(r.document)))
      }

    new StratifiedSampledPool(stratas, pool.lRuns, new QRels(pool.qRels.id+"@Stratified@"+stratas.map(e => e.depth+":"+e.rate).mkString("_"), nQRels.seq))
  }

  def repoolWith(lRuns:List[Runs], pool:Pool):Pool = pool match {
    case pool:DepthNPool => repoolWith(lRuns, pool)
  }

  def repoolWith(lRuns:List[Runs], pool:DepthNPool): DepthNPool = {
    val nQRels =
      for (qRel <- pool.qRels.qRels.par if (lRuns.head.selectByTopicId(qRel.id) != null)) yield {
        val docs = DepthNPool.getPooledDocuments(qRel.id)(pool.n,lRuns)
        new QRel(qRel.id, qRel.qrelRecord.filter(r => docs.contains(r.document)))
      }
    val lRunsId = lRuns.map(_.id(0)).mkString("")
    new DepthNPool(pool.n, lRuns, new QRels(pool.qRels.id+"@"+lRunsId, nQRels.seq))
  }

  def to(toPool:String, pool:Pool):Pool = parseToPool(toPool) match {
    case DepthNOption(n) => pool match {
      case pool:DepthNPool => repoolToDepthN(n, pool)
    }
    case UniformSampledOption(n, rate) => pool match {
      case pool:DepthNPool => repoolToUniformSampled(n, rate, pool)
    }
    case StratifiedSampledOption(stratas) => pool match {
      case pool:DepthNPool => repoolToStratifiedSampled(stratas, pool)
    }
  }

  /**
   * Commands:
   * depth_n
   * uniformsampled_n:rate
   * stratifiedsampled[_n:rate]+
   * @param toPool
   */
  def parseToPool(toPool:String):PoolCommand = {
    val sToPool = toPool.split("_")
    val cmd = sToPool.head.toLowerCase()
    if(cmd == "depth") {
      val n = sToPool(1).toInt
      new DepthNOption(n)
    }else if(cmd == "sampleddepth") {
      val args = sToPool(1).split(":")
      val n = args(0).toInt
      val rate = args(1).toFloat
      new UniformSampledOption(n, rate)
    }else if(cmd == "stratified") {
      val args = sToPool.drop(1).map(_.split(":"))
      val stratas = args.map(e => new Strata(e(0).toInt, e(1).toFloat)).toList
      new StratifiedSampledOption(stratas)
    }else{
      throw new Exception("toPoolingStrategy command not recognized")
    }
  }
}

