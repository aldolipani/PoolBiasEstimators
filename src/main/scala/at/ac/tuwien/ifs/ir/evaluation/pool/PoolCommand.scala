package at.ac.tuwien.ifs.ir.evaluation.pool

class PoolCommand

case class DepthNOption(n: Int) extends PoolCommand

case class TakeNOption(poolSize: Int) extends PoolCommand

case class FairTakeNOption(poolSize: Int) extends PoolCommand

case class UniformSampledOption(n: Int, rate: Float) extends PoolCommand

case class Strata(depth: Int, rate: Float)

case class StratifiedSampledOption(stratas: List[Strata]) extends PoolCommand

case class RBPBasedOption(method: String, p: Float, nD: Int) extends PoolCommand

case class DCGBasedOption(nD: Int) extends PoolCommand

case class MABBasedOption(method: String, c1: Float, c2: Float, nD: Int) extends PoolCommand

case class DetailedMABBasedOption(method: String, c1: Float, c2: Float, nDs: Map[Int, Int]) extends PoolCommand

case class DepthNMABBasedOption(method: String, depth: Int, c1: Float, c2: Float, nD: Int) extends PoolCommand

case class DetailedDepthNMABBasedOption(method: String, depth: Int, c1: Float, c2: Float, nDs: Map[Int, Int]) extends PoolCommand

case class RRFBasedOption(k: Int, nD: Int) extends PoolCommand

case class FusionBasedOption(method: String, nD: Int) extends PoolCommand

case class HedgeBasedOption(beta: Double, nD: Int) extends PoolCommand

case class DepthNHedgeBasedOption(depth: Int, beta: Double, nD: Int) extends PoolCommand

case class MTFBasedOption(m: String, par: Map[String, Int], nD: Int) extends PoolCommand

case class DetailedMTFBasedOption(m: String, par: Map[String, Int], nDs: Map[Int, Int]) extends PoolCommand

case class DepthNMTFBasedOption(m: String, par: Map[String, Int], n: Int, nD: Int) extends PoolCommand

case class DetailedDepthNMTFBasedOption(m: String, par: Map[String, Int], n: Int, nDs: Map[Int, Int]) extends PoolCommand

case class BordaBasedOption(collectionSize:Int, nD: Int) extends PoolCommand

case class CondorcetBasedOption(nD: Int) extends PoolCommand

case class UnsupportedBasedOption(cmd: String) extends PoolCommand

case class UnsupportedFixedSizeBasedOption(cmd: String, poolSize: Int) extends PoolCommand

case class Stratified2StrataOption(n: Int, nD: Int) extends PoolCommand