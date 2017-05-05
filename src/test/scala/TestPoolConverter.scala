import at.ac.tuwien.ifs.ir.evaluation.pool._
import at.ac.tuwien.ifs.ir.model.{QRels, Runs}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by aldo on 30/03/16.
 */
class TestPoolConverter extends FlatSpec with Matchers {

    val runs1Input = List(
      Array("1","Q0","document_id_1", "1", "10"),
      Array("1","Q0","document_id_4", "2", "9"),
      Array("1","Q0","document_id_3", "3", "8"),
      Array("1","Q0","document_id_2", "4", "7"),
      Array("1","Q0","document_id_5", "5", "6"),
      Array("1","Q0","document_id_6", "6", "5"))

    val runs2Input = List(
      Array("1","Q0","document_id_2", "1", "10"),
      Array("1","Q0","document_id_3", "2", "9"),
      Array("1","Q0","document_id_4", "3", "8"),
      Array("1","Q0","document_id_5", "4", "7"),
      Array("1","Q0","document_id_6", "6", "5"),
      Array("1","Q0","document_id_1", "5", "6"))

    val runs3Input = List(
      Array("1","Q0","document_id_2", "1", "10"),
      Array("1","Q0","document_id_3", "2", "9"),
      Array("1","Q0","document_id_4", "3", "8"),
      Array("1","Q0","document_id_5", "4", "7"),
      Array("1","Q0","document_id_1", "5", "6"),
      Array("1","Q0","document_id_6", "6", "5"))

    val qRelsInput = List(
      Array("1","Q0","document_id_1", "1"),
      Array("1","Q0","document_id_2", "0"),
      Array("1","Q0","document_id_3", "0"),
      Array("1","Q0","document_id_4", "0"),
      Array("1","Q0","document_id_5", "1"),
      Array("1","Q0","document_id_6", "1"))

    "PoolConverter" should "return a proper depth when converting to depthNPool" in {
      val runs1 = Runs.fromListOfItems(runs1Input, "1")
      val runs2 = Runs.fromListOfItems(runs2Input, "2")
      val runs3 = Runs.fromListOfItems(runs3Input, "3")
      val runs = List(runs1, runs2, runs3)
      val qRels = QRels.fromListOfItems("Test", qRelsInput)

      val poolAnalyser = new PoolAnalyzer(Pool(runs, qRels))
      val pool = new DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, qRels)
      //val nPool = PoolConverter.repoolWith(poolAnalyser.pooledRuns, pool) match {
      //  case pool:DepthNPool => pool
      //}

      //val nPoolAnalyser = new PoolAnalyzer(nPool)

//      nPoolAnalyser.d should be (poolAnalyser.d)
    }

  "PoolConverter" should "return a proper depth when converting to Stratified2Strata" in {
    val runs1 = Runs.fromListOfItems(runs1Input, "1")
    val runs2 = Runs.fromListOfItems(runs2Input, "2")
    val runs3 = Runs.fromListOfItems(runs3Input, "3")
    val runs = List(runs1, runs2, runs3)
    val qRels = QRels.fromListOfItems("Test", qRelsInput)

    val poolAnalyser = new PoolAnalyzer(Pool(runs, qRels))
    val pool = new DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, qRels)
    /*val depthNPool = PoolConverter.repoolWith(poolAnalyser.pooledRuns, pool) match {
      case pool:DepthNPool => pool
    }

    val nD = 3
    val n = 6
    val stratified2StrataPool = PoolConverter.repoolToStratified2Strata(nD, n, depthNPool)
    val fd =Stratified2StrataPool.getFd(nD,n,poolAnalyser.pooledRuns, poolAnalyser.getPool().qRels)
    println(fd)
    println(Stratified2StrataPool.getSr(nD,n,fd, poolAnalyser.pooledRuns, poolAnalyser.getPool().qRels))
    println(stratified2StrataPool.qRels.size)*/
  }

  "PoolConverter" should "return a proper depth when converting to RBPBased" in {
    val runs1 = Runs.fromListOfItems(runs1Input, "1")
    val runs2 = Runs.fromListOfItems(runs2Input, "2")
    val runs3 = Runs.fromListOfItems(runs3Input, "3")
    val runs = List(runs1, runs2, runs3)
    val qRels = QRels.fromListOfItems("Test", qRelsInput)

    val poolAnalyser = new PoolAnalyzer(Pool(runs, qRels))
    val pool = new DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, qRels)
    //val depthNPool = PoolConverter.repoolWith(poolAnalyser.pooledRuns, pool)match {
    //  case pool:DepthNPool => pool
   // }

    val nD = 3
    //val rbpBasedPool = PoolConverter.repoolToRBPBased("Pooling", 0.8, nD, depthNPool)
    //val rbpBasedPool = PoolConverter.repoolToRBPBased("C", 0.8, nD, depthNPool)
    //println(rbpBasedPool.qRels.qRels.mkString(" "))
  }
/*
    "PoolAnalyser" should "should repool a run correctly" in {
      val runs1 = Runs.fromListOfItems(runs1Input)
      val runs2 = Runs.fromListOfItems(runs2Input)
      val runs3 = Runs.fromListOfItems(runs3Input)
      val runs = List(runs1, runs2, runs3)
      val qRels = QRels.fromListOfItems("Test", qRelsInput)

      val poolAnalyser = new PoolAnalyzer(runs, qRels)

      poolAnalyser.repoolWith(List(runs2)).size should be (6)
      poolAnalyser.repoolWith(List(runs3)).size should be (1)
      poolAnalyser.repoolWith(runs).size should be (7)
    }
    */

  }
