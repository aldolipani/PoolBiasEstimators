package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.io.TXTFile
import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.evaluation.pool.PoolAnalyzerType.PoolAnalyzerType
import at.ac.tuwien.ifs.ir.evaluation.pool._
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.PrintOut
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.PrintOut._
import at.ac.tuwien.ifs.ir.model.{Descs, QRels, Runs}

/**
 * Created by aldo on 10/10/14.
 */

class Analysis(val metrics:List[String], val estimators:List[String]) extends Command{


  def computeAll(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File, l1xo: L1xo, top75Runs: Boolean, toPool:String, sizeRuns:Int, poolAnalyzerType:String) {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir, toPool, poolAnalyzerType)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) Descs.fromXMLDesc(descRunsFile) else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val pool = new Pool(lRuns, qRels)
    printPoolProperties(pool, poolAnalyzerType)
    //WARNING: input must be a DepthNPool
    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Converting Original Pool to depth_" + poolAnalyser.d)
    val nlRuns =
      if(sizeRuns > 0) {
        println("Resizing Pooled Runs to " + sizeRuns)
        RunsTransformer.resizeRuns(sizeRuns, poolAnalyser.fixedPooledRuns)
      } else
        poolAnalyser.pooledRuns
    val depthNPool = DepthNPool(poolAnalyser.d, nlRuns, pool.qRels)
    //*****
    System.setProperty("pool.depth", poolAnalyser.d + "")
    //*****
    printConvertedPoolProperties(depthNPool, poolAnalyzerType)


    val nPool = PoolConverter.to(toPool, depthNPool)
    if(toPool!="") {
      printNewPoolProperties(nPool, poolAnalyzerType)
      printCrossPoolProperties(depthNPool, nPool)
    }

    for (metric <- metrics){
      if(!top75Runs)
        computeRuns(depthNPool, nPool, descs, metric, pValuesDir, l1xo, PrintOut.all)
      else
        compute75TopRuns(depthNPool, nPool, descs, metric, pValuesDir, l1xo, PrintOut.all)
    }
  }

  def analyzePool(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File, l1xo: L1xo, top75Runs: Boolean, toPool:String, poolAnalyzerType: String) {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir, toPool, poolAnalyzerType)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) Descs.fromXMLDesc(descRunsFile) else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val pool = new Pool(lRuns, qRels)
    printPoolProperties(pool, poolAnalyzerType)
    //WARNING: input must be a DepthNPool
    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    val depthNPool = DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, pool.qRels)
    //*****
    System.setProperty("pool.depth", poolAnalyser.d + "")
    //*****
    def f(d: Double) = "%1.2f" format d

    //Print Distribution
    val nPoolAnalyzer = PoolAnalyzer(depthNPool, poolAnalyzerType)

    //println("all:")
    //println(nPoolAnalyzer.getNumDocumentsPerQuery.map(l => l._1 + "\t" + f(Stats.mean(l._2)) + "\t" + f(Stats.sd(l._2)) + "\t" + l._2.mkString(", ")).mkString("\n"))
    //val notRel = nPoolAnalyzer.getNumRelDocuments
    //println("all\t" + f(Stats.mean(notRel)) + "\t" + f(Stats.sd(notRel)) + "\t" + notRel.mkString(", ") + "\n")

    //println("rel:")
    //println(nPoolAnalyzer.getNumRelDocumentsPerQuery.map(l => l._1 + "\t" + f(Stats.mean(l._2)) + "\t" + f(Stats.sd(l._2)) + "\t" + l._2.mkString(", ")).mkString("\n"))
    //val rel = nPoolAnalyzer.getNumRelDocuments
    //println("all\t" + f(Stats.mean(rel)) + "\t" + f(Stats.sd(rel)) + "\t" + rel.mkString(", ") + "\n")

    //println(nPoolAnalyzer.getRatioRelOverAllDocumentsPerQuery.map(l => l._1 + "\t" + f(Stats.mean(l._2)) + "\t" + f(Stats.sd(l._2)) + "\t" + l._2.mkString(", ")).mkString("\n"))
    val relall = nPoolAnalyzer.getRatioRelOverAllDocuments2(descs, 5)
    //val relall = nPoolAnalyzer.getRatioRelOverAllDocuments(5)
    println("all\t" + /*f(Stats.mean(relall)) + "\t" + f(Stats.sd(relall)) + "\t" + */ relall.mkString(", ") + "\n")

    val eAll = nPoolAnalyzer.getEDocuments(descs)
    println("all\t" + /*f(Stats.mean(relall)) + "\t" + f(Stats.sd(relall)) + "\t" + */ eAll.mkString(", ") + "\n")

    //println(nPoolAnalyzer.getRatioRelOverAll2DocumentsPerQuery(descs).map(l => l._1 + "\t" + f(Stats.mean(l._2)) + "\t" + f(Stats.sd(l._2)) + "\t" + l._2.mkString(", ")).mkString("\n"))
    //val relall2 = nPoolAnalyzer.getRatioRelOverAll2Documents(descs)
    //println("all\t" + f(Stats.mean(relall2)) + "\t" + f(Stats.sd(relall2)) + "\t" + relall2.mkString(", ") + "\n")

    //println("not_rel:")
    //println(nPoolAnalyzer.getNumNotRelDocumentsPerQuery.map(l => l._1 + "\t" + f(Stats.mean(l._2)) + "\t" + f(Stats.sd(l._2)) + "\t" + l._2.mkString(", ")).mkString("\n"))
    //val all = nPoolAnalyzer.getNumNotRelDocuments
    //println("all\t" + f(Stats.mean(all)) + "\t" + f(Stats.sd(all)) + "\t" + all.mkString(", ") + "\n")
    //println(nPoolAnalyzer.getRatioNotRelOverAllDocumentsPerQuery.map(l => l._1 + "\t" + f(Stats.mean(l._2)) + "\t" + f(Stats.sd(l._2)) + "\t" + l._2.mkString(", ")).mkString("\n"))
    //val ratall = nPoolAnalyzer.getRatioNotRelOverAllDocuments
    //println("all\t" + f(Stats.mean(ratall)) + "\t" + f(Stats.sd(ratall)) + "\t" + ratall.mkString(", ") + "\n")
    /*val nPool = PoolConverter.to(toPool, depthNPool)
    if(toPool!="") printNewPoolProperties(nPool)

    for (metric <- metrics){
      if(!top75Runs)
        computeRuns(nPool, descs, metric, pValuesDir, l1xo, PrintOut.all)
      else
        compute75TopRuns(nPool, descs, metric, pValuesDir, l1xo, PrintOut.all)
    }*/
  }

  def computeEstimatesAll(trecRelFile: File, trecRunsDir: File, trecRunFile:File, descRunsFile: File, pValuesDir: File, toPool:String, poolAnalyzerType: String) {
    printParameters(trecRelFile, trecRunsDir, trecRunFile, toPool,poolAnalyzerType)

    val lRuns = getListRuns(trecRunsDir)
    val runs = getRuns(trecRunFile)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) Descs.fromXMLDesc(descRunsFile) else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val pool = new Pool(lRuns, qRels)
    printPoolProperties(pool, poolAnalyzerType)

    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Converting Original Pool to depth_" + poolAnalyser.d)
    val depthNPool = new DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, pool.qRels)
    printConvertedPoolProperties(depthNPool, poolAnalyzerType)
    //*****
    System.setProperty("pool.depth", poolAnalyser.d + "")
    //*****
    val nPool = PoolConverter.to(toPool, depthNPool)
    if(toPool!="") {
      printNewPoolProperties(nPool, poolAnalyzerType)
      printCrossPoolProperties(depthNPool, nPool)
    }

    for (metric <- metrics){
      computeEstimatesForRuns(nPool, runs, metric, pValuesDir)
      computeEstimatesFor75TopRuns(nPool, runs, metric, pValuesDir)
    }
  }

  def computeOnlyErrors(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File, l1xo: L1xo, top75Runs: Boolean = false, poolAnalyzerType: String) = {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir, "", poolAnalyzerType)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) Descs.fromXMLDesc(descRunsFile) else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val pool = new Pool(lRuns, qRels)
    printPoolProperties(pool, poolAnalyzerType)

    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Converting Original Pool to depth_" + poolAnalyser.d)
    val depthNPool = new DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, pool.qRels)
    printConvertedPoolProperties(depthNPool, poolAnalyzerType)
    //*****
    System.setProperty("pool.depth", poolAnalyser.d + "")
    //*****
    for (metric <- metrics) {
      if (!top75Runs) computeRuns(depthNPool, pool, descs, metric, pValuesDir, l1xo, PrintOut.onlyErrors)
      else compute75TopRuns(depthNPool, pool, descs, metric, pValuesDir, l1xo, PrintOut.onlyErrors)
    }
  }

  def computeOnlyRuns(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File, l1xo: L1xo, top75Runs: Boolean = false, poolAnalyzerType: String) = {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir, "",poolAnalyzerType)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) {
      Descs.fromXMLDesc(descRunsFile)
    } else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val pool = new Pool(lRuns, qRels)
    printPoolProperties(pool, poolAnalyzerType)

    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Converting Original Pool to depth_" + poolAnalyser.d)
    val depthNPool = new DepthNPool(poolAnalyser.d, poolAnalyser.pooledRuns, pool.qRels)
    printConvertedPoolProperties(depthNPool, poolAnalyzerType)
    //*****
    System.setProperty("pool.depth", poolAnalyser.d + "")
    //*****
    for (metric <- metrics) {
      if (!top75Runs) computeRuns(depthNPool, pool, descs, metric, pValuesDir, l1xo, PrintOut.onlyRuns)
      else compute75TopRuns(depthNPool, pool, descs, metric, pValuesDir, l1xo, PrintOut.onlyRuns)
    }
  }

  private def isOnlyRuns(printOut:PrintOut):Boolean = printOut == onlyRuns || printOut == all

  private def isOnlyErrors(printOut:PrintOut):Boolean = printOut == onlyRuns || printOut == all

  private def isOnlyAnalysis(printOut:PrintOut):Boolean = printOut == onlyAnalysis || printOut == all

  private def isL1ro(l1xo:L1xo):Boolean = l1xo == run || l1xo == both

  private def isL1oo(l1xo:L1xo):Boolean = l1xo == organization || l1xo == both


  private def computeRuns(gPool:DepthNPool, pool:Pool, descs: Descs, metric: String, pValuesDir: File, l1xo: L1xo = L1xo.both, printOut:PrintOut = PrintOut.all) = {
    compute(gPool, pool, descs, metric, pValuesDir, l1xo, printOut)
  }

  private def computeEstimatesForRuns(pool:Pool, runs:Runs, metric: String, pValuesDir: File) = {
    computeEstimates(pool:Pool, runs, metric, pValuesDir)
  }

  private def get75TopLRuns(pool:Pool, metric: String): List[Runs] = {
    ScoreEstimator.excludeRunsByIDs(
      TRECEval().getScores(pool.qRels, pool.lRuns, metric).sortBy(-_.score)
        .drop(Math.round(pool.lRuns.size * 0.75f))
        .map(_.runId), pool.lRuns)
  }

  private def compute75TopRuns(gPool:DepthNPool, pool:Pool, descs: Descs, metric: String, pValuesDir: File, l1xo: L1xo = L1xo.both, printOut:PrintOut = PrintOut.all) = {
    val pooled75TopRuns = get75TopLRuns(gPool, metric)
    println("Only 75% Top Best Runs: " + pooled75TopRuns.size)
    val nPool = pool.getNewInstance(pooled75TopRuns)
    val nGPool = gPool.getNewInstance(pooled75TopRuns).asInstanceOf[DepthNPool]
    compute(nGPool, nPool, descs, metric, pValuesDir, l1xo, printOut)
  }

  private def computeEstimatesFor75TopRuns(pool:Pool, runs: Runs, metric: String, pValuesDir: File) = {
    val pooled75TopRuns = get75TopLRuns(pool, metric)
    println("Only 75% Top Best Runs: " + pooled75TopRuns.size)
    val nPool = pool.getNewInstance(pooled75TopRuns)
    computeEstimates(nPool, runs, metric, pValuesDir)
  }

  private def compute(gPool:DepthNPool, pool:Pool, descs: Descs, metric: String, pValuesDir: File, l1xo:L1xo, printOut:PrintOut = PrintOut.all) = {
    println("Metric\t" + metric)
    val estimators = getEstimators(gPool, pool, descs, metric)

    if (isL1ro(l1xo))
      computeL1ro(estimators, metric, pValuesDir, printOut)

    if (descs != null && isL1oo(l1xo))
      computeL1oo(estimators, metric, pValuesDir, printOut)
  }

  private def computeEstimates(pool:Pool, runs:Runs, metric: String, pValuesDir: File) = {
    println("Metric\t" + metric)
    val estimators = getEstimators(pool.asInstanceOf[DepthNPool], pool, descs = null, metric)
    estimators.map(_.printScore(runs))
    println("")
  }

  private def computeL1ro(estimators: List[ScoreEstimator], metric:String, pValuesDir: File, printOut:PrintOut = all) = {
    println("Leave one RUN out approach")

    if(isOnlyRuns(printOut))
      printReportScores(estimators, L1xo.run)

    if(isOnlyErrors(printOut)) {
      val scoresError = new ScoresError(
        estimators.filter(_.isInstanceOf[TrueEstimator]).head.getAllScores(L1xo.run),
        estimators.filter(_.isInstanceOf[TrueEstimator]).head.getAllScoresPerQuery(L1xo.run),
        pValuesDir,
        metric)
      for(e <- estimators.filter(!_.isInstanceOf[TrueEstimator]))
        scoresError.printReportErrors(e, L1xo.run)
    }
  }

  private def computeL1oo(estimators: List[ScoreEstimator], metric:String, pValuesDir: File, printOut:PrintOut = all) = {
    println("Leave one ORGANIZATION out approach")

    if(isOnlyRuns(printOut))
      printReportScores(estimators, L1xo.organization)

    if(isOnlyErrors(printOut)) {
      val scoresError = new ScoresError(
        estimators.filter(_.isInstanceOf[TrueEstimator]).head.getAllScores(L1xo.organization),
        estimators.filter(_.isInstanceOf[TrueEstimator]).head.getAllScoresPerQuery(L1xo.organization),
        pValuesDir,
        metric)

      estimators.filter(!_.isInstanceOf[TrueEstimator]).map(e =>
        scoresError.printReportErrors(e, L1xo.organization))
    }

    /*if(isOnlyAnalysis(printOut)){
      val poolEstimator = estimators.filter(_.isInstanceOf[PoolEstimator]).head
      val otherEstimators = estimators.filter(e => !e.isInstanceOf[PoolEstimator] && !e.isInstanceOf[TrueEstimator])

      val biasAnalyser = new BiasAnalyser(
        estimators.filter(_.isInstanceOf[TrueEstimator]).head,
        L1xo.organization,
        pValuesDir,
        metric)

      otherEstimators.map(e =>
        biasAnalyser.printReportAnalysis(poolEstimator.asInstanceOf[PoolEstimator], e, L1xo.organization))
    }*/
  }

  private def getEstimators(oPool:DepthNPool, pool:Pool, descs: Descs, metric: String) =
      List(TrueEstimator(oPool, metric, descs)) :::
        List(
          TrueEstimatorQB(oPool, metric, descs),                              //TrueQB
          PoolEstimator(pool, metric, descs),                                 //Pool,,LipaniQB
          PoolEstimatorQB(pool, metric, descs),                               //PoolQB
          WebberOnRunsEstimator(pool, metric, descs),                         //WebberOnRuns
          WebberOnRunsEstimator(pool, metric, descs, L1xo.organization),      //WebberOnRunsL1OO
          WebberOnRunsEstimatorV2(pool, metric, descs),                       //WebberOnRunsV2 - CIKM
          WebberOnRunsEstimatorV2(pool, metric, descs, L1xo.organization),    //WebberOnRunsV2L1OO - CIKM
          WebberOnRunsEstimatorV2QB(pool, metric, descs),                     //WebberOnRunsV2QB
          WebberOnRunsEstimatorV2QB(pool, metric, descs, L1xo.organization),  //WebberOnRunsV2QBL1OO
          WebberOnRunsEstimatorV3(pool, metric, descs),                       //WebberOnRunsV3 - CIKM
          WebberOnRunsEstimatorV3(pool, metric, descs, L1xo.organization),    //WebberOnRunsV3L1OO - CIKM
          WebberOnRunsEstimatorV3QB(pool, metric, descs),                     //WebberOnRunsV3QB
          WebberOnRunsEstimatorV3QB(pool, metric, descs, L1xo.organization),  //WebberOnRunsV3QBL1OO
          LipaniEstimator(pool, metric, descs),                               //Lipani - SIGIR
          LipaniEstimatorQB(pool, metric, descs)                              //LipaniQB
          //LipaniEstimatorV2QB(pool, metric, descs)
          //LipaniEstimatorV2(pool, metric, descs),
          //LipaniEstimatorV3(pool, metric, descs)
        ).filter(e => e.isMetricSupported(metric) && estimators.contains(e.getName))


  // Printing Methods

  private def printTestCollectionProperties(lRuns: List[Runs], qRels: QRels, descs: Descs = null) = {
    def printTestCollectionProperty(name:String, value:String) = System.out.format("%-15s\t%s\n", name, value)
    println("Test Collection Properties")
    printTestCollectionProperty("num_of_runs", lRuns.size.toString)
    printTestCollectionProperty("num_of_topics", qRels.sizeTopics.toString)
    if (descs != null) printTestCollectionProperty("num_of_organizations", descs.numberOfOrganizations.toString)
    println("")
  }

  private def printPoolProperties(pool: Pool, poolAnalyzerType: String) = {
    def printPoolProperty[A](name:String, value:A) = System.out.format("%-15s\t%s\n", name, value.toString)
    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Pool Properties")
    printPoolProperty("depth_of_pool", poolAnalyser.d)
    printPoolProperty("num_runs", poolAnalyser.pool.lRuns.size)
    printPoolProperty("num_pooled_runs", poolAnalyser.pooledRuns.size)
    printPoolProperty("num_judjed_docs", pool.qRels.size)
    printPoolProperty("num_judjed_rel_docs", pool.qRels.sizeRel)
    println("")
  }

  private def printConvertedPoolProperties(pool: Pool, poolAnalyzerType: String) = {
    def printPoolProperty[A](name:String, value:A) = System.out.format("%-15s\t%s\n", name, value.toString)
    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Converted Pool Properties")
    printPoolProperty("depth_of_pool", poolAnalyser.d)
    printPoolProperty("num_runs", poolAnalyser.pool.lRuns.size)
    printPoolProperty("num_pooled_runs", poolAnalyser.pooledRuns.size)
    printPoolProperty("num_judjed_docs", pool.qRels.size)
    printPoolProperty("num_judjed_rel_docs", pool.qRels.sizeRel)
    printPoolProperty("num_non_judjed_docs", pool.qRels.size - pool.qRels.sizeRel - pool.qRels.sizeNotRel)
    println("")
  }

  private def printNewPoolProperties(pool: Pool, poolAnalyzerType: String) = {
    def printPoolProperty[A](name:String, value:A) = System.out.format("%-15s\t%s\n", name, value.toString)
    val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
    println("Pool Properties")
    printPoolProperty("depth_of_pool", poolAnalyser.d)
    printPoolProperty("num_runs", poolAnalyser.pool.lRuns.size)
    printPoolProperty("num_pooled_runs", poolAnalyser.pooledRuns.size)
    printPoolProperty("num_judjed_docs", pool.qRels.size)
    printPoolProperty("num_judjed_rel_docs", pool.qRels.sizeRel)
    printPoolProperty("num_non_judjed_docs", pool.qRels.size - pool.qRels.sizeRel - pool.qRels.sizeNotRel)
    println("")
  }

  private def printCrossPoolProperties(oPool:Pool, cPool:Pool) = {
    println("Cross Pools Properties")
    def printPoolProperty[A](name:String, value:A) = System.out.format("%-15s\t%s\n", name, value.toString)
    printPoolProperty("num_cross_non_judged_docs",
      oPool.qRels.qRels.map(_.qrelRecords.filter(_.rel == -1).map(_.document).toSet).zip(
        cPool.qRels.qRels.map(_.qrelRecords.filter(_.rel == -1).map(_.document).toSet))
        .map(e => e._1.intersect(e._2).size).sum)
    println("")
  }

  private def printParameters(trecRelFile: File, trecRunsDir:File, descRunsFile:File, pValuesDir:File, toPool:String,poolAnalyzerType:String): Unit ={
    def printParameter(name:String, value:String) = System.out.format("%-15s\t%s\n", name, value)
    println("List Parameters")
    printParameter("metrics", metrics.mkString(","))
    printParameter("estimators", estimators.mkString(","))
    printParameter("trec_rel_file", trecRelFile.getAbsolutePath)
    printParameter("trec_runs_dir", trecRunsDir.getAbsolutePath)
    if (descRunsFile != null) printParameter("desc_runs_file", descRunsFile.getAbsolutePath)
    if (pValuesDir != null) printParameter("p_values_dir", pValuesDir.getAbsolutePath)
    if (toPool != "") printParameter("toPoolingStrategy", toPool)
    if (poolAnalyzerType != "") printParameter("poolAnalyzerType", poolAnalyzerType)
    println("")
  }


  private def printParameters(trecRelFile: File, trecRunsDir:File, trecRunsFile:File, toPool:String, poolAnalyzerType:String): Unit ={
    def printParameter(name:String, value:String) = System.out.format("%-10s\t%s\n", name, value)
    println("List Parameters")
    printParameter("trec_rel_file", trecRelFile.getAbsolutePath)
    printParameter("trec_runs_dir", trecRunsDir.getAbsolutePath)
    if (trecRunsFile != null) printParameter("trec_runs_file", trecRunsFile.getAbsolutePath)
    if (toPool != "") printParameter("toPoolingStrategy", toPool)
    if (poolAnalyzerType != "") printParameter("poolAnalyzerType", poolAnalyzerType)
    println("")
  }

  private def printReportScores(estimators: List[ScoreEstimator], l1xo: L1xo = L1xo.run) =
    estimators.map(_.printReportScores(l1xo))

}


object Analysis {

  val cutoffs = Seq(5, 10, 15, 20, 30, 100)

  def apply(metrics:String, estimators:String) = {
    val sEstimators = estimators.split(",").distinct.toList
    val sMetrics = metrics.split(",").flatMap(m => if(m.endsWith("*")) cutoffs.map(m.dropRight(1) + _) else List(m)).distinct.toList
    new Analysis(sMetrics, sEstimators)
  }

}