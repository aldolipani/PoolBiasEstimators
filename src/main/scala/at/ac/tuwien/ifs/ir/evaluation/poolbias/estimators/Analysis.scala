package at.ac.tuwien.ifs.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.poolbias.estimators.Analysis.PrintOut.PrintOut
import at.ac.tuwien.io.TXTFile
import at.ac.tuwien.ir.evaluation.{PoolAnalyser, TRECEval}
import at.ac.tuwien.ir.model.{Descs, QRels, Runs}


/**
 * Created by aldo on 10/10/14.
 */
object Analysis {

  lazy val metrics = Seq("P_5", "P_10", "P_15", "P_20", "P_30", "P_100")

  object PrintOut extends Enumeration {
    type PrintOut = Value
    val onlyRuns, onlyErrors, all = Value
  }

  import PrintOut._

  private def isOnlyRuns(printOut:PrintOut):Boolean = printOut == onlyRuns || printOut == all
  private def isOnlyErrors(printOut:PrintOut):Boolean = printOut == onlyErrors || printOut == all

  object L1xo extends Enumeration {
    type L1xo = Value
    val run, organization, both = Value
  }

  import L1xo._

  private def isL1ro(l1xo:L1xo):Boolean = l1xo == run || l1xo == both
  private def isL1oo(l1xo:L1xo):Boolean = l1xo == organization || l1xo == both

  private def printTestCollectionProperties(lRuns: List[Runs], qRels: QRels, descs: Descs = null) = {
    def printTestCollectionProperty(name:String, value:String) = System.out.format("%-15s\t%s\n", name, value)
    println("Test Collection Properties")
    printTestCollectionProperty("num_of_runs", lRuns.size.toString)
    printTestCollectionProperty("num_of_topics", qRels.sizeTopics.toString)
    if (descs != null) printTestCollectionProperty("num_of_organizations", descs.numberOfOrganizations.toString)
    println("")
  }

  private def printPoolProperties(poolAnalyser: PoolAnalyser) = {
    def printPoolProperty(name:String, value:String) = System.out.format("%-15s\t%s\n", name, value)
    println("Pool Properties")
    printPoolProperty("depth_of_pool", poolAnalyser.d.toString)
    printPoolProperty("num_pooled_runs", poolAnalyser.pooledRuns.size.toString)
    println("")
  }

  private def printParameters(trecRelFile: File, trecRunsDir:File, descRunsFile:File, pValuesDir:File): Unit ={
    def printParameter(name:String, value:String) = System.out.format("%-15s\t%s\n", name, value)
    println("List Parameters")
    printParameter("trec_rel_file", trecRelFile.getAbsolutePath)
    printParameter("trec_runs_dir", trecRunsDir.getAbsolutePath)
    if (descRunsFile != null) printParameter("desc_runs_file", descRunsFile.getAbsolutePath)
    if (pValuesDir != null) printParameter("p_values_dir", pValuesDir.getAbsolutePath)
    println("")
  }

  private def printParameters(trecRelFile: File, trecRunsDir:File, trecRunsFile:File): Unit ={
    def printParameter(name:String, value:String) = System.out.format("%-10s\t%s\n", name, value)
    println("List Parameters")
    printParameter("trec_rel_file", trecRelFile.getAbsolutePath)
    printParameter("trec_runs_dir", trecRunsDir.getAbsolutePath)
    if (trecRunsFile != null) printParameter("trec_runs_file", trecRunsFile.getAbsolutePath)
    println("")
  }

  def computeAll(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File) {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) Descs.fromXMLDesc(descRunsFile) else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val poolAnalyser = getPoolAnalyser(qRels, lRuns)
    printPoolProperties(poolAnalyser)

    val pQRels = poolAnalyser.repoolWith(poolAnalyser.pooledRuns)
    for (metric <- metrics){
      computeRuns(pQRels, poolAnalyser.pooledRuns, descs, metric, pValuesDir, L1xo.both, PrintOut.all)
      compute75TopRuns(pQRels, poolAnalyser.pooledRuns, descs, metric, pValuesDir, L1xo.both, PrintOut.all)
    }
  }

  def computeEstimatesAll(trecRelFile: File, trecRunsDir: File, trecRunFile:File, descRunsFile: File, pValuesDir: File) {
    printParameters(trecRelFile, trecRunsDir, trecRunFile)

    val lRuns = getListRuns(trecRunsDir)
    val runs = getRuns(trecRunFile)

    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) Descs.fromXMLDesc(descRunsFile) else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val poolAnalyser = getPoolAnalyser(qRels, lRuns)
    printPoolProperties(poolAnalyser)

    val pQRels = poolAnalyser.repoolWith(poolAnalyser.pooledRuns)
    for (metric <- metrics){
      computeEstimatesForRuns(pQRels, poolAnalyser.pooledRuns, runs, metric, pValuesDir)
      computeEstimatesFor75TopRuns(pQRels, poolAnalyser.pooledRuns, runs, metric, pValuesDir)
    }
  }

  def computeOnlyErrors(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File, l1xo: L1xo, top75Runs: Boolean = false) = {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) {
      Descs.fromXMLDesc(descRunsFile)
    } else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val poolAnalyser = getPoolAnalyser(qRels, lRuns)
    printPoolProperties(poolAnalyser)

    val pQRels = poolAnalyser.repoolWith(poolAnalyser.pooledRuns)
    for (metric <- metrics) {
      if (!top75Runs) computeRuns(pQRels, poolAnalyser.pooledRuns, descs, metric, pValuesDir, l1xo, PrintOut.onlyErrors)
      else compute75TopRuns(pQRels, poolAnalyser.pooledRuns, descs, metric, pValuesDir, l1xo, PrintOut.onlyErrors)
    }
  }

  def computeOnlyRuns(trecRelFile: File, trecRunsDir: File, descRunsFile: File, pValuesDir: File, l1xo: L1xo, top75Runs: Boolean = false) = {
    printParameters(trecRelFile, trecRunsDir, descRunsFile, pValuesDir)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)
    val descs = if (descRunsFile != null) {
      Descs.fromXMLDesc(descRunsFile)
    } else null
    printTestCollectionProperties(lRuns, qRels, descs)

    val poolAnalyser = getPoolAnalyser(qRels, lRuns)
    printPoolProperties(poolAnalyser)

    val pQRels = poolAnalyser.repoolWith(poolAnalyser.pooledRuns)
    for (metric <- metrics) {
      if (!top75Runs) computeRuns(pQRels, poolAnalyser.pooledRuns, descs, metric, pValuesDir, l1xo, PrintOut.onlyRuns)
      else compute75TopRuns(pQRels, poolAnalyser.pooledRuns, descs, metric, pValuesDir, l1xo, PrintOut.onlyRuns)
    }
  }

  private def computeRuns(qRels: QRels, lRuns: List[Runs], descs: Descs, metric: String, pValuesDir: File, l1xo: L1xo = L1xo.both, printOut:PrintOut = PrintOut.all) = {
    compute(qRels, lRuns, descs, metric, pValuesDir, l1xo, printOut)
  }

  private def computeEstimatesForRuns(qRels: QRels, lRuns: List[Runs], runs:Runs, metric: String, pValuesDir: File) = {
    computeEstimates(qRels, lRuns, runs, metric, pValuesDir)
  }

  private def get75TopLRuns(qRels: QRels, lRuns: List[Runs], metric: String): List[Runs] = {
    ScoreEstimator.excludeRunsByIDs(
      TRECEval().getScores(qRels, lRuns, metric).sortBy(-_.score)
        .drop((lRuns.size.toDouble / 100d * 75d).toInt)
        .map(_.runId), lRuns)
  }

  private def compute75TopRuns(qRels: QRels, lRuns: List[Runs], descs: Descs, metric: String, pValuesDir: File, l1xo: L1xo = L1xo.both, printOut:PrintOut = PrintOut.all) = {
    val poolAnalyser = getPoolAnalyser(qRels, lRuns)
    val pooled75TopRuns = get75TopLRuns(qRels, lRuns, metric)
    println("Only 75% Top Best Runs: " + pooled75TopRuns.size)
    val pTopBestRunsQRels = poolAnalyser.repoolWith(pooled75TopRuns)
    compute(pTopBestRunsQRels, pooled75TopRuns, descs, metric, pValuesDir, l1xo, printOut)
  }

  private def computeEstimatesFor75TopRuns(qRels: QRels, lRuns: List[Runs], runs: Runs, metric: String, pValuesDir: File) = {
    val poolAnalyser = getPoolAnalyser(qRels, lRuns)
    val pooled75TopRuns = get75TopLRuns(qRels, lRuns, metric)
    println("Only 75% Top Best Runs: " + pooled75TopRuns.size)
    val pTopBestRunsQRels = poolAnalyser.repoolWith(pooled75TopRuns)
    computeEstimates(pTopBestRunsQRels, pooled75TopRuns, runs, metric, pValuesDir)
  }

  private def compute(qRels: QRels, pooledRuns: List[Runs], descs: Descs, metric: String, pValuesDir: File, l1xo:L1xo, printOut:PrintOut = PrintOut.all) = {
    println("Metric\t" + metric)

    val estimators = getEstimators(qRels, pooledRuns, descs, metric)

    if (isL1ro(l1xo))
      computeL1ro(estimators, metric, pValuesDir, printOut)

    if (descs != null && isL1oo(l1xo))
      computeL1oo(estimators, metric, pValuesDir, printOut)
  }

  private def computeEstimates(qRels: QRels, pooledRuns: List[Runs], runs:Runs, metric: String, pValuesDir: File) = {
    println("Metric\t" + metric)
    val estimators = getEstimators(qRels, pooledRuns, descs = null, metric)
    estimators.map(_.printScore(runs))
    println("")
  }

  private def computeL1ro(estimators: List[ScoreEstimator], metric:String, pValuesDir: File, printOut:PrintOut = all) = {
    println("Leave one RUN out approach")
    if(isOnlyRuns(printOut))
      printReportScores(estimators)

    if(isOnlyErrors(printOut)) {
      val scoresError = new ScoresError(estimators.filter(_.isInstanceOf[TrueEstimator]).head.getAllScores(), pValuesDir, metric)
      for(e <- estimators.filter(!_.isInstanceOf[TrueEstimator]))
        scoresError.printReportErrors(e)
    }
  }

  private def computeL1oo(estimators: List[ScoreEstimator], metric:String, pValuesDir: File, printOut:PrintOut = all) = {
    println("Leave one ORGANIZATION out approach")

    if(isOnlyRuns(printOut))
      printReportScores(estimators, L1xo.organization)

    if(isOnlyErrors(printOut)) {
      val scoresError = new ScoresError(estimators.filter(_.isInstanceOf[TrueEstimator]).head.getAllScores(), pValuesDir, metric)
      for(e <- estimators.filter(!_.isInstanceOf[TrueEstimator]))
        scoresError.printReportErrors(e, L1xo.organization)
    }
  }

  private def getEstimators(qRels: QRels, pooledRuns: List[Runs], descs: Descs, metric: String) = List(
    TrueEstimator(qRels, pooledRuns, metric, descs),
    PoolEstimator(qRels, pooledRuns, metric, descs),
    WebberOnRunsEstimator(qRels, pooledRuns, metric, descs),
    LipaniEstimator(qRels, pooledRuns, metric, descs))

  private def printReportScores(estimators: List[ScoreEstimator], l1xo: L1xo = L1xo.run) =
    estimators.map(_.printReportScores(l1xo))

  private def getQRels(file: File) = QRels.fromLines("test", TXTFile.getLines(file))

  private def getPoolAnalyser(qRels: QRels, runs: List[Runs]): PoolAnalyser = new PoolAnalyser(runs, qRels)

  private def getListRuns(path: String): List[Runs] = getListRuns(new File(path))

  private def getListRuns(path: File): List[Runs] = {
    val lF = path.listFiles
    lF.filterNot(_.getName.startsWith(".")).map(getRuns(_)).toList
  }

  private def getRuns(path: File): Runs =
    Runs.fromLines(TXTFile.getLines(path.getCanonicalPath))

  private def getListRuns(path: String, n: Int): List[Runs] = {
    val lF = new File(path).listFiles.take(n)
    lF.filter(f => f.getName.endsWith(".gz")).map(f => {
      Runs.fromLines(TXTFile.getLines(f.getCanonicalPath))
    }).toList
  }
}
