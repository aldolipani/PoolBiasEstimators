package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin

import java.io.File

import at.ac.tuwien.ifs.io.TXTFile
import at.ac.tuwien.ifs.ir.evaluation.pool.{Pool, PoolAnalyzer, PoolAnalyzerType}
import at.ac.tuwien.ifs.ir.model.{QRels, Runs}



/**
  * Created by aldo on 14/02/15.
  */


object Utils extends App {

  protected def getQRels(file: File) = QRels.fromLines("test", TXTFile.getLines(file))

  protected def getListRuns(path: String): List[Runs] = getListRuns(new File(path))

  protected def getListRuns(dir: File): List[Runs] =
    dir.listFiles.filterNot(_.getName.startsWith(".")).toList.par.map(getRuns(_)).seq.toList

  protected def getRuns(path: File): Runs =
    Runs.fromLines(TXTFile.getLines(path.getCanonicalPath), path.getName.replaceAllLiterally("input.", ""))

  override def main(args: Array[String]) {
    val trecRelFile = new File(args(0))
    val trecRunsDir = args(1)
    println(trecRelFile)
    println(trecRunsDir)

    val lRuns = getListRuns(trecRunsDir)
    val qRels = getQRels(trecRelFile)

    val pool = new Pool(lRuns, qRels)

    val poolAnalyser = PoolAnalyzer(pool, PoolAnalyzerType.MODE)
    println(poolAnalyser.pooledRuns.map(e => e.id).mkString("\n"))
  }
}
