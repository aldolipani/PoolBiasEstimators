package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.io.TXTFile
import at.ac.tuwien.ifs.ir.model.{Runs, QRels}

/**
  * Created by aldo on 08/05/17.
  */
class Command {

  protected def getQRels(file: File) = QRels.fromLines("test", TXTFile.getLines(file))

  protected def getListRuns(path: String): List[Runs] = getListRuns(new File(path))

  protected def getListRuns(path: File): List[Runs] = {
    val lF = path.listFiles
    lF.filterNot(_.getName.startsWith(".")).map(getRuns(_)).toList
  }

  protected def getRuns(path: File): Runs =
    Runs.fromLines(TXTFile.getLines(path.getCanonicalPath), path.getName.replaceAllLiterally("input.",""))

  protected def getListRuns(path: String, n: Int): List[Runs] = {
    val lF = new File(path).listFiles.take(n)
    lF.filter(f => f.getName.endsWith(".gz")).map(f => {
      Runs.fromLines(TXTFile.getLines(f.getCanonicalPath), f.getName.replaceAllLiterally("input.",""))
    }).toList
  }

}
