package at.ac.tuwien.ifs.ir.evaluation.pool

import java.io.File

import at.ac.tuwien.ifs.io.TXTFile
import at.ac.tuwien.ifs.ir.model.{Document, QRels, Runs}

import scala.sys.process._
import scala.util.Random

/**
  * Created by aldo on 31/08/16.
  */
class UnsupportedFixedSizeBasedPool(cmd: String, poolSize: Int, lRuns: List[Runs], gT: QRels) extends FixedSizePool(poolSize, lRuns, gT) {

  override lazy val qRels: QRels = PoolConverter.repoolToUnsupportedFixedSizeBased(cmd, topicSizes, lRuns, gT)

  override def getPooledDocuments(topicId: Int): Set[Document] = qRels.topicQRels(topicId).qrelRecords.map(_.document).toSet

  override def getNewInstance(lRuns: List[Runs]): Pool = UnsupportedFixedSizeBasedPool(cmd, poolSize, lRuns, gT)

}

object UnsupportedFixedSizeBasedPool {

  def getName(cmd:String) = cmd.replaceAll(" ", "_")

  def apply(cmd: String, poolSize:Int, lRuns: List[Runs], gT: QRels) = new UnsupportedFixedSizeBasedPool(cmd, poolSize, lRuns, gT)

  def getPooledDocuments(cmd: String, topicSizes:Map[Int, Int], lRuns: List[Runs], qRels: QRels): Map[Int, Set[Document]] = {
    def getRandomString = 1 to 10 map (i => Random.nextInt(10)) mkString ("")

    def clearFolder(name: String):Unit = {
      val listFiles = (new File(name)).listFiles
      if (listFiles != null)
        listFiles.map(e =>
          if(e.isDirectory)
            clearFolder(e.getAbsolutePath)
          else
            e.delete)
    }

    //store runs
    def getPath: String = {
      val rS = getRandomString
      if ((new File(".uBP-" + rS)).exists) getPath else new File(".uBP-" + rS).getAbsolutePath()
    }
    val path = getPath
    val runsP = new File(path, "runs").getCanonicalPath + File.separator
    new File(runsP).mkdirs()

    for (run <- lRuns)
      TXTFile.writeFile(new File(runsP, run.id).getCanonicalPath, run.toString)

    //store qRels
    val qRelsF = new File(path, ".qRels").getCanonicalPath
    TXTFile.writeFile(qRelsF, qRels.toString)

    val sTopicSizes  = topicSizes.map(e => e._1 + ":" + e._2).mkString(",")

    //execute cmd
    val iQRels = (s"$cmd $sTopicSizes $runsP $qRelsF" !!).trim.split("\n").toIterator
    val nQRels = QRels.fromLines("new", iQRels)

    //delete folder
    s"rm -r $path" !

    //parse qRels
    nQRels.topicQRels.mapValues(_.qrelRecords.map(_.document).toSet)
  }

}
