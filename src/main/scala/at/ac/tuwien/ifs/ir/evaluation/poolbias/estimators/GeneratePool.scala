package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.io.TXTFile
import at.ac.tuwien.ifs.ir.evaluation.pool._
import at.ac.tuwien.ifs.ir.interactive.InteractiveQRels
import at.ac.tuwien.ifs.ir.model.QRels
import akka.util.Timeout
/**
  * Created by aldo on 08/05/17.
  */
class GeneratePool extends Command{

  def generate(trecQRelsFile: File, trecRunsDir: File, toPool:String, sizeRuns:Int, discoverPooledRuns:Boolean, poolAnalyzerType:String, interactivePool:Boolean, httpPort:Int, restore:Boolean) {
    val qRels =
      if(!interactivePool)
        getQRels(trecQRelsFile)
      else{
        val qRels = getInteractiveQRels(trecQRelsFile, Map(), httpPort)
        qRels
      }

    val lRuns = getListRuns(trecRunsDir)
    val nLRuns =
      if(sizeRuns > 0){
        RunsTransformer.resizeRuns(sizeRuns, lRuns)
      } else{
        lRuns
      }

    val pool = new Pool(nLRuns, qRels)

    val nPool =
      if(discoverPooledRuns) {
        val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
        val plRuns = poolAnalyser.pooledRuns
        Pool(plRuns, pool.qRels)
      }else{
        pool
      }

    if(!interactivePool)
      println(PoolConverter.to(toPool, nPool).qRels)
    else {
      val pool = PoolConverter.to(toPool, nPool, restore)
      qRels.asInstanceOf[InteractiveQRels].nDs = pool.asInstanceOf[FixedSizePool].nDs
      pool.qRels
      println(qRels.toString)
    }
  }

  protected def getInteractiveQRels(file: File, nDs:Map[Int, Int], httpPort:Int):InteractiveQRels = {
    val qRels = QRels.fromLines("test", TXTFile.getLines(file))
    new InteractiveQRels(qRels.id, qRels.qRels, nDs, httpPort)
  }


}

object GeneratePool{

  def apply() = new GeneratePool()

}
