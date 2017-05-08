package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators

import java.io.File

import at.ac.tuwien.ifs.ir.evaluation.pool._
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.L1xo._
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin.Main.PrintOut
import at.ac.tuwien.ifs.ir.model.Descs

/**
  * Created by aldo on 08/05/17.
  */
class GeneratePool extends Command{

  def generate(trecQRelsFile: File, trecRunsDir: File, toPool:String, sizeRuns:Int, discoverPooledRuns:Boolean, poolAnalyzerType:String) {
    val lRuns = getListRuns(trecRunsDir)
    val nLRuns =
      if(sizeRuns > 0){
        RunsTransformer.resizeRuns(sizeRuns, lRuns)
      } else{
        lRuns
      }
    val qRels = getQRels(trecQRelsFile)

    val pool = new Pool(nLRuns, qRels)
    //WARNING: input must be a DepthNPool
    val nPool =
      if(discoverPooledRuns) {
        val poolAnalyser = PoolAnalyzer(pool, poolAnalyzerType)
        val plRuns = poolAnalyser.pooledRuns
        Pool(plRuns, pool.qRels)
      }else{
        pool
      }

    println(PoolConverter.to(toPool, nPool).qRels)
  }

}

object GeneratePool{

  def apply() = {
    new GeneratePool();
  }

}
