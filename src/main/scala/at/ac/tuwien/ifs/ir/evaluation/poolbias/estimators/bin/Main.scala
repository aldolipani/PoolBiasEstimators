package at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.bin

import java.io.File

import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.Analysis
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.Analysis.L1xo.L1xo
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.Analysis.PrintOut.PrintOut
import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.Analysis.{L1xo, PrintOut}

/**
 * Created by aldo on 14/02/15.
 */
object Main extends App {

  case class Config(trecRelFile: File = null,
                    trecRunsDir: File = null,
                    trecRunsFile: File = null,
                    descRunsFile: File = null,
                    pValuesDir: File = null,
                    printOut: PrintOut = PrintOut.all,
                    l1xo: L1xo = L1xo.both,
                    toPool:String = "",
                    top75Runs: Boolean = false)

  override def main(args: Array[String]){
    val parser = new scopt.OptionParser[Config]("pool_bias_estimators") {
      head("pool_bias_estimators", "2.0")
      opt[Unit]('r', "leaveOneRunOut") action { (x, c) =>
        c.copy(l1xo = L1xo.run)
      } text ("active the leave-one-run-out, only in analysis mode")
      opt[Unit]('o', "leaveOneOrganizationOut") action { (x, c) =>
        c.copy(l1xo = L1xo.organization)
      } text ("active the leave-one-organization-out, it requires the file of description of the runs, only in analysis mode")
      opt[Unit]('s', "onlyRunScoresReport") action { (x, c) =>
        c.copy(printOut = PrintOut.onlyRuns)
      } text ("print only the run scores report, only in analysis mode")
      opt[Unit]('b', "onlyPoolBiasReport") action { (x, c) =>
        c.copy(printOut = PrintOut.onlyErrors)
      } text ("print only the pool bias report, only in analysis mode")
      opt[String]("toPoolingStrategy") optional() action { (x, c) =>
        c.copy(toPool = x.toLowerCase)
      } text ("create a syntetic pool from Depth@d pooling strategy to another pooling strategy, available pooling strategies are: Depth_d, SampledDepth_d:r and Stratified{_d:r}+")
      opt[Unit]('t', "top75Runs") action { (x, c) =>
        c.copy(top75Runs = true)
      } text ("use only the top 75% of pooled runs per metric")
      opt[File]('p', "pValues") optional() action { (x, c) =>
        c.copy(pValuesDir = x)
      } text ("directory of files in which are stored the p-values for each pair of runs, if not provided the metrics based on statistical significance (*) are not computed")
      opt[File]('d', "desc") optional() action { (x, c) =>
        c.copy(descRunsFile = x)
      } text ("file of description of the runs, if not provided the leave-one-organization-out can NOT be computed")
      arg[File]("<trec_rel_file>") required() action { (x, c) =>
        c.copy(trecRelFile = x)
      } text ("relevance assessments in standard TREC format")
      arg[File]("<trec_runs_dir>") required() action { (x, c) =>
        c.copy(trecRunsDir = x)
      } text ("directory of the runs in standard TREC format")
      arg[File]("<trec_run_file>") optional() action { (x, c) =>
        c.copy(trecRunsFile = x)
      } text ("run in standard TREC format, if not provided it activates the analysis mode")
      help("help") text ("prints this usage text")
    }

    parser.parse(args, Config()) match {
      case Some(config) => {
        if(config.trecRunsFile == null) {
          if (config.printOut == PrintOut.onlyErrors)
            Analysis.computeOnlyErrors(config.trecRelFile, config.trecRunsDir, config.descRunsFile, config.pValuesDir, config.l1xo, config.top75Runs)
          else if (config.printOut == PrintOut.onlyRuns)
            Analysis.computeOnlyRuns(config.trecRelFile, config.trecRunsDir, config.descRunsFile, config.pValuesDir, config.l1xo, config.top75Runs)
          else
            Analysis.computeAll(config.trecRelFile, config.trecRunsDir, config.descRunsFile, config.pValuesDir, config.l1xo, config.top75Runs, config.toPool)
        }else{
          Analysis.computeEstimatesAll(config.trecRelFile, config.trecRunsDir, config.trecRunsFile, config.descRunsFile, config.pValuesDir, config.toPool)
        }
      }
      case None =>
    }
  }
}
