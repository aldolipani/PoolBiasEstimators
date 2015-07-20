import java.io.File

import at.ac.tuwien.ir.evaluation
import at.ac.tuwien.ir.evaluation.{TRECEval, PoolAnalyser}
import at.ac.tuwien.ir.model.{Run, QRels, Runs}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TestTRECEval extends FlatSpec with Matchers {

  "TRECEval" should " return the same P_X as the original trec_eval" in {
    val fileQRels = new File(getClass.getResource("/TREC-2/Adhoc/QRels/qrels.101-150.disk1.disk2").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-2/Adhoc/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      trecEval.computeP5(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_5", runs, qRels))
      trecEval.computeP10(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_10", runs, qRels))
      trecEval.computeP15(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_15", runs, qRels))
      trecEval.computeP20(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_20", runs, qRels))
      trecEval.computeP30(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_30", runs, qRels))
      trecEval.computeP100(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_100", runs, qRels))
    }
  }

  "TRECEval" should " return the same AP_X as the original trec_eval with inverse qrels" in {
    val fileQRels = new File(getClass.getResource("/TREC-2/Adhoc/QRels/qrels.101-150.disk1.disk2").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-2/Adhoc/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      trecEval.computeAntiP5(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_5", runs, qRels.inverse))
      trecEval.computeAntiP10(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_10", runs, qRels.inverse))
      trecEval.computeAntiP15(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_15", runs, qRels.inverse))
      trecEval.computeAntiP20(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_20", runs, qRels.inverse))
      trecEval.computeAntiP30(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_30", runs, qRels.inverse))
      trecEval.computeAntiP100(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_100", runs, qRels.inverse))
    }
  }
}