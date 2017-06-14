import java.io.File

import at.ac.tuwien.ifs.ir.evaluation
import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.model.{Run, QRels, Runs}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TestTRECEval extends FlatSpec with Matchers {

  "TRECEval" should " return the same R_X as the original trec_eval on TREC-20 as the original" in {
    val fileQRels = new File(getClass.getResource("/TREC-20/Medical/QRels/qrels.txt").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-20/Medical/Runs").getPath).listFiles.filter(f => f.isFile /*&& f.getName.contains("uogTrDeNsEc")*/).map { f =>
      val runs = Runs.fromFile(f)
      println(runs.id)
      //println(trecEval.computeMetricPerTopic("recall_20", runs, qRels).toList.sortBy(_._1).map(_._2).mkString(", "))
      //println(trecEval.computeUnsupportedMetricPerTopic("recall_20", runs, qRels).toList.sortBy(_._1).map(_._2).mkString(", "))
      trecEval.computeRecall5(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_5", f.getAbsolutePath, fileQRels.getAbsolutePath))
      trecEval.computeRecall10(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_10", f.getAbsolutePath, fileQRels.getAbsolutePath))
      trecEval.computeRecall15(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_15", f.getAbsolutePath, fileQRels.getAbsolutePath))
      trecEval.computeRecall20(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_20", f.getAbsolutePath, fileQRels.getAbsolutePath))
      trecEval.computeRecall30(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_30", f.getAbsolutePath, fileQRels.getAbsolutePath))
      trecEval.computeRecall100(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_100", f.getAbsolutePath, fileQRels.getAbsolutePath))
    }
  }

  "TRECEval" should " return the same R_X as the original trec_eval on TREC-20" in {
    val fileQRels = new File(getClass.getResource("/TREC-20/Medical/QRels/qrels.txt").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-20/Medical/Runs").getPath).listFiles.filter(f => f.isFile /*&& f.getName.contains("uogTrDeNsEc")*/).map { f =>
      val runs = Runs.fromFile(f)
      println(runs.id)
      //println(trecEval.computeMetricPerTopic("recall_20", runs, qRels).toList.sortBy(_._1).map(_._2).mkString(", "))
      //println(trecEval.computeUnsupportedMetricPerTopic("recall_20", runs, qRels).toList.sortBy(_._1).map(_._2).mkString(", "))
      trecEval.computeRecall5(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_5", runs, qRels))
      trecEval.computeRecall10(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_10", runs, qRels))
      trecEval.computeRecall15(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_15", runs, qRels))
      trecEval.computeRecall20(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_20", runs, qRels))
      trecEval.computeRecall30(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_30", runs, qRels))
      trecEval.computeRecall100(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_100", runs, qRels))
    }
  }


  "TRECEval" should " return the same MAP as the original trec_eval on TREC-2" in {
    val fileQRels = new File(getClass.getResource("/TREC-2/Adhoc/QRels/qrels.101-150.disk1.disk2").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-2/Adhoc/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      //println(runs.id)
      trecEval.computeMAP(runs, qRels) should be(trecEval.computeUnsupportedMetric("map", runs, qRels))
    }
  }

  "TRECEval" should " return the same MAP as the original trec_eval on TREC-20" in {
    val fileQRels = new File(getClass.getResource("/TREC-20/Medical/QRels/qrels.txt").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-20/Medical/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      println(runs.id)
      trecEval.computeMAP(runs, qRels) should be(trecEval.computeUnsupportedMetric("map", runs, qRels))
    }
  }

  "TRECEval" should " return the same R_X as the original trec_eval on TREC-2" in {
    val fileQRels = new File(getClass.getResource("/TREC-2/Adhoc/QRels/qrels.101-150.disk1.disk2").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-2/Adhoc/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      println(runs.id)
      trecEval.computeRecall5(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_5", runs, qRels))
      trecEval.computeRecall10(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_10", runs, qRels))
      trecEval.computeRecall15(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_15", runs, qRels))
      trecEval.computeRecall20(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_20", runs, qRels))
      trecEval.computeRecall30(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_30", runs, qRels))
      trecEval.computeRecall100(runs, qRels) should be(trecEval.computeUnsupportedMetric("recall_100", runs, qRels))
    }
  }


  "TRECEval" should " return the same P_X as the original trec_eval on TREC-2" in {
    val fileQRels = new File(getClass.getResource("/TREC-2/Adhoc/QRels/qrels.101-150.disk1.disk2").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-2/Adhoc/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      //println(runs.id)
      trecEval.computeP5(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_5", runs, qRels))
      trecEval.computeP10(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_10", runs, qRels))
      trecEval.computeP15(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_15", runs, qRels))
      trecEval.computeP20(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_20", runs, qRels))
      trecEval.computeP30(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_30", runs, qRels))
      trecEval.computeP100(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_100", runs, qRels))
    }
  }

  "TRECEval" should " return the same P_X as the original trec_eval on TREC-20" in {
    val fileQRels = new File(getClass.getResource("/TREC-20/Medical/QRels/qrels.txt").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-20/Medical/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      println(runs.id)
      trecEval.computeP5(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_5", runs, qRels))
      trecEval.computeP10(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_10", runs, qRels))
      trecEval.computeP15(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_15", runs, qRels))
      trecEval.computeP20(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_20", runs, qRels))
      trecEval.computeP30(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_30", runs, qRels))
      trecEval.computeP100(runs, qRels) should be(trecEval.computeUnsupportedMetric("P_100", runs, qRels))
    }
  }

  "TRECEval" should " return the same NDCG as the original trec_eval on TREC-2" in {
    val fileQRels = new File(getClass.getResource("/TREC-2/Adhoc/QRels/qrels.101-150.disk1.disk2").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-2/Adhoc/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      //println(runs.id)
      trecEval.computeNDCG(runs, qRels) should be(trecEval.computeUnsupportedMetric("ndcg", runs, qRels))
    }
  }

  "TRECEval" should " return the same NDCG as the original trec_eval on TREC-20" in {
    val fileQRels = new File(getClass.getResource("/TREC-20/Medical/QRels/qrels.txt").getPath)
    val qRels = QRels.fromFile("Test", fileQRels)

    val trecEval = new TRECEval

    new File(getClass.getResource("/TREC-20/Medical/Runs").getPath).listFiles.filter(f => f.isFile).map { f =>
      val runs = Runs.fromFile(f)
      //println(runs.id)
      val res = trecEval.computeNDCG(runs, qRels)
      val tRes = trecEval.computeUnsupportedMetric("ndcg", runs, qRels)
      assert( tRes - 0.0001d <= res && res <= tRes + 0.0001d )
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