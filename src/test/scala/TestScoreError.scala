import java.io.File

import at.ac.tuwien.ifs.ir.evaluation.poolbias.estimators.ScoresError
import at.ac.tuwien.ifs.ir.evaluation.TRECEval
import at.ac.tuwien.ifs.ir.model.{Score, QRels, Runs}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TestScoreError extends FlatSpec with Matchers {

  def round(score: Double):Double = Math.round(score * 10000) / 10000d
/*
  "ScoreError" should " return the right MAE" in {
    val trueScores = List(
      new Score("A", 0.5d),
      new Score("B", 0.3d),
      new Score("C", 0.3d),
      new Score("D", 0.2d),
      new Score("E", 0.1d)
    )

    val sE = new ScoresError(trueScores, new File(getClass.getResource("pValues").getPath),"P_5")

    val newScores = trueScores.map(s => new Score(s.runId, s.score + 0.01d))
    round(sE.meanAbsoluteError(newScores)._1) should be(0.01d)
  }

  "ScoreError" should " return the right SRE" in {
    val trueScores = List(
      new Score("A", 0.5d),
      new Score("B", 0.3d),
      new Score("C", 0.3d),
      new Score("D", 0.2d),
      new Score("E", 0.1d)
    )

    val newScores = List(
      new Score("A", 0.2d),
      new Score("B", 0.3d),
      new Score("C", 0.3d),
      new Score("D", 0.0d),
      new Score("E", 0.1d)
    )

    val sE = new ScoresError(trueScores, new File(getClass.getResource("pValues").getPath),"P_5")
    sE.systemRankError(newScores) should be(3)
  }

  "ScoreError" should " return the right SRE*" in {
    val trueScores = List(
      new Score("A", 0.5d),
      new Score("B", 0.3d),
      new Score("C", 0.3d),
      new Score("D", 0.2d),
      new Score("E", 0.1d)
    )

    val newScores = List(
      new Score("A", 0.5d),
      new Score("B", 0.3d),
      new Score("C", 0.5d),
      new Score("D", 0.2d),
      new Score("E", 0.1d)
    )

    val sE = new ScoresError(trueScores, new File(getClass.getResource("pValues").getPath),"P_5")
    val pValues: Map[String, Double] = Source.fromFile(new File(getClass.getResource("pValues/pValues.P_5.csv").getPath)).getLines().map(_.split(",")).filter(_.size == 3).map(a => (a(0).replace("input.", "") + a(1).replace("input.", "") -> a(2).toDouble)).toMap

    sE.systemRankError(newScores,pValues) should be(1)
  }*/

}