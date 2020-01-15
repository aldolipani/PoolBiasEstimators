package at.ac.tuwien.ifs.ir.model

/**
 * Created by aldo on 17/02/15.
 */
class Score(val runId: String, val score: Double, val metric: String, val qRels: QRels) {

  def format(score: Double): String = "%1.4f" format score

  override def toString: String = "%-15s\t%s" format(runId, format(score))

}

class DetailedScore(runId: String, score: Double, val topicScores: Map[Int, Double], metric: String, qRels: QRels) extends Score(runId, score, metric, qRels) {

  override def toString: String = ("%-15s\t%s" format(runId, format(score))) + "\t[" +
    topicScores.toList.sortBy(_._1).map(e => format(e._2)).mkString("; ") + "]"

}