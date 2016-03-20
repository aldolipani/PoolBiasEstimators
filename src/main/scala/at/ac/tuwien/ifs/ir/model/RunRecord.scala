package at.ac.tuwien.ifs.ir.model

/**
 * Created by aldo on 10/10/14.
 */

class RunRecord(val iteration: String, val document: Document, val rank: Int, val score: Float) {
  override def toString: String = s"$iteration ${document.id} $rank $score"
}

object RunRecord {
  def fromItems(arr: Array[String]): RunRecord = try {
    new RunRecord(arr(0), new Document(arr(1)), arr(2).toInt, arr(3).toFloat)
  } catch {
    case e: Exception => {
      println(arr.mkString(" "));
      throw e
    }
  }
}