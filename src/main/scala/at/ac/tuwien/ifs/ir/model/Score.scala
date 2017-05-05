package at.ac.tuwien.ifs.ir.model

/**
 * Created by aldo on 17/02/15.
 */
class Score(val runId: String, val score: Double, val metric:String, val qRels:QRels) {

  override def toString = "%-15s\t%s" format (runId, "%.4f" format score)

}