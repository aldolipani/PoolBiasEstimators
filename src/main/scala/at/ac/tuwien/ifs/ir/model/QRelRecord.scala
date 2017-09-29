package at.ac.tuwien.ifs.ir.model

/**
 * Created by aldo on 10/10/14.
 */
class QRelRecord(val iteration: String, val document: Document, val rel: Int) {
  override def toString: String = s"$iteration ${document.id} $rel"

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: QRelRecord => that.iteration == iteration && that.document.id == document.id && that.rel == rel
    case _ => false
  }

}

object QRelRecord {
  def fromItems(arr: Array[String]): QRelRecord = {
    //if(arr(2).toInt == -1) throw new IllegalArgumentException("-1 is used for non judged documents")
    new QRelRecord(arr(0), new Document(arr(1)), arr(2).toInt)
  }

  def apply(iteration: String, document: Document, rel: Int) =
    new QRelRecord(iteration, document, rel)

}
