package at.ac.tuwien.ir.model

/**
 * Created by aldo on 10/10/14.
 */
class QRel(val id: Int, val qrelRecord: List[QRelRecord]) {
  lazy val documentIDs = qrelRecord.map(qR => (qR.document.id -> qR.rel)).toMap

  override def toString: String = qrelRecord.map(s"$id " + _.toString).mkString("\n")

  def containsDocumentId(id: String): Boolean = documentIDs.contains(id) //||
  //    (if (id.startsWith("ZF0")) documentIDs.contains(id.replace("ZF0", "ZF10")) || documentIDs.contains(id.replace("ZF0", "ZF20")) else false)

  def getRel(idDocument: String): Int = documentIDs.getOrElse(idDocument, -1)

  //if (idDocument.startsWith("ZF0")) documentIDs.getOrElse(idDocument.replace("ZF0", "ZF10"), documentIDs.getOrElse(idDocument.replace("ZF0", "ZF20"), -1)) else -1)
}

object QRel {
  def fromListOfItems(list: List[Array[String]]): QRel = {
    val id = list.head.head.toInt
    val qRs = list.map(qRIs => QRelRecord.fromItems(qRIs.tail))
    new QRel(id, qRs.toList)
  }
}
