package at.ac.tuwien.ifs.ir.model

/**
 * Created by aldo on 10/10/14.
 */
class QRel(val id: Int, val qrelRecords: List[QRelRecord]) {
  lazy val documentIDs = qrelRecords.map(qR => (qR.document.id -> qR.rel)).toMap
  lazy val documents = qrelRecords.map(qR => (qR.document -> qR.rel)).toMap

  lazy val size: Int = qrelRecords.size
  lazy val sizeRel: Int = qrelRecords.count(qR => qR.rel > 0)
  lazy val sizeNotRel: Int = qrelRecords.count(qR => qR.rel == 0)

  override def toString: String = qrelRecords.sortBy(_.document).map(s"$id " + _.toString).mkString("\n")

  def containsDocumentId(id: String): Boolean = documentIDs.contains(id) //||
  //    (if (id.startsWith("ZF0")) documentIDs.contains(id.replace("ZF0", "ZF10")) || documentIDs.contains(id.replace("ZF0", "ZF20")) else false)

  @deprecated
  def getRel(idDocument: String): Int = documentIDs.getOrElse(idDocument, -1)

  def getRel(document: Document): Int = documents.getOrElse(document, -1)
  //if (idDocument.startsWith("ZF0")) documentIDs.getOrElse(idDocument.replace("ZF0", "ZF10"), documentIDs.getOrElse(idDocument.replace("ZF0", "ZF20"), -1)) else -1)
}

object QRel {
  def fromListOfItems(list: List[Array[String]]): QRel = {
    val id = list.head.head.toInt
    val qRs = list.map(qRIs => QRelRecord.fromItems(qRIs.tail))
    new QRel(id, qRs)
  }
}
