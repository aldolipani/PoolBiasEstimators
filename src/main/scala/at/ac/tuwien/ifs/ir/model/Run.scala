package at.ac.tuwien.ifs.ir.model

/**
 * Created by aldo on 10/10/14.
 */
class Run(val id: Int, val runRecords: List[RunRecord]) {
  private lazy val runRecordsMapByDocumentID = {
    val runRecordsMapByDocumentID = runRecords.map(rR => rR.document.id -> rR).toMap
    //if(runRecords.size != runRecordsMapByDocumentID.size)
    //  throw new Exception(runRecords.size + " " + runRecordsMapByDocumentID.size)
    runRecordsMapByDocumentID
  }

  private lazy val runRecordsMapByDocument = runRecords.map(rR => rR.document -> rR).toMap

  override def toString: String = runRecords.sortBy(-_.score).map(s"$id " + _.toString).mkString("\n")

  def getByDocument(doc:Document): RunRecord = runRecordsMapByDocument.getOrElse(doc, null)

  @deprecated
  def getByDocumentId(id: String): RunRecord = runRecordsMapByDocumentID.getOrElse(id, null)

}

object Run {

  val rID = """^(\d+).*$""".r

  def runIDToInt(id: String): Int = id.reverse match {
    case rID(nId) => nId.reverse.toInt
  }

  def filterDuplicateDocuments(list: List[Array[String]]): List[Array[String]] = {
    def filterDuplicateDocuments(list: List[Array[String]], seen: Set[String], acc: List[Array[String]]): List[Array[String]] = {
      if (list.isEmpty)
        acc
      else {
        val e = list.head
        if (seen.contains(e(2)))
          filterDuplicateDocuments(list.tail, seen, acc)
        else
          filterDuplicateDocuments(list.tail, seen + e(2), acc :+ e)
      }
    }

    filterDuplicateDocuments(list, Set(), Nil)
  }

  def fromListOfItems(list: List[Array[String]]): Run = {
    //println(list.head.take(10).mkString(","))
    val id = runIDToInt(list.head.head)
    val nList = normalizeRawRank(list.filterNot(_.length == 4))
    val uList = filterDuplicateDocuments(nList)
    val nuList = normalizeRawRank(uList)
    val rRs = nuList.groupBy(_.head).values.head.map(rRIs =>
      try {
        RunRecord.fromItems(rRIs.tail)
      } catch {
        case e: Exception => {
          println(nList.map(_.mkString(" ")).mkString("\n"))
          throw e
        }
      })
    new Run(id, rRs.toList)
  }

  def normalizeRawRank(list: List[Array[String]]): List[Array[String]] = {
    val ordering: Ordering[(Float, String)] = Ordering.Tuple2(Ordering.Float.reverse, Ordering.String.reverse)
    val sList = list.sortBy(e => (e(4).toFloat, e(2).trim))(ordering)
    val sListNaN = sList.takeWhile(e => e(4).toFloat.isNaN)
    val cList = sList.drop(sListNaN.size) ::: sListNaN
    cList.zipWithIndex.map(e => {
      e._1(3) = (e._2 + 1).toString
      e._1})
  }

  def normalizeRank(runRecords: List[RunRecord]): List[RunRecord] = {
    val sList = runRecords.sortBy(_.document.id).reverse.sortBy(-_.score)
    (1 to sList.size).foldRight(sList)((i, sList) => {
      sList.take(i - 1) ::: new RunRecord(sList(i - 1).iteration, sList(i - 1).document, i, sList(i - 1).score) :: sList.drop(i)
    })
  }
}