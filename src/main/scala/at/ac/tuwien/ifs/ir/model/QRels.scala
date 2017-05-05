package at.ac.tuwien.ifs.ir.model

import java.io.File

import scala.io.Source

/**
 * Created by aldo on 10/10/14.
 */
class QRels(val id: String, val qRels: Seq[QRel]) {
  lazy val topicQRels = qRels.map(qRel => qRel.id -> qRel).toMap
  lazy val topicIds = qRels.map(qRel => qRel.id).toSet

  lazy val sizeTopics = topicIds.size
  lazy val size: Int = qRels.map(_.qrelRecords.size).sum
  lazy val sizeRel: Int = qRels.map(_.sizeRel).sum
  lazy val sizeNotRel: Int = qRels.map(_.sizeNotRel).sum

  override def toString: String = qRels.mkString("\n")

  @deprecated
  def getRel(idTopic: Int, idDocument: String) = topicQRels(idTopic).getRel(idDocument)

  def getRel(idTopic: Int, document: Document) = topicQRels(idTopic).getRel(document)

  lazy val inverse = new QRels("inv" + id, {
    qRels.map(qR => {
      new QRel(
        qR.id,
        qR.qrelRecords.map(qRR =>
          new QRelRecord(qRR.iteration, qRR.document,
            if (qRR.rel == 0) 1 else 0)))
    })
  })
}

object QRels {

  def fromFile(id: String, name: String): QRels =
    fromLines(id, Source.fromFile(name).getLines())

  def fromFile(id: String, file: File): QRels =
    fromLines(id, Source.fromFile(file).getLines())

  def fromLines(id: String, iterator: Iterator[String]): QRels =
    fromListOfItems(id, iterator.map(_.trim.split("\\s+")).filter(_.size == 4).toList)

  def fromListOfItems(id: String, list: List[Array[String]]): QRels = {
    val runs = list.groupBy(_.head).values.map(QRel.fromListOfItems)
    new QRels(id, runs.toList)
  }

}
