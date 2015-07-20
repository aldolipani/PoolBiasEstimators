package at.ac.tuwien.ir.model

import java.io.File

import scala.io.Source

/**
 * Created by aldo on 10/10/14.
 */
class QRels(val id: String, val qRels: Seq[QRel]) {
  lazy val topicQRels = qRels.map(qRel => (qRel.id -> qRel)).toMap
  lazy val topicIds = qRels.map(qRel => qRel.id).toSet

  override def toString: String = qRels.mkString("\n")

  def sizeTopics = topicIds.size

  def size: Int = qRels.map(_.qrelRecord.size).sum

  def getRel(idTopic: Int, idDocument: String) = topicQRels(idTopic).getRel(idDocument)

  def inverse = new QRels("inv" + id, {
    qRels.map(qR => {
      new QRel(
        qR.id,
        qR.qrelRecord.map(qRR =>
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
    val runs = list.groupBy(_.head).values.map(QRel.fromListOfItems(_))
    new QRels(id, runs.toList)
  }

}
