package at.ac.tuwien.ifs.ir.model

import java.io.File

import scala.io.Source

/**
  * Created by aldo on 10/10/14.
  */
class Runs(val id: String, val runs: List[Run]) {

  lazy val runsMapByTopicId = runs.map(r => (r.id -> r)).toMap

  lazy val topicIds = runs.map(_.id).toSet

  @deprecated
  def selectByTopicId(id: Int): Run = runsMapByTopicId.getOrElse(id, null)

  def selectByTopicIdOrNil(id: Int): Run = runsMapByTopicId.getOrElse(id, new Run(id, Nil))

  override def toString: String = runs.sortBy(_.id).map(_.toString.split("\n").map(_ + s" $id").mkString("\n")).mkString("\n")

  def getRunsTopic(topic: Int): Runs = Runs(id + "@" + topic, List(this.selectByTopicIdOrNil(topic)))

}

object Runs {

  val runsIDs = scala.collection.mutable.Set[String]()

  def apply(id: String, runs: List[Run]) = new Runs(id, runs)

  def fromFile(name: String): Runs = {
    fromLines(Source.fromFile(name).getLines(), name.replaceAllLiterally("input.", ""))
  }

  def fromFile(file: File): Runs = {
    fromLines(Source.fromFile(file).getLines(), file.getName.replaceAllLiterally("input.", ""))
  }

  def fromLines(iterator: Iterator[String], tmpId: String = ""): Runs = {
    val group = iterator.withFilter(_.nonEmpty).map(e => {
      val es = e.trim.split("\\s+")
      if (es.size > 6) es.take(6) else es
    }).toList.groupBy(_.last)
    fromListOfItems(group.values.head, tmpId)
  }

  def fromLines2(iterator: Iterator[String], tmpId: String = ""): Runs = {
    val list = iterator.withFilter(_.nonEmpty).map(e => {
      val es = e.trim.split("\\s+")
      if (es.size > 6) es.take(6) else es
    }).toList
    fromListOfItems(list, tmpId)
  }

  def fromListOfItems(list: List[Array[String]], tmpId: String = ""): Runs = {
    val tId = if (list.head.size == 5) tmpId else list.head.last
    val id = if (runsIDs.contains(tId)) {
      println("Warning: runs with id " + tId + "already exists! Try to recover with " + tmpId + "...")
      if (tmpId.isEmpty || runsIDs.contains(tmpId)) throw new Exception("Could not recover id runs!")
      tmpId
    } else
      tId
    val runs = list.groupBy(_.head).values.map(rIs => Run.fromListOfItems(if (list.head.size == 5) rIs else rIs.map(_.init)))
    new Runs(id, runs.toList.sortBy(_.id))
  }
}