package at.ac.tuwien.ir.model

import java.io.File

import scala.io.Source

/**
 * Created by aldo on 10/10/14.
 */
class Runs(val id: String, val runs: List[Run]) {
  lazy val runsMapByTopicId = runs.map(r => (r.id -> r)).toMap
  lazy val topicIds = runs.map(_.id).toSet

  def selectByTopicId(id: Int): Run = runsMapByTopicId.getOrElse(id, null)

  override def toString: String = runs.sortBy(_.id).map(_.toString.split("\n").map(_ + s" $id").mkString("\n")).mkString("\n")
}

object Runs {

  def fromFile(name: String): Runs =
    fromLines(Source.fromFile(name).getLines)

  def fromFile(file: File): Runs =
    fromLines(Source.fromFile(file).getLines)

  def fromLines(iterator: Iterator[String]): Runs = {
    val group = iterator.withFilter(_.nonEmpty).map(e => {
      val es = e.trim.split("\\s+")
      if (es.size > 6) es.take(6) else es
    }).toList.groupBy(_.last)
    fromListOfItems(group.values.head)
  }

  def fromListOfItems(list: List[Array[String]]): Runs = {
    val id = if (list.head.size == 5) "" else list.head.last
    val runs = list.groupBy(_.head).values.map(rIs => Run.fromListOfItems(if (list.head.size == 5) rIs else rIs.map(_.init)))
    new Runs(id, runs.toList.sortBy(_.id))
  }
}