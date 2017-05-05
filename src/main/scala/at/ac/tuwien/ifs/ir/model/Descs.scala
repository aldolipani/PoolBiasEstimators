package at.ac.tuwien.ifs.ir.model

import java.io.File

import scala.xml.{Elem, Node, NodeSeq, XML}

/**
 * Created by aldo on 28/10/14.
 */
class Descs(val descs: List[Desc]) {
  def size = descs.size

  lazy val manualDescs = new Descs(descs.map(desc => desc.manualRuns))
  lazy val tags = descs.flatMap(desc => desc.tags).toSet
  lazy val automaticDescs = new Descs(descs.map(_.automaticRuns))
  lazy val numberOfOrganizations = descs.map(_.organization).distinct.size

  def getRunsPerOrganization(runs: List[Runs]) =
    descs.map(_.runs.map(rs => {
      val l = runs.filter(r => r.id.split("@").head == rs.tag)
      if (l.nonEmpty) l.head else null
    }).filter(_ != null)).filter(_.nonEmpty)

  def contains(tag: String) = tags.contains(tag)
}

object Descs {

  def fromXMLDesc(name: String): Descs =
    fromXMLElem(XML.loadFile(name))

  def fromXMLDesc(file: File): Descs =
    fromXMLElem(XML.loadFile(file))

  def fromXMLElem(elem: Elem): Descs = {
    fromXMLNodeSeq((elem \ "runs"))
  }

  def fromXMLNodeSeq(node: NodeSeq): Descs =
    new Descs(node.map(item => {
      ((item \ "organization").text,
        RunDesc.fromXMLNode(item))
    }).groupBy(_._1).map(desc => {
      new Desc(
        desc._1,
        desc._2.map(_._2).toList
      )
    }).toList)
}

class Desc(val organization: String, val runs: List[RunDesc]) {
  lazy val manualRuns = new Desc(organization, runs.filter(_.queryMethod == "manual"))
  lazy val automaticRuns = new Desc(organization, runs.filter(_.queryMethod == "automatic"))
  lazy val tags = runs.map(_.tag).toSet

  def contains(tag: String) = tags.contains(tag)
}

class RunDesc(val tag: String, val queryMethod: String)

object RunDesc {

  def fromXMLNode(node: Node) =
    new RunDesc(
      (node \ "tag").text,
      (node \ "query_method").text)

}