package at.ac.tuwien.ifs.utils

import at.ac.tuwien.ifs.io.TXTFile

import scala.collection._
/**
  * Created by aldo on 14/11/16.
  */

object Exporter {
  val pages:mutable.Map[String, mutable.Map[String, List[String]]] = mutable.Map()
  var currentPage:String = null
}

trait Exporter {

  def addPage(id:String) {
    Exporter.pages.put(id, mutable.Map[String, List[String]]())
    Exporter.currentPage = id
  }

  def selectPage(id:String) {
    if(!Exporter.pages.contains(id))
      addPage(id)
    else
      Exporter.currentPage = id
  }

  def getMaxRow(columns:mutable.Map[String, List[String]]): Int = {
    if(columns.nonEmpty)
      columns.values.map(_.size).max
    else
      0
  }

  def getPadList(key:String, columns:mutable.Map[String, List[String]], maxRow:Int):List[String] = {
    val padSize = maxRow - columns.getOrElse(key, Nil).size
    (0 until padSize).toList.map(e => "")
  }

  def addRow(values:Map[String, String]) {
    val columns = Exporter.pages(Exporter.currentPage)
    val maxRow = getMaxRow(columns)
    for(key <- values.keys){
      val value =
        (if(columns.contains(key))
          columns(key)
        else
          Nil) :::
          (getPadList(key, columns, maxRow) :+ values(key))
      columns.put(key, value)
    }
    padRows()
    write()
  }

  def padRows() {
    val columns = Exporter.pages(Exporter.currentPage)
    val maxRow = getMaxRow(columns)

    for(key <- columns.keys){
      columns.put(key,
        columns(key) :::
          getPadList(key, columns, maxRow))
    }
  }

  def pastePages():Map[String, List[String]] = {
    val maps =
      for(key <- Exporter.pages.keys) yield {
        Exporter.pages(key).toMap.map(e => (key+"."+e._1 -> e._2))
      }
    maps.flatten.toMap
  }

  def write() {
    val columns = pastePages
    val keys = columns.keys.toList.sorted
    val maxRow = columns.values.map(_.size).max
    val body =
      (for(i <- 0 until maxRow) yield {
        (for(key <- keys) yield {
          val list = columns(key)
          if(i < list.size){
            list(i)
          }else{
            ""
          }
        }).mkString(",")
      }).mkString("\n")
    val head = keys.mkString(",")
    TXTFile.writeFile("export.csv", head + "\n" + body)
  }
}