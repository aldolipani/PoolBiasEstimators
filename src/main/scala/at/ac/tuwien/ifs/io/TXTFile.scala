package at.ac.tuwien.io

import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.GZIPInputStream

import scala.io.Source

/**
 * Created by aldo on 10/10/14.
 */
object TXTFile {

  def getLines(path: String): Iterator[String] = getLines(new File(path))

  def getLines(file: File): Iterator[String] = {
    if (file.isDirectory)
      getLines(file.listFiles.filterNot(_.getName.startsWith(".")).head)
    else if (file.getName.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(file.getAbsolutePath)))).getLines
    else
      Source.fromFile(file).getLines
  }

  def writeFile(path: String, text: String) = {
    Files.write(Paths.get(path), text.getBytes(StandardCharsets.UTF_8))
    scala.io.Source.fromFile(path).getLines().map(println(_))
  }
}
