package at.ac.tuwien.ifs.io

import java.io.{IOException, BufferedInputStream, File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

/**
 * Created by aldo on 10/10/14.
 */
object TXTFile {
  import ExecutionContext.Implicits.global

  def getLines(path: String): Iterator[String] = getLines(new File(path))

  def getLines(file: File): Iterator[String] = {
    if (file.isDirectory)
      getLines(file.listFiles.filterNot(_.getName.startsWith(".")).head)
    else if (file.getName.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(file.getAbsolutePath)))).getLines
    else
      Source.fromFile(file).getLines
  }

  def writeFile(path: String, text: String) {
    Files.write(Paths.get(path), text.getBytes(StandardCharsets.UTF_8))
  }

  def reWriteFile(path: String, text: String) {
    if(Files.exists(Paths.get(path)))
      try {
        Files.delete(Paths.get(path))
      }catch{
        case e:IOException => e.printStackTrace()
      }
    Files.write(Paths.get(path), text.getBytes(StandardCharsets.UTF_8))
  }

  def writeFileAsync(path: String, text: String):Future[Path] = Future {
    Files.write(Paths.get(path), text.getBytes(StandardCharsets.UTF_8))
  }
}
