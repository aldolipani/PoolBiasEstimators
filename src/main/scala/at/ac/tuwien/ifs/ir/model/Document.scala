package at.ac.tuwien.ir.model

/**
 * Created by aldo on 24/11/14.
 */
class Document(val id: String) extends Comparable[Document] {
  override def equals(obj: scala.Any): Boolean = obj.asInstanceOf[Document].id == id

  override def hashCode = classOf[Document].hashCode + this.id.hashCode

  override def compareTo(document: Document): Int = document.id.compareTo(id)

  override def toString: String = id
}
