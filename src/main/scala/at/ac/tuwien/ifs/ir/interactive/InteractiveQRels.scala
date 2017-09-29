package at.ac.tuwien.ifs.ir.interactive

import akka.util.Timeout
import at.ac.tuwien.ifs.ir.model.{QRelRecord, Document, QRel, QRels}
import scala.collection.parallel.ParSeq
import scala.concurrent.{TimeoutException, Await}
import scala.concurrent.duration._
import akka.actor.{Inbox, ActorSystem, Props}
import spray.can.Http
import scala.language.postfixOps
import akka.pattern.ask
import akka.io.IO
import scala.collection._

/**
  * Created by aldo on 11/05/17.
  */
class InteractiveQRels(id: String, qRels: Seq[QRel], var nDs: Map[Int, Int], httpPort: Int) extends QRels(id, qRels) {

  implicit val system = ActorSystem("PoolBiasEstimators")
  implicit val inbox: Inbox = Inbox.create(system)
  implicit val timeout = Timeout(30.seconds)

  val managerService = system.actorOf(ManagerServiceActor.props(this), ManagerServiceActor.name)
  IO(Http) ? Http.Bind(managerService, interface = "0.0.0.0", port = httpPort)
  println("accepting connection to port: " + httpPort)

  for (qRel <- qRels) {
    val request = new ManagerServiceActor.Wait(qRel.id)
    managerService ! request
  }

  lazy val nQRels: mutable.Map[Int, mutable.Map[Document, QRelRecord]] = {
    val map = this.topicQRels.mapValues(qrel => {
      val map = qrel.qrelRecords.map(record => (record.document -> record)).toMap
      collection.mutable.HashMap(map.toSeq: _*)
    })
    collection.mutable.HashMap(map.toSeq: _*)
  }

  override
  def complete(idTopic: Int) {
    val request = new ManagerServiceActor.Done(idTopic)
    managerService ! request
  }

  override
  def getRel(idTopic: Int, document: Document) = {
    val rel =  nQRels(idTopic).getOrElse(document, QRelRecord("Q0", document, -1)).rel
    if (rel < 0) {
      askRelToUser(idTopic, document,
        if (nDs.nonEmpty) {
          nDs(idTopic) - nQRels(idTopic).count(e => e._2.rel >= 0)
        }else
          -1)
    } else {
      rel
    }
  }

  def askRelToUser(idTopic: Int, document: Document, left: Int): Int = {
    println("send request for topic " + idTopic + " and document " + document.id + ", " + left + " left")

    val rel =
      try {
        val request = ManagerServiceActor.Rel(idTopic, document, left)
        val future = managerService ? request
        val rel = Await.result(future, timeout.duration).asInstanceOf[Int]

        println("received result for topic " + idTopic + " and document " + document.id + ": " + rel)
        if(nQRels(idTopic).contains(document)) {
          val oldQRelRecord = nQRels(idTopic)(document)
          nQRels.get(idTopic).get.update(document,
            new QRelRecord(oldQRelRecord.iteration, oldQRelRecord.document, rel))
        }else{
          nQRels.get(idTopic).get.put(document,
            new QRelRecord("Q0", document, rel))
        }
        rel
      } catch {
        case e: TimeoutException => askRelToUser(idTopic, document, left)
      }
    rel
  }

  //split per topic
  override def toString: String =
    nQRels.toList.sortBy(_._1).map(e =>
      e._2.values.toList.sortBy(_.document).map(r =>
        e._1 + " " + r.toString).mkString("\n")).filter(_.nonEmpty).mkString("\n")
}

object InteractiveQRels {

  def par(qRels: Seq[QRel]): ParSeq[QRel] = {
    import scala.collection.parallel._
    val parQRels = qRels.par
    parQRels.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(qRels.size))
    parQRels
  }

}