package at.ac.tuwien.ifs.ir.interactive

import akka.actor.{Actor, ActorRef, Props}
import at.ac.tuwien.ifs.ir.model.Document
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, _}
import spray.routing._

import scala.collection._

/**
  * Created by aldo on 11/05/17.
  */

case class TopicDocumentRel(topic: Int, document: String, rel: Int)

case class TopicState(topic: Int, document: String, left:Int, state: String = "judge")

case class Response(value: List[TopicState], status: String = "OK")

case class ErrorResponse(status: String = "Error")

case object TDJsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val topicDocumentFormat: JsonFormat[TopicState] = jsonFormat4(TopicState)
}

case object MasterJsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {

  import TDJsonProtocol._

  implicit val statusFormat: JsonFormat[Response] = jsonFormat2(Response)
  implicit val errorFormat: JsonFormat[ErrorResponse] = jsonFormat1(ErrorResponse)
}

object ManagerServiceActor {

  val name = "manager-service"

  trait IRequest

  case class Wait(idTopic: Int) extends IRequest

  case class Rel(idTopic: Int, document: Document, left:Int) extends IRequest

  case class GotRel(idTopic: Int) extends IRequest

  case class Done(idTopic: Int) extends IRequest

  def props(qRels: InteractiveQRels): Props = Props(new ManagerServiceActor(qRels))

}

class TopicsState(val requests: Map[Int, (Document, Int,ActorRef)] = Map(), val status: Map[Int, String] = Map()) {

  def changeStatus(idTopic: Int, state: String) =
    TopicsState(requests - (idTopic),
      status + (idTopic -> state))

  def addRequest(idTopic: Int, document: Document, left: Int, actorRef: ActorRef) =
    TopicsState(requests + (idTopic ->(document, left, actorRef)),
      status - idTopic)

  def removeRequest(idTopic: Int) =
    TopicsState(requests - idTopic,
      status - idTopic)
}

object TopicsState {
  def apply(requests: Map[Int, (Document, Int, ActorRef)], status: Map[Int, String]) = new TopicsState(requests, status)
}

class ManagerServiceActor(val qRels: InteractiveQRels) extends Actor with HttpService {

  def actorRefFactory = context

  import ManagerServiceActor._

  def start(topicsState: TopicsState): Receive = {
    case Rel(idTopic, document, left) =>
      context become receive(topicsState.addRequest(idTopic, document, left, sender()))
    case GotRel(idTopic) =>
      context become receive(topicsState.removeRequest(idTopic))
    case Wait(idTopic) =>
      context become receive(topicsState.changeStatus(idTopic, "wait"))
    case Done(idTopic) =>
      context become receive(topicsState.changeStatus(idTopic, "done"))
  }

  def receive(topicsState: TopicsState) = start(topicsState) orElse runRoute(myRoute(topicsState))

  def receive = receive(new TopicsState())

  def stopService(): Unit = {
    context.stop(self)
  }

  def myRoute(topicsState: TopicsState) = {
    import MasterJsonProtocol._

    path("") {
      get {
        respondWithMediaType(`application/json`) {
          complete {
            Response(
              (topicsState.requests.map(e => TopicState(e._1, e._2._1.id, e._2._2)).toList
                ::: topicsState.status.map(e => TopicState(e._1, "", -1, e._2)).toList).sortBy(_.topic),
              "OK").toJson.prettyPrint
          }
        }
      }
    } ~
      pathPrefix("judge") {
        get {
          parameters('topic.as[Int], 'document.as[String], 'rel.as[Int]).as(TopicDocumentRel) {
            elem: TopicDocumentRel => {
              if (!topicsState.requests.contains(elem.topic)) {
                respondWithMediaType(`application/json`) {
                  complete {
                    ErrorResponse("Error: " + "The topic " + elem.topic + " is not in the request list!").toJson.prettyPrint
                  }
                }
              } else {
                val request = topicsState.requests.get(elem.topic).get
                if (request._1.id != elem.document) {
                  respondWithMediaType(`application/json`) {
                    complete {
                      ErrorResponse("Error: " + "The document " + elem.document + " is not in the request list!").toJson.prettyPrint
                    }
                  }
                } else {
                  respondWithMediaType(`application/json`) {
                    self ! GotRel(elem.topic)
                    val request = topicsState.requests.get(elem.topic).get
                    request._3 ! elem.rel
                    complete {
                      ErrorResponse("OK").toJson.prettyPrint
                    }
                  }
                }
              }
            }
          }
        }
      }
  }
}
