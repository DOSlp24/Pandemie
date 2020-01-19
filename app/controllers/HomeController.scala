package controllers

import actors.{ListenActor, StartProcessActor}
import actors.ListenActor.Start
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.{ActorMaterializer, Materializer}
import javax.inject._
import play.api.mvc._
import akka.pattern.ask
import akka.util.Timeout
import models.GameInterface
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import java.util.concurrent.atomic.AtomicInteger

import actors.StartProcessActor.RunIcExe

import scala.concurrent.duration._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents)(implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {
  val counter = new AtomicInteger()

  object HomeControllerFactory {
    def create(out: ActorRef): Props = {
      Props(new PandemieDisplayWebsocketActor(out))
    }
  }

  class PandemieDisplayWebsocketActor(out: ActorRef) extends Actor {
    implicit val timeout: Timeout = 2.minutes

    val ListenerActor: ActorRef = system.actorOf(Props[ListenActor])
    val StartProcessActor: ActorRef = system.actorOf(Props[StartProcessActor])
    val myCount: Int = counter.get()

    out ! "Socket " + myCount

    ListenerActor ? Start(self, counter.getAndIncrement())

    override def receive: Receive = {
      case game: GameInterface =>
        out ! game.prettyPrint()
      case js: JsValue =>
        out ! js.toString()
      case msg =>
        println("Message from *" + myCount + "*: " + msg)
        if (msg == "New Run please") {
          StartProcessActor ? RunIcExe(myCount)
        }
    }
  }

  /**
    * Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index = Action { request =>

    println(request.body)
    Ok(views.html.index("Hey"))
  }

  def webSocket = WebSocket.accept[String, String] {
    request =>
      ActorFlow.actorRef { out =>
        HomeControllerFactory.create(out)
      }
  }
}
