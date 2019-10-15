package controllers

import actors.ListenActor
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

import scala.concurrent.duration._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents)(implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {

  object HomeControllerFactory {
    def create(out: ActorRef) = {
      Props(new PandemieDisplayWebsocketActor(out))
    }
  }

  class PandemieDisplayWebsocketActor(out: ActorRef) extends Actor {
    implicit val timeout: Timeout = 99999.seconds

    val ListenerActor: ActorRef = system.actorOf(Props[ListenActor])

    ListenerActor ? Start(self)

    override def receive: Receive = {
      case game: GameInterface =>
        out ! game.prettyPrint()
      case js: JsValue =>
        out ! js.toString()
      case msg =>
        println(msg)
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
    //Ok(views.html.index("Your new application is ready."))
    Ok(views.html.index("Hey"))
  }

  def webSocket = WebSocket.accept[String, String] {
    request =>
      ActorFlow.actorRef { out =>
        HomeControllerFactory.create(out)
      }
  }
}
