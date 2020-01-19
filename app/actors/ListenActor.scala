package actors

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._
import controllers.Deciders.{DecisionController, DecisionMaker, FirstTryDecider, StatisticDecisionController}
import models.baseImpl.Game
import play.api.libs.json._
import services.{CityCalculator, JsonGenerator, LoggingService, StatisticLoggingService}
import akka.http.scaladsl.Http

import scala.concurrent.duration._
import scala.io.StdIn

object ListenActor {

  case class Start(sender: ActorRef, count: Int)

  case class PoisonPill()

}

class ListenActor extends Actor {

  import ListenActor._

  implicit val system = ActorSystem("Pandemie")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val basePort = 50123

  override def receive: Receive = {
    case Start(sender: ActorRef, count: Int) =>

      implicit val readGame: Reads[Game] = new Reads[Game] {
        def reads(js: JsValue): JsResult[Game] = {
          val events: JsValue = if (js.as[JsObject].keys.contains("events")) {
            (js \ "events").as[JsValue]
          } else {
            null
          }
          JsSuccess(Game((js \ "round").as[Int], (js \ "points").as[Int], (js \ "outcome").as[String], (js \ "cities").as[JsValue], events))
        }
      }

      val route = extractStrictEntity(3.seconds) { strict =>
        val myGame = Json.fromJson[Game](Json.parse(strict.getData().decodeString("utf8"))).get
        //val decider: DecisionMaker = FirstTryDecider(myGame)
        //val decider: DecisionMaker = DecisionController(myGame)
        val decider: DecisionMaker = StatisticDecisionController(myGame)
        sender ! JsonGenerator().generateGameVizJson(myGame)
        println(myGame.round + "\n" + myGame.outcome + "\n\n")
        LoggingService().logGameState(myGame)
        StatisticLoggingService().log(myGame, decider.toString)
        complete(decider.decide())
        //complete(DecisionController().decide(myGame))
        //complete(StatisticDecisionController().decide(myGame))
      }

      val bindingFuture = Http().bindAndHandle(route, "localhost", basePort + count)

      println(s"Listener online at http://localhost:" + (basePort + count) + "/\nSend PoisonPill to stop...")
      StdIn.readLine() // let it run until user presses return
      bindingFuture
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => system.terminate()) // and shutdown when done

    case PoisonPill =>
      system.terminate()
  }
}
