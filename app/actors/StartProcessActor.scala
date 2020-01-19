package actors

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.stream.ActorMaterializer

object StartProcessActor {

  case class RunIcExe(count: Int)

  case class PoisonPill()

}

class StartProcessActor extends Actor {

  import StartProcessActor._
  import sys.process._

  implicit val system = ActorSystem("Pandemie")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val basePort = 50123

  override def receive: Receive = {
    case RunIcExe(count) =>
      "ic_exe/ic20_windows.exe -u \"http://localhost:" + (basePort + count) + "\"" !
    case PoisonPill =>
      system.terminate()
  }
}
