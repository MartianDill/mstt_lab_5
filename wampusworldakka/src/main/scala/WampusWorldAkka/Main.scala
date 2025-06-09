package WampusWorldAkka

import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

// головний об'єкт для запуску системи акторів
object Main extends App {

  // поведінка кореневого актора, який створює та запускає інших акторів
  def apply(): Behavior[String] = Behaviors.setup { context =>
    context.log.info("Запуск системи Wampus World Akka...")

    // створення акторів
    val environmentActor = context.spawn(EnvironmentActor(), "environmentActor")
    val navigatorActor = context.spawn(NavigatorActor(), "navigatorActor")
    val explorerActor = context.spawn(ExplorerActor(), "explorerActor")

    // реєстрація агентів в Akka System
    // запуск гри: відправити повідомлення Start в EnvironmentActor, передавши йому посилання на ExplorerActor та NavigatorActor.
    environmentActor ! EnvironmentActor.StartGame(explorerActor, navigatorActor)
    explorerActor ! ExplorerActor.Start(environmentActor, navigatorActor)
    Behaviors.empty // кореневий актор не робить нічого після створення дочірніх акторів
  }

  // запуск Akka ActorSystem, працюватиме поки всі актори не зупиняться або програма не завершиться
  val system = ActorSystem(Main(), "WampusWorldAkkaSystem")
}