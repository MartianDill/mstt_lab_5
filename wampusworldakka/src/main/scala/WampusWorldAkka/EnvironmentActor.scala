package WampusWorldAkka

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import WampusWorldCore.{Action, Direction, PerceptionType, Room, WampusWorld}

// актор-середовище, відповідає за моделювання світу Вампуса,
// управління станом спелеолога та обробку його дій
object EnvironmentActor {

  // повідомлення, які може отримувати EnvironmentActor
  sealed trait Command

  // запит на ініціалізацію гри
  case class StartGame(explorer: ActorRef[ExplorerActor.Command], navigator: ActorRef[NavigatorActor.Command]) extends Command
  // запит на виконання дії від ExplorerActor
  case class PerformAction(action: Action) extends Command
  // повідомлення про вихід зі світу (для завершення гри)
  case object ClimbOut extends Command
  // внутрішнє повідомлення для завершення актора
  private case object Stop extends Command


  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val wampusWorld = new WampusWorld()
    val explorerState = ActorState(0, 0, Direction.EAST) // початкова позиція та напрямок спелеолога
    var explorerAID: Option[ActorRef[ExplorerActor.Command]] = None
    var navigatorAID: Option[ActorRef[NavigatorActor.Command]] = None
    var initialTime: Long = 0
    var currentTurn: Int = 0
    var gameOver: Boolean = false

    // допоміжний метод для друку стану середовища
    def printEnvironmentState(): Unit = {
      val sb = new StringBuilder()
      sb.append("Поточний стан світу Вампуса:\n")
      for (i <- 0 until wampusWorld.size) {
        for (j <- 0 until wampusWorld.size) {
          sb.append("|")
          val currentRoom = wampusWorld.getRoom(j, i)
          if (explorerState.x == j && explorerState.y == i) {
            sb.append("A") // агент-актор спелеолог
          } else {
            if (currentRoom.hasGold) sb.append("G")
            if (currentRoom.hasPit) sb.append("P")
            if (currentRoom.hasWampus && wampusWorld.isWampusAlive()) sb.append("W")
            if (currentRoom.hasStench) sb.append("S")
            if (currentRoom.hasBreeze) sb.append("B")
            sb.append(" ")
          }
        }
        sb.append("|\n") // кінець рядка
      }
      sb.append(explorerState.toString).append("\n")
      context.log.info(sb.toString())
    }

    // обробка завершення гри
    def handleGameOver(message: String): Unit = {
      if (!gameOver) {
        gameOver = true
        val endTime = System.currentTimeMillis()
        context.log.info(s"Гра завершена: $message")
        context.log.info(s"Гра завершена після $currentTurn ходів за ${endTime - initialTime} мс.")
        explorerAID.foreach(_ ! ExplorerActor.GameOver(message))
        navigatorAID.foreach(_ ! NavigatorActor.GameOver)
        context.self ! Stop // зупинка актора EnvironmentActor
      }
    }

    Behaviors.receiveMessage {
      case StartGame(explorer, navigator) =>
        explorerAID = Some(explorer)
        navigatorAID = Some(navigator)
        initialTime = System.currentTimeMillis()
        currentTurn = 0
        context.log.info("Гра Wumpus World розпочата!")
        printEnvironmentState()
        // відправка початкових сприйняттів
        val currentRoom = wampusWorld.getRoom(explorerState.x, explorerState.y)
        val perceptions = collection.mutable.Set[PerceptionType]()
        if (currentRoom.hasStench && wampusWorld.isWampusAlive()) perceptions += PerceptionType.STENCH
        if (currentRoom.hasBreeze) perceptions += PerceptionType.BREEZE
        if (currentRoom.hasGold) perceptions += PerceptionType.GLITTER
        if (perceptions.isEmpty) perceptions += PerceptionType.SAFE

        explorerAID.foreach(_ ! ExplorerActor.Perceive(perceptions.toSet))
        Behaviors.same

      case PerformAction(action) =>
        if (gameOver) {
          Behaviors.same
        } else {
          currentTurn += 1
          var bumped = false
          var screamed = false
          var outcomeMessage: Option[String] = None

          action match {
            case Action.FORWARD =>
              val moved = explorerState.moveForward(wampusWorld.size, wampusWorld.size)
              if (!moved) {
                bumped = true
              }
              val currentRoom = wampusWorld.getRoom(explorerState.x, explorerState.y)
              if (currentRoom.hasPit) {
                outcomeMessage = Some("Спелеолог впав у яму. Гра завершена.")
              } else if (currentRoom.hasWampus && wampusWorld.isWampusAlive()) {
                outcomeMessage = Some("Вампус з'їв спелеолога. Гра завершена.")
              }

            case Action.TURN_LEFT =>
              explorerState.turnLeft()

            case Action.TURN_RIGHT =>
              explorerState.turnRight()

            case Action.GRAB =>
              val currentRoom = wampusWorld.getRoom(explorerState.x, explorerState.y)
              if (currentRoom.hasGold) {
                explorerState.setHasGold(true)
                currentRoom.hasGold = false // Золото підібрано
                context.log.info("Спелеолог підібрав золото!")
              } else {
                context.log.warn("Спелеолог намагався підібрати золото, але його немає.")
              }

            case Action.SHOOT =>
              if (explorerState.useArrow()) {
                val (targetX, targetY) = explorerState.direction match {
                  case Direction.NORTH => (explorerState.x, explorerState.y - 1)
                  case Direction.EAST  => (explorerState.x + 1, explorerState.y)
                  case Direction.SOUTH => (explorerState.x, explorerState.y + 1)
                  case Direction.WEST  => (explorerState.x - 1, explorerState.y)
                }

                if (targetX >= 0 && targetX < wampusWorld.size && targetY >= 0 && targetY < wampusWorld.size) {
                  val targetRoom = wampusWorld.getRoom(targetX, targetY)
                  if (targetRoom.hasWampus && wampusWorld.isWampusAlive()) {
                    wampusWorld.killWampus()
                    screamed = true
                    context.log.info("Вампус убитий!")
                  } else {
                    context.log.info("Постріл у нікуди.")
                  }
                } else {
                  context.log.info("Постріл у стіну.")
                }
              } else {
                context.log.warn("Спелеолог намагався вистрілити, але не має стріл.")
              }

            case Action.CLIMB =>
              if (explorerState.x == 0 && explorerState.y == 0) {
                if (explorerState.hasGold) {
                  outcomeMessage = Some("Спелеолог виліз зі світу з золотом! Перемога!")
                } else {
                  outcomeMessage = Some("Спелеолог виліз зі світу без золота. Програш.")
                }
              } else {
                context.log.warn("Спелеолог намагався вилізти, але не знаходиться у початковій кімнаті.")
              }

            case Action.NO_ACTION =>
              context.log.info("Спелеолог не виконав жодної дії.")
          }

          printEnvironmentState()

          outcomeMessage.foreach(handleGameOver)

          if (!gameOver) {
            // відправлення нових сприйнять
            val currentRoom = wampusWorld.getRoom(explorerState.x, explorerState.y)
            val perceptions = collection.mutable.Set[PerceptionType]()
            if (currentRoom.hasStench && wampusWorld.isWampusAlive()) perceptions += PerceptionType.STENCH
            if (currentRoom.hasBreeze) perceptions += PerceptionType.BREEZE
            if (currentRoom.hasGold) perceptions += PerceptionType.GLITTER
            if (bumped) perceptions += PerceptionType.BUMP
            if (screamed) perceptions += PerceptionType.SCREAM
            if (perceptions.isEmpty) perceptions += PerceptionType.SAFE // якщо немає інших сприйнять, то безпечно

            explorerAID.foreach(_ ! ExplorerActor.Perceive(perceptions.toSet))
          }
          Behaviors.same
        }

      case ClimbOut =>
        handleGameOver("Спелеолог виліз зі світу.")
        Behaviors.same

      case Stop =>
        Behaviors.stopped // зупинити актора
    }
  }
}