package WampusWorldAkka

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import WampusWorldAkka.WampusWorldCore.{Action, Direction, NavigatorCellInfo, PerceptionType}
import scala.collection.mutable
import scala.util.Random

// актор-навігатор, відповідає за обробку сприйнять спелеолога, побудову внутрішньої карти світу
// та прийняття рішень щодо наступної дії
object NavigatorActor {

  // повідомлення, які може отримувати NavigatorActor
  sealed trait Command

  // обробка сприйнять від ExplorerActor
  case class ProcessPerceptions(perceptions: Set[PerceptionType], explorer: ActorRef[ExplorerActor.Command]) extends Command
  // повідомлення про завершення гри від EnvironmentActor
  case object GameOver extends Command

  // стан навігатора
  case class NavigatorState(
                             worldMap: Array[Array[NavigatorCellInfo]],
                             gridSize: Int,
                             var currentExplorerX: Int,
                             var currentExplorerY: Int,
                             var currentExplorerDirection: Direction,
                             var hasArrow: Boolean,
                             var hasGold: Boolean,
                             var goldLocation: Option[(Int, Int)],
                             path: mutable.Stack[(Int, Int)], // шлях для повернення до початкової точки
                             visitedRooms: mutable.Set[(Int, Int)], // відвідані кімнати
                             safeRooms: mutable.Set[(Int, Int)], // підтверджені безпечні кімнати
                           )


  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val gridSize = 4
    val initialWorldMap: Array[Array[NavigatorCellInfo]] = Array.ofDim[NavigatorCellInfo](gridSize, gridSize)
    for (i <- 0 until gridSize; j <- 0 until gridSize) {
      initialWorldMap(i)(j) = NavigatorCellInfo()
    }

    // ініціалізація початкового стану навігатора
    val initialState = NavigatorState(
      worldMap = initialWorldMap,
      gridSize = gridSize,
      currentExplorerX = 0,
      currentExplorerY = 0,
      currentExplorerDirection = Direction.EAST,
      hasArrow = true,
      hasGold = false,
      goldLocation = None, // ініціалізація goldLocation
      path = mutable.Stack(),
      visitedRooms = mutable.Set(),
      safeRooms = mutable.Set((0, 0)) // початкова кімната завжди безпечна
    )

    // функція для оновлення карти світу та стану навігатора
    def updateMapAndState(state: NavigatorState, perceptions: Set[PerceptionType]): NavigatorState = {
      val currentCell = state.worldMap(state.currentExplorerX)(state.currentExplorerY)

      currentCell.visited = true
      state.visitedRooms += ((state.currentExplorerX, state.currentExplorerY))

      // оновлення сприйнять у поточній клітинці
      currentCell.perceivedGlitter = perceptions.contains(PerceptionType.GLITTER)
      currentCell.perceivedStench = perceptions.contains(PerceptionType.STENCH)
      currentCell.perceivedBreeze = perceptions.contains(PerceptionType.BREEZE)

      // логіка для зняття підозр якщо кімната безпечна
      if (perceptions.contains(PerceptionType.SAFE)) {
        currentCell.safe = true
        // якщо кімната безпечна то сусідні кімнати не можуть мати Вампуса чи Яму
        val neighbors = getNeighbors(state.currentExplorerX, state.currentExplorerY, state.gridSize)
        neighbors.foreach { case (nx, ny) =>
          val neighborCell = state.worldMap(nx)(ny)
          if (!state.visitedRooms.contains((nx, ny))) { // тільки для невідвіданих сусідів
            neighborCell.pitSuspect = false
            neighborCell.wampusSuspect = false
            state.safeRooms += ((nx, ny)) // сусіди безпечної кімнати теж безпечні
          }
        }
      } else {
        currentCell.safe = false // якщо є будь-яке сприйняття, кімната не безпечна
        // якщо є запах або вітер додається підозра на сусідні клітинки
        val neighbors = getNeighbors(state.currentExplorerX, state.currentExplorerY, state.gridSize)
        neighbors.foreach { case (nx, ny) =>
          val neighborCell = state.worldMap(nx)(ny)
          if (!state.visitedRooms.contains((nx, ny))) { //для невідвіданих сусідів
            if (perceptions.contains(PerceptionType.STENCH)) {
              neighborCell.wampusSuspect = true
            }
            if (perceptions.contains(PerceptionType.BREEZE)) {
              neighborCell.pitSuspect = true
            }
          }
        }
      }

      // обробка золота
      if (perceptions.contains(PerceptionType.GLITTER)) {
        state.hasGold = true
        state.goldLocation = Some((state.currentExplorerX, state.currentExplorerY))
        currentCell.isGoldConfirmed = true
      }

      // обробка крику Вампуса
      if (perceptions.contains(PerceptionType.SCREAM)) {
        // очистити підозри на вампуса і підтвердити смерть вампуса
        for (i <- 0 until state.gridSize; j <- 0 until state.gridSize) {
          state.worldMap(i)(j).wampusSuspect = false
          if (state.worldMap(i)(j).isWampusConfirmed) {
            state.worldMap(i)(j).isWampusConfirmed = false // Вампус мертвий
          }
        }
      }
      state // повернення оновленого стану
    }

    // функція для вибору наступної дії спелеолога
    def chooseAction(state: NavigatorState, explorer: ActorRef[ExplorerActor.Command]): Action = {
      // 1. якщо є золото, підібрати його і спробувати вилізти
      if (state.hasGold && state.currentExplorerX == 0 && state.currentExplorerY == 0) {
        return Action.CLIMB
      }
      if (state.hasGold && (state.currentExplorerX != 0 || state.currentExplorerY != 0)) {
        // якщо золото є, але не в початковій кімнаті, іде до неї
        val initialRoom = (0, 0)
        navigateTo(state, initialRoom._1, initialRoom._2, explorer)
      } else {

        // 2. досліджувати безпечні та невідвідані кімнати
        val unvisitedSafeRooms = state.safeRooms.filterNot(state.visitedRooms.contains)
        if (unvisitedSafeRooms.nonEmpty) {
          // іде до найближчої безпечної невідвіданої кімнати
          val nextRoom = unvisitedSafeRooms.head
          navigateTo(state, nextRoom._1, nextRoom._2, explorer)
        } else {

          // 3. якщо немає безпечних невідвіданих кімнат, розглянути стрільбу або пошук шляху до підозрілих кімнат
          val wampusSuspectRooms = for {
            x <- 0 until state.gridSize
            y <- 0 until state.gridSize
            if state.worldMap(x)(y).wampusSuspect && !state.worldMap(x)(y).isWampusConfirmed
          } yield (x, y)

          if (state.hasArrow && wampusSuspectRooms.nonEmpty) {
            val targetWampus = wampusSuspectRooms.head
            Action.SHOOT
          } else {

            // 4. якщо нічого не виходить, повернутися назад по шляху
            if (state.path.nonEmpty) {
              val prevPos = state.path.pop() // береться остання позиція зі шляху
              // navigateTo повертає Action, що переміщує агента до prevPos
              navigateTo(state, prevPos._1, prevPos._2, explorer)
            } else {

              // 5. якщо немає куди йти, повернути NO_ACTION
              context.log.warn(s"Навігатор: неможливо дістатися до жодної кімнати або повернутися назад.")
              Action.NO_ACTION
            }
          }
        }
      }
    }


    // допоміжна функція для переміщення до цільової кімнати (або кроку до неї)
    def navigateTo(state: NavigatorState, targetX: Int, targetY: Int, explorer: ActorRef[ExplorerActor.Command]): Action = {
      if (state.currentExplorerX == targetX && state.currentExplorerY == targetY) {
        return Action.NO_ACTION // Вже в цільовій кімнаті
      }

      // збереження поточної позиції в шляху перед рухом
      state.path.push((state.currentExplorerX, state.currentExplorerY))

      val (dx, dy) = (targetX - state.currentExplorerX, targetY - state.currentExplorerY)

      // спочатку іде по X потім по Y
      if (dx != 0) {
        // повертається у правильному напрямку по X
        val desiredDirection = if (dx > 0) Direction.EAST else Direction.WEST
        if (state.currentExplorerDirection == desiredDirection) {
          return Action.FORWARD
        } else {
          // повертається до бажаного напрямку
          val currentDir = state.currentExplorerDirection
          // логіка повороту
          if ((currentDir == Direction.NORTH && desiredDirection == Direction.EAST) ||
            (currentDir == Direction.EAST && desiredDirection == Direction.SOUTH) ||
            (currentDir == Direction.SOUTH && desiredDirection == Direction.WEST) ||
            (currentDir == Direction.WEST && desiredDirection == Direction.NORTH)) {
            return Action.TURN_RIGHT
          } else {
            return Action.TURN_LEFT
          }
        }
      } else if (dy != 0) {
        // рухається по Y
        val desiredDirection = if (dy > 0) Direction.SOUTH else Direction.NORTH
        if (state.currentExplorerDirection == desiredDirection) {
          return Action.FORWARD
        } else {
          val currentDir = state.currentExplorerDirection
          // логіка повороту
          if ((currentDir == Direction.NORTH && desiredDirection == Direction.SOUTH) || // двічі повертаємо
            (currentDir == Direction.EAST && desiredDirection == Direction.NORTH) ||
            (currentDir == Direction.SOUTH && desiredDirection == Direction.WEST) ||
            (currentDir == Direction.WEST && desiredDirection == Direction.SOUTH)) {
            return Action.TURN_LEFT
          } else {
            return Action.TURN_RIGHT
          }
        }
      }
      Action.NO_ACTION //не повинно бути досяжним, якщо цільова кімната не та ж
    }

    // допоміжна функція для NavigatorActor для отримання сусідів
    def getNeighbors(x: Int, y: Int, gridSize: Int): Set[(Int, Int)] = {
      val neighbors = mutable.Set[(Int, Int)]()
      // Північ
      if (y > 0) neighbors += ((x, y - 1))
      // Схід
      if (x < gridSize - 1) neighbors += ((x + 1, y))
      // Південь
      if (y < gridSize - 1) neighbors += ((x, y + 1))
      // Захід
      if (x > 0) neighbors += ((x - 1, y))
      neighbors.toSet
    }

    def updateStateOnAction(state: NavigatorState, action: Action): NavigatorState = {
      action match {
        case Action.FORWARD =>
          val (newX, newY) = state.currentExplorerDirection match {
            case Direction.NORTH => (state.currentExplorerX, state.currentExplorerY - 1)
            case Direction.EAST  => (state.currentExplorerX + 1, state.currentExplorerY)
            case Direction.SOUTH => (state.currentExplorerX, state.currentExplorerY + 1)
            case Direction.WEST  => (state.currentExplorerX - 1, state.currentExplorerY)
          }
          // перевірка меж світу, якщо переміщення можливе
          if (newX >= 0 && newX < state.gridSize && newY >= 0 && newY < state.gridSize) {
            state.currentExplorerX = newX
            state.currentExplorerY = newY
          } else {
            // зіткнення зі стіною, агент-актор не перемістився
            context.log.info(s"Навігатор: зіткнення зі стіною при русі вперед. Позиція: (${state.currentExplorerX}, ${state.currentExplorerY})")
          }
        case Action.TURN_LEFT =>
          state.currentExplorerDirection = state.currentExplorerDirection match {
            case Direction.NORTH => Direction.WEST
            case Direction.EAST  => Direction.NORTH
            case Direction.SOUTH => Direction.EAST
            case Direction.WEST  => Direction.SOUTH
          }
        case Action.TURN_RIGHT =>
          state.currentExplorerDirection = state.currentExplorerDirection match {
            case Direction.NORTH => Direction.EAST
            case Direction.EAST  => Direction.SOUTH
            case Direction.SOUTH => Direction.WEST
            case Direction.WEST  => Direction.NORTH
          }
        case Action.SHOOT =>
          state.hasArrow = false
        case Action.GRAB =>
          state.hasGold = true
        case Action.CLIMB =>
        // актор-навігатор не змінює стан, бо вже оброблено в EnvironmentActor
        case Action.NO_ACTION => // Нічого не робити
      }
      state // повернення новго стану
    }

    // допоміжний метод для друку карти світу
    def printWorldMap(state: NavigatorState): Unit = {
      val sb = new StringBuilder()
      sb.append("Карта світу навігатора:\\n")
      for (j <- 0 until state.gridSize) { // по рядках У
        sb.append("| ")
        for (i <- 0 until state.gridSize) { //по стовпцях Х
          val cell = state.worldMap(i)(j) // доступ (X, Y)
          if (i == state.currentExplorerX && j == state.currentExplorerY) {
            sb.append("A") // Агент
          } else if (cell.isGoldConfirmed) {
            sb.append("G")
          } else if (cell.isWampusConfirmed) {
            sb.append("W")
          } else if (cell.isPitConfirmed) {
            sb.append("P")
          } else if (cell.safe) {
            sb.append("S")
          } else if (cell.wampusSuspect) {
            sb.append("w")
          } else if (cell.pitSuspect) {
            sb.append("p")
          } else if (cell.visited) {
            sb.append("V")
          } else {
            sb.append("?")
          }
          sb.append(" ")
        }
        sb.append("|\\n")
      }
      sb.append(s"Позиція спелеолога: (${state.currentExplorerX}, ${state.currentExplorerY}), Напрямок: ${state.currentExplorerDirection}\\n")
      sb.append(s"Наявність стріли: ${state.hasArrow}, Наявність золота: ${state.hasGold}\\n")
      context.log.info(sb.toString())
    }

    Behaviors.receivePartial {
      case (context, ProcessPerceptions(perceptions, explorerRef)) =>
        // оновити стан навігатора на основі сприйнять
        val newState = updateMapAndState(initialState, perceptions)
        printWorldMap(newState)

        // вибрати наступну дію
        val nextAction = chooseAction(newState, explorerRef)
        context.log.info(s"Навігатор рекомендує дію: $nextAction")

        // відправити дію спелеологу
        explorerRef ! ExplorerActor.PerformAction(nextAction)

        Behaviors.same

      case (context, GameOver) =>
        context.log.info("Навігатор отримав повідомлення про завершення гри.")
        Behaviors.stopped
    }
  }
}