package WampusWorldAkka

import scala.collection.mutable
import scala.util.Random

// об'єкт-контейнер для всіх допоміжних класів та переліків світу Вампуса
object WampusWorldCore {

  // можливі напрямки руху або орієнтації спелеолога
  enum Direction {
    case NORTH // північ
    case EAST  // схід
    case SOUTH // південь
    case WEST   // захід
  }

  // можливі сприйняття які актор-спелеолог може отримати від середовища
  enum PerceptionType {
    case STENCH  // запах Вампуса
    case BREEZE  // вітер від ями
    case GLITTER // блиск золота (якщо спелеолог у кімнаті з золотом)
    case BUMP    // зіткнення зі стіною
    case SCREAM  // крик Вампуса після пострілу
    case SAFE     // безпечна кімната (немає запаху, вітру, Вампуса, ями)
  }

  // можливі дії які спелеолог може виконати
  enum Action {
    case TURN_LEFT  // повернути ліворуч
    case TURN_RIGHT // повернути праворуч
    case FORWARD    // рухатися вперед
    case SHOOT      // вистрілити
    case GRAB       // підібрати золото
    case CLIMB      // вийти зі світу
    case NO_ACTION   // немає дії коли дія не визначена
  }

  // опис окремої кімнати у світі Вампуса
  case class Room(
                   var hasWampus: Boolean = false,
                   var hasPit: Boolean = false,
                   var hasGold: Boolean = false,
                   var hasStench: Boolean = false, // запах
                   var hasBreeze: Boolean = false  // вітер
                 )

  // клас для внутрішнього представлення світу навігатором
  // містить інформацію про кожну клітинку, що відома навігатору
  case class NavigatorCellInfo(
                                var visited: Boolean = false,
                                var safe: Boolean = true, // що кімната безпечна поки не виявлено протилежне
                                var wampusSuspect: Boolean = false, // підозра на Вампуса
                                var pitSuspect: Boolean = false,    // підозра на яму
                                var isWampusConfirmed: Boolean = false, // Вампус підтверджений
                                var isPitConfirmed: Boolean = false,    // яма підтверджена
                                var isGoldConfirmed: Boolean = false,   // золото підтверджене
                                var perceivedGlitter: Boolean = false,  // помічено блиск
                                var perceivedStench: Boolean = false,   // помічено запах
                                var perceivedBreeze: Boolean = false    // помічено вітер
                              ) {
    // оновлення безпечного статусу на основі сприйнять
    def updateSafeStatus(): Unit = {
      if (perceivedStench || perceivedBreeze) {
        safe = false
      } else {
        safe = true
      }
    }

    // встановлення підозри на яму
    def setPitSuspect(safe: Boolean): Unit = {
      if (!safe) {
        pitSuspect = true
      }
    }

    // встановлення підозри на вампуса
    def setWampusSuspect(safe: Boolean): Unit = {
      if (!safe) {
        wampusSuspect = true
      }
    }
  }

  // клас, що представляє світ Вампуса
  class WampusWorld(val size: Int = 4) {
    private val grid: Array[Array[Room]] = Array.ofDim[Room](size, size)
    private val random = new Random()

    private var wampusX: Int = _
    private var wampusY: Int = _
    private var goldX: Int = _
    private var goldY: Int = _
    private var wampusAlive: Boolean = true

    // ініціалізація світу з ямами, Вампусом та золотом
    initializeWorld()

    // ініціалізація світу, розміщення об'єктів
    private def initializeWorld(): Unit = {
      // ініціалізація всіх кімнат
      for (i <- 0 until size; j <- 0 until size) {
        grid(i)(j) = Room()
      }

      // розміщення Вампуса (не в (0,0))
      while ({
        wampusX = random.nextInt(size)
        wampusY = random.nextInt(size)
        (wampusX == 0 && wampusY == 0) // умова перевіряється після виконання блоку
      }) {}

      grid(wampusX)(wampusY).hasWampus = true
      addStench(wampusX, wampusY)

      // розміщення золота (не в (0,0), не там, де Вампус)
      while ({
        goldX = random.nextInt(size)
        goldY = random.nextInt(size)
        ((goldX == 0 && goldY == 0) || (goldX == wampusX && goldY == wampusY)) // умова перевіряється після виконання блоку
      }) {}

      grid(goldX)(goldY).hasGold = true

      // розміщення ям (до 3, не в (0,0), не там де Вампус, не там де золото)
      var pitsPlaced = 0
      while (pitsPlaced < 3) {
        val pitX = random.nextInt(size)
        val pitY = random.nextInt(size)
        if (!((pitX == 0 && pitY == 0) || (pitX == wampusX && pitY == wampusY) || (pitX == goldX && pitY == goldY) || grid(pitX)(pitY).hasPit)) {
          grid(pitX)(pitY).hasPit = true
          addBreeze(pitX, pitY)
          pitsPlaced += 1
        }
      }
    }

    // додавання запаху до сусідніх кімнат Вампуса
    private def addStench(x: Int, y: Int): Unit = {
      getNeighbors(x, y).foreach(p => grid(p._1)(p._2).hasStench = true)
    }

    // додавання вітру до сусідніх кімнат з ямами
    private def addBreeze(x: Int, y: Int): Unit = {
      getNeighbors(x, y).foreach(p => grid(p._1)(p._2).hasBreeze = true)
    }

    // отримання сусідніх координат
    private def getNeighbors(x: Int, y: Int): Set[(Int, Int)] = {
      val neighbors = new mutable.HashSet[(Int, Int)]()
      for {
        dx <- -1 to 1
        dy <- -1 to 1
        if dx != 0 || dy != 0 // виключення поточної кімнати
      } {
        val nx = x + dx
        val ny = y + dy
        if (nx >= 0 && nx < size && ny >= 0 && ny < size) {
          neighbors.add((nx, ny))
        }
      }
      neighbors.toSet
    }

    // отримання кімнати за координатами
    def getRoom(x: Int, y: Int): Room = grid(x)(y)

    // перевірка чи Вампус живий
    def isWampusAlive(): Boolean = wampusAlive

    // вбивство Вампуса
    def killWampus(): Unit = {
      wampusAlive = false
      // видалення запаху Вампуса з усіх кімнат після його смерті
      for (i <- 0 until size; j <- 0 until size) {
        grid(i)(j).hasStench = false
      }
    }
  }
}