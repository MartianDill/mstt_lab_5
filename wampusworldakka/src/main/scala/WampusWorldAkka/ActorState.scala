package WampusWorldAkka
import WampusWorldCore.Direction

// клас, що описує поточний стан спелеолога у світі Вампуса
// містить координати, напрямок, наявність стріли та золота
case class ActorState(
                       var x: Int,
                       var y: Int,
                       var direction: Direction,
                       var hasArrow: Boolean = true, // спелеолог починає з однією стрілою
                       var hasGold: Boolean = false  // не має золота
                     ) {
  // переміщення спелеолога вперед на одну клітинку
  // @param maxX максимальна X-координата (розмір світу - 1)
  // @param maxY максимальна Y-координата (розмір світу - 1)
  // @return true, якщо переміщення успішне, false - якщо зіткнувся зі стіною
  def moveForward(maxX: Int, maxY: Int): Boolean = {
    var newX = x
    var newY = y
    direction match {
      case Direction.NORTH => newY -= 1
      case Direction.EAST  => newX += 1
      case Direction.SOUTH => newY += 1
      case Direction.WEST  => newX -= 1
    }

    // перевірка меж світу
    if (newX >= 0 && newX < maxX && newY >= 0 && newY < maxY) {
      x = newX
      y = newY
      true
    } else {
      false // зіткнувся зі стіною
    }
  }

  // повертає спелеолога ліворуч
  def turnLeft(): Unit = {
    direction = direction match {
      case Direction.NORTH => Direction.WEST
      case Direction.EAST  => Direction.NORTH
      case Direction.SOUTH => Direction.EAST
      case Direction.WEST  => Direction.SOUTH
    }
  }

  // повертає спелеолога праворуч
  def turnRight(): Unit = {
    direction = direction match {
      case Direction.NORTH => Direction.EAST
      case Direction.SOUTH => Direction.WEST
      case Direction.EAST  => Direction.SOUTH
      case Direction.WEST  => Direction.NORTH
    }
  }

  // використання стріли
  // @return true, якщо стріла була використана, false - якщо стріл немає
  def useArrow(): Boolean = {
    if (hasArrow) {
      hasArrow = false
      true
    } else {
      false
    }
  }

  // оновлення наявності золота
  def setHasGold(hasGold: Boolean): Unit = {
    this.hasGold = hasGold
  }

  override def toString: String = {
    s"Стан спелеолога: [x=$x, y=$y, напрямок=$direction, має стрілу=$hasArrow, має золото=$hasGold]"
  }
}