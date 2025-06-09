package WampusWorldAkka

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import WampusWorldCore.{Action, PerceptionType}
import scala.concurrent.duration._
import scala.util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

// актор-спелеолог, який взаємодіє з середовищем та навігатором, перетворює сприйняття у природну мову, 
// надсилає їх навігатору та виконує дії, рекомендовані навігатором
object ExplorerActor {

  // повідомлення які може отримувати ExplorerActor
  sealed trait Command

  // ініціалізація актора
  case class Start(environment: ActorRef[EnvironmentActor.Command], navigator: ActorRef[NavigatorActor.Command]) extends Command
  // отримання сприйнять від середовища
  case class Perceive(perceptions: Set[PerceptionType]) extends Command
  // отримання рекомендованої дії від навігатора
  case class PerformAction(action: Action) extends Command
  // повідомлення про завершення гри від середовища
  case class GameOver(outcomeMessage: String) extends Command

  // випадковий вибір синонімів
  private val random = new Random()

  // мапа синонімів для різних типів сприйнять
  private val PERCEPTION_SYNONYMS = new HashMap[PerceptionType, List[String]]()
  PERCEPTION_SYNONYMS.put(PerceptionType.STENCH, List("чую сморід", "відчуваю жахливий запах", "смердить"))
  PERCEPTION_SYNONYMS.put(PerceptionType.BREEZE, List("відчуваю вітер", "легкий подих вітру", "вітер"))
  PERCEPTION_SYNONYMS.put(PerceptionType.GLITTER, List("бачу блиск", "щось блищить", "блищить золото"))
  PERCEPTION_SYNONYMS.put(PerceptionType.BUMP, List("зіткнувся зі стіною", "врізався", "уперся"))
  PERCEPTION_SYNONYMS.put(PerceptionType.SCREAM, List("чую крик", "хтось кричить", "пролунав крик вампуса"))
  PERCEPTION_SYNONYMS.put(PerceptionType.SAFE, List("безпечно", "все спокійно", "немає небезпеки"))

  // мапа ключових слів для розбору дій з природно-мовного повідомлення
  private val ACTION_KEYWORDS = new HashMap[String, Action]()
  ACTION_KEYWORDS.put("повернути ліворуч", Action.TURN_LEFT)
  ACTION_KEYWORDS.put("повернути праворуч", Action.TURN_RIGHT)
  ACTION_KEYWORDS.put("рухатися вперед", Action.FORWARD)
  ACTION_KEYWORDS.put("вистрілити", Action.SHOOT)
  ACTION_KEYWORDS.put("підібрати", Action.GRAB)
  ACTION_KEYWORDS.put("вибратися", Action.CLIMB)

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    var environmentAID: Option[ActorRef[EnvironmentActor.Command]] = None
    var navigatorAID: Option[ActorRef[NavigatorActor.Command]] = None
    var gameStarted: Boolean = false

    // отримання випадкового синоніму для даного типу сприйняття
    def getRandomSynonym(perceptionType: PerceptionType): String = {
      PERCEPTION_SYNONYMS.get(perceptionType) match {
        case Some(synonyms) if synonyms.nonEmpty => synonyms(random.nextInt(synonyms.size))
        case _ => "невідоме сприйняття."
      }
    }

    // формування природно-мовного повідомлення на основі сприйнять
    def formatPerceptions(perceptions: Set[PerceptionType]): String = {
      val sb = new StringBuilder()
      perceptions.foreach { p =>
        if (sb.nonEmpty) sb.append(", ")
        sb.append(getRandomSynonym(p))
      }
      if (sb.isEmpty) {
        sb.append("нічого не сприймаю.")
      }
      sb.toString()
    }

    // граматичний розбір природно-мовного повідомлення від навігатора
    def parseActionMessage(actionMessage: String): Option[Action] = {
      val lowerCaseMessage = actionMessage.toLowerCase.trim
      ACTION_KEYWORDS.find { case (keyword, _) => lowerCaseMessage.contains(keyword) }.map(_._2)
    }

    Behaviors.receiveMessage {
      case Start(env, nav) =>
        environmentAID = Some(env)
        navigatorAID = Some(nav)
        gameStarted = true
        context.log.info("Агент-спелеолог розпочав роботу.")
        Behaviors.same

      case Perceive(perceptions) =>
        if (!gameStarted) {
          context.log.warn("Агент-спелеолог отримав сприйняття до початку гри.")
          Behaviors.same
        } else {
          val naturalLanguagePerceptions = formatPerceptions(perceptions)
          context.log.info(s"Спелеолог сприймає: $naturalLanguagePerceptions")

          navigatorAID.foreach { nav =>
            nav ! NavigatorActor.ProcessPerceptions(perceptions.toSet, context.self)
          }
          Behaviors.same
        }

      case PerformAction(action) =>
        if (!gameStarted) {
          context.log.warn("Агент-спелеолог отримав дію до початку гри.")
          Behaviors.same
        } else {
          context.log.info(s"Спелеолог виконує дію: $action")
          environmentAID.foreach { env =>
            env ! EnvironmentActor.PerformAction(action)
          }
          Behaviors.same
        }

      case GameOver(outcomeMessage) =>
        context.log.info(s"Гра завершена (від ExplorerActor): $outcomeMessage")
        Behaviors.stopped // зупинити актора
    }
  }
}