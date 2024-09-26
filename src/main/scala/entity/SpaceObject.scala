package entity
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.Random

trait SpaceObject {
  def get_object_type: String
  def get_name: String
  def getLevel: Int
  def getExp: Float
  def getStamina: Int
  def action: Move
}
// agent go through the grid with some random up down  and if he face wall he get -1 and 1 if he found the exit door
trait CombatStyle {
 def performAttack(obj1: Option[SpaceObject], obj2: Option[SpaceObject]): Boolean = {
  (obj1, obj2) match {
    case (Some(attacker: Player[_]), Some(defender: Monster)) =>
       while (attacker.getStamina > 0 && defender.getStamina > 0) {

        println(s"${attacker.get_name}, choose an item to attack with:")
        for (i <- attacker.items.indices) {
          println(s"($i) ${attacker.items(i).get_name}")
        }
        val choice = try {
          scala.io.StdIn.readLine().toInt
        } catch {
          case _: NumberFormatException => -1
        }

        if (choice < 0 || choice >= attacker.items.length) {
          println("Invalid choice. Try again.")
          
        }
        val sword = attacker.items(choice)
        val damage = if (sword != null) attacker.action.increment_exp + 1 else 0
        println(s"${attacker.get_name} attacks ${defender.get_name} with ${sword.get_name} causing $damage damage!")
        val newStaminaDefender = defender.getStamina - damage
        defender.setStamina(newStaminaDefender)

        if (defender.getStamina <= 0) {
          println(s"${defender.get_name} has been defeated!")
          return true
        }
        val attackStamina = attacker.getStamina - defender.action.increment_exp
        attacker.setStamina(attackStamina)

        if (attacker.getStamina <= 0) {
          println(s"${attacker.get_name} has been defeated!")
          return false
        }
        println(s"${defender.get_name} counter-attacks! ${attacker.get_name} now has ${attacker.getStamina} stamina.")
      }
      
     
      if (attacker.getStamina < 0) false
      else true
  
      
       
    case (Some(attacker: Player[_]), Some(_: Item)) =>
      println("Defender is Wall")
      true

    case (Some(_: Player[_]), None) =>
      println("Defender not found.")
      false

    case (None, Some(_: Monster)) =>
      println("Attacker not found.")
      false

   
  }
}
def pickItem(player: Option[SpaceObject], Item :Option[SpaceObject]) : Unit = {
  (player, Item) match {
    case (Some(player: Player[_]), Some(item: Item)) =>
      println(s"${player.get_name} picked ${item.get_name}")
      player.addItem(item)
      if (item.name == "sword") {
        println("Congratulations! You found a sword!")
        player.setStamina(player.getStamina +10 )
        player.setLevel(player.getLevel +1)
      }
      item.set_owner(Some(player))
    case (Some(_), None) =>
      println("Item not found.")
  }

}


} 

class Move(val name: String, val increment_exp: Int = 0)

case class Person(name: String) extends SpaceObject {
  override def get_object_type: String = "Person"
  override def get_name: String = name
  override def getLevel: Int = 1
  override def getExp: Float = 1.0f
  override def getStamina: Int = 1
  override def action: Move = new Move("None")
}

case class Item(name: String) extends SpaceObject {
  override def get_object_type: String = "Item"
  override def get_name: String = name
  override def getLevel: Int = 0
  override def getExp: Float = 0.0f
  override def getStamina: Int = 0
  override def action: Move = new Move("None")
  var owner: Option[SpaceObject] = None
  def set_owner(_owner: Option[SpaceObject]): Unit = this.owner = _owner
}

case class Wall(level: Int) extends SpaceObject {
  override def get_object_type: String = "Block"
  override def get_name: String = "Wall"
  override def getLevel: Int = level
  override def getExp: Float = 0.0f
  override def getStamina: Int = 0

  val how_many_increasing: Int = if (level == 0) 1 else 5
  override def action: Move = new Move("Block", how_many_increasing)
}

case class Monster(name: String, level: Int, exp: Float,var  stamina: Int = 5) extends SpaceObject {
  val moves: List[Move] = List(
    new Move("EnergyBlast", 3),
    new Move("FlashStrike", 1),
    new Move("PhotonWave", 2)
  )
  
  override def get_name: String = name
  override def get_object_type: String = "Monster"
  override def getLevel: Int = level
  override def getExp: Float = exp
  override def getStamina: Int = stamina
  def setStamina(_stamina: Int ) :Unit = {
    this.stamina = _stamina
  }  
  override def action: Move = moves(Random.nextInt(moves.size))
}

class Player[T <: SpaceObject](val person: Option[SpaceObject], val role: String, var level: Int, var reward: List[Float]=List[Float](), var stamina: Int = 10) extends SpaceObject  ,CombatStyle
{
  
  
  val moves: List[Move] = List(
    new Move("EnergyBlast", 3),
    new Move("FlashStrike", 1),
    new Move("PhotonWave", 2)
  )
  var current_reward :Float = 0.0f
  def get_current_reward :Float = current_reward
  var memory_steps :Map[(Int,Int),Option[SpaceObject]] = Map[(Int,Int),Option[SpaceObject]]()
  def get_memory_steps:Map[(Int,Int),Option[SpaceObject]]=memory_steps
  val items : ListBuffer[Item] = ListBuffer()
  def addItem(item: Item): Unit = {
    items += item
  }
  def get_reward :List[Float] = reward
  def get_cum_reward :Float =reward.sum
  override def action: Move = moves(Random.nextInt(moves.size))
  override def getLevel: Int = level
  override def getExp: Float = reward.sum
  override def getStamina: Int = stamina
  def setLevel(level: Int) :Unit = this.level = level

  def setStamina(s: Int): Unit = {
    this.stamina = s
  }
  val agentActions = Map("up" -> (-1, 0), "down" -> (1, 0), "left" -> (0, -1), "right" -> (0, 1))

  def getAgentActions: Map[String, (Int, Int)] = agentActions


  override def get_name: String = person.map(_.get_name).getOrElse("Unknown")
  println(f"Player name is ${get_name} and role is ${role} and level is ${level}")
  override def get_object_type: String = "Player"

  def movePlayer(
  space: Array[Array[Option[SpaceObject]]], 
  x: Int, 
  y: Int, 
  prev_x: Int, 
  prev_y: Int
): Unit = {
  // Validate new coordinates
  if (x < 0 || x >= space.length || y < 0 || y >= space(0).length) {
    throw new IllegalArgumentException("Invalid coordinates")
  }
  
  // Check if the new position is empty
  if (space(x)(y).isEmpty) {
    // Move player to new position
    for {
      i <- space.indices
      j <- space(i).indices
      if space(i)(j).contains(this)
    } space(i)(j) = None
    
    memory_steps((x,y))=None
    println("Player moved to a new position. with empty space")
    space(x)(y) = Some(this)
    reward = -0.01 :: reward
    current_reward= - 0.01

  } else {
    // Encounter with an object at the new position
    val playerOption = space(prev_x)(prev_y)
    val unknownObjectOption = space(x)(y)
    
    (playerOption, unknownObjectOption) match {
      /*
      case (Some(player: Player[_]), Some(defender:Monster)) =>
        if (defender.get_object_type !="Item") {
        println("The player has encountered an enemy or another object.")
        val battle = performAttack(Some(player), Some(defender))
        if (battle) {
          space(prev_x)(prev_y) = None
          space(x)(y) = Some(this)
        } else {
          println("The player has died facing an unknown enemy.")
          //space(prev_x)(prev_y) = None
        }
        }else {
          println("The player has encountered an item.")
          pickItem (Some(player) ,Some(defender))
          for (i<- 0 until player.items.size) {
            println(s"Player picked ${player.items(i).get_name}")
          }
          space(prev_x)(prev_y) = space(x)(y)
          space(x)(y) = Some(this)
        }
      */

      
      
      case (Some(player: Player[_]), Some(defender:Wall)) =>
        
        println("Agent face  A wall ")
        reward = -1 :: reward
        current_reward = -1
      case (Some(player:Player[_]),Some(defender:Item)) =>
        println(s"Agent found an item :${defender.get_name}")
        space(prev_x)(prev_y) = None
        space(x)(y) = Some(this)
        reward = 10 :: reward
        current_reward = 10

      case (Some(_), None) =>
        println("Encountered an object but no defender found.")
      
      case (None, Some(_)) =>
        println("Player not found at previous location.")
      
      case (None, None) =>
        println("Neither player nor defender found.")
    }
  }
}
  
  
  
  def findPlayer(space: Array[Array[Option[SpaceObject]]]): (Int, Int) = {
    for {
      i <- space.indices
      j <- space(i).indices
      if space(i)(j).contains(this)
    } return (i, j)

    throw new IllegalStateException("Player not found")
  }
  def setReward() :Unit  ={
   reward =List[Float]()
  }
  def isValidMove(space: Array[Array[Option[SpaceObject]]], new_x: Int, new_y: Int) :Boolean ={
    if (new_x < 0 || new_x >= space.length || new_y < 0 || new_y >= space(0).length) {
     reward = -1.1 :: reward
     return false
    }
    return true 
  }
  def move(space: Array[Array[Option[SpaceObject]]],action:String): Unit = {
    // the previous position
    val (x, y) = findPlayer(space)
    val (deltaX, deltaY) = agentActions(action)
    val new_x = x + deltaX
    val new_y = y + deltaY
  
    if (isValidMove(space,new_x,new_y)){
     movePlayer(space, new_x, new_y,x,y)
    }else {
      println(action)
    }
    
  }

  
}
