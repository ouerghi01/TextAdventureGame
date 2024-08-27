package entity

import scala.util.Random

trait SpaceObject {
  def get_object_type: String
  def get_name: String
  def getLevel: Int
  def getExp: Float
  def getStamina: Int
  def action: Move
}

trait CombatStyle {
 def performAttack(obj1: Option[SpaceObject], obj2: Option[SpaceObject]): Boolean = {
  // Pattern matching to extract attacker and defender
  (obj1, obj2) match {
    case (Some(attacker: Player[_]), Some(defender)) =>
      println(s"${attacker.get_name} attacks ${defender.get_name}")

      // Check if the attacker can attack the defender based on levels
      if (attacker.getLevel >= defender.getLevel) {
        // Calculate new stamina after the attack
        val newStamina = attacker.getStamina - defender.action.increment_exp

        // Update stamina if attacker is a Player
        attacker.setStamina(newStamina)

        println(s"Attacker ${attacker.get_name} has attacked defender ${defender.get_name}")
        true
      } else {
        println("Player Warning\n---------------->")
        println("You can't attack this level\n---------------->")
        false
      }

    case (Some(_: Player[_]), None) =>
      println("Defender not found.")
      false

    case (None, Some(_)) =>
      println("Attacker not found.")
      false

    case _ =>
      println("Both attacker and defender not found.")
      false
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

case class Monster(name: String, level: Int, exp: Float, stamina: Int = 5) extends SpaceObject {
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
  override def action: Move = moves(Random.nextInt(moves.size))
}

class Player[T <: SpaceObject](val person: Option[SpaceObject], val role: String, val level: Int, val reward: List[Float]=List[Float](), var stamina: Int = 10) extends SpaceObject  ,CombatStyle{
  val moves: List[Move] = List(
    new Move("EnergyBlast", 3),
    new Move("FlashStrike", 1),
    new Move("PhotonWave", 2)
  )
  
  override def action: Move = moves(Random.nextInt(moves.size))
  override def getLevel: Int = level
  override def getExp: Float = reward.sum
  override def getStamina: Int = stamina

  def setStamina(s: Int): Unit = {
    this.stamina = s
  }

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
    space(x)(y) = Some(this)
  } else {
    // Encounter with an object at the new position
    val playerOption = space(prev_x)(prev_y)
    val unknownObjectOption = space(x)(y)
    
    (playerOption, unknownObjectOption) match {
      case (Some(player: Player[_]), Some(defender)) =>
        println("The player has encountered an enemy or another object.")

        // Perform attack if the defender is an enemy
        val battle = performAttack(Some(player), Some(defender))
        if (battle) {
          // Update position if the player survived
          space(prev_x)(prev_y) = None
          space(x)(y) = Some(this)
        } else {
          // Handle player death
          println("The player has died facing an unknown enemy.")
          space(prev_x)(prev_y) = None
        }

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

  def move(space: Array[Array[Option[SpaceObject]]], deltaX: Int, deltaY: Int): Unit = {
    // the previous position
    val (x, y) = findPlayer(space)
    val new_x = x + deltaX
    val new_y = y + deltaY

    if (new_x < 0 || new_x >= space.length || new_y < 0 || new_y >= space(0).length) {
      throw new IllegalArgumentException("Move out of bounds")
    }
    movePlayer(space, new_x, new_y,x,y)
  }

  def moveUp(space: Array[Array[Option[SpaceObject]]]): Unit = {
    move(space, -1, 0)
  }

  def moveLeft(space: Array[Array[Option[SpaceObject]]]): Unit = {
    move(space, 0, -1)
  }

  def moveRight(space: Array[Array[Option[SpaceObject]]]): Unit = {
    move(space, 0, 1)
  }

  def moveDown(space: Array[Array[Option[SpaceObject]]]): Unit = {
    move(space, 1, 0)
  }
}
