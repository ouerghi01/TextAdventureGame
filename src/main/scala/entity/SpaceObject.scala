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
    val attacker = obj1.getOrElse(throw new IllegalArgumentException("Attacker not found"))
    val defender = obj2.getOrElse(throw new IllegalArgumentException("Defender not found"))

    println(s"${attacker.get_name} attacks ${defender.get_name}")
    
    if (attacker.getLevel >= defender.getLevel) {
      val newStamina = attacker.getStamina - defender.action.increment_exp
      attacker match {
        case p: Player => p.setStamina(newStamina)
        case _ => println("Cannot set stamina for this type")
      }
      println(s"the attacker ${attacker.get_name} has attacked the defender ${defender.get_name}\n")
      true
    } 
    else {
      println("Player Warning\n---------------->")
      println("You can't attack this level\n---------------->")
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

class Player(val person: Option[SpaceObject], val role: String, val level: Int, val reward: List[Float], var stamina: Int = 10) extends SpaceObject  ,CombatStyle{
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

  def movePlayer(space: Array[Array[Option[SpaceObject]]], x: Int, y: Int,prev_x:Int,prev_y :Int ): Unit = {
    if (x < 0 || x >= space.length || y < 0 || y >= space(0).length) {
      throw new IllegalArgumentException("Invalid coordinates")
    } else {
      if (space(x)(y).isEmpty) {
        for {
          i <- space.indices
          j <- space(i).indices
          if space(i)(j).contains(this)
        } space(i)(j) = None
        space(x)(y) = Some(this)
      } else {
        val player =space(prev_x)(prev_y).getOrElse(throw new IllegalArgumentException("Player not found"))
        val unknown_object = space(x)(y).getOrElse(throw new IllegalArgumentException("Object not found"))
        val battle=performAttack(Some(player), Some(unknown_object))
        if (battle == true) space(x)(y) = Some(this)
        else {
          println("the player died  from face unknown enemy \n")
          space(prev_x)(y) = None
        }

        
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
    space(x)(y) = None
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
