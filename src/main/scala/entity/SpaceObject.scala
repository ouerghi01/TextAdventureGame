package entity

trait SpaceObject {
  def get_object_type: String
  def get_name: String
}

case class Person(name: String) extends SpaceObject {
  override def get_object_type: String = "Person"
  override def get_name: String = name
}

case class Item(name: String) extends SpaceObject {
  override def get_object_type: String = "Item"
  override def get_name: String = name
}

case class Player(person: Option[entity.SpaceObject], role: String) extends SpaceObject {
  override def get_name: String = person.map(_.get_name).getOrElse("Unknown")

  // Correctly overriding get_object_type without parentheses
  override def get_object_type: String = "Player"

  def movePlayer(space: Array[Array[Option[SpaceObject]]], x: Int, y: Int): Unit = {
    // Corrected bounds check
    if (x < 0 || x >= space.length || y < 0 || y >= space(0).length) {
      throw new IllegalArgumentException("Invalid coordinates")
    } else {
      if (space(x)(y).isEmpty) {
        // Remove player from current position
        for {
          i <- space.indices
          j <- space(i).indices
          if space(i)(j).contains(this)
        } space(i)(j) = None

        // Place player in the new position
        space(x)(y) = Some(this)
      } else {
        println("you found something!!!!!!!!")
        space(x)(y) = Some(this)
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
    // find the position of the player
    val (x, y) = findPlayer(space)
    val new_x = x + deltaX
    val new_y = y + deltaY

    if (new_x < 0 || new_x >= space.length || new_y < 0 || new_y >= space(0).length) {
    throw new IllegalArgumentException("Move out of bounds")
    }
    space(x)(y) = None
    movePlayer(space, new_x, new_y)
  }
  def moveUp(space: Array[Array[Option[SpaceObject]]]): Unit = {
   move(space,-1,0)
  }
  def moveLeft(space: Array[Array[Option[SpaceObject]]]): Unit ={
    move(space, 0, -1)
  }
  def moveRight(space: Array[Array[Option[SpaceObject]]]): Unit = {
   move(space, 0, 1)
  }
  def moveDown(space: Array[Array[Option[SpaceObject]]]): Unit = {
    move(space, 1,0)
  }

  
}
