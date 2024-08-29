package  scala
import entity.SpaceObject as SpaceObject
import entity.Room as Room
import entity.{Person, Item,Player,Wall,Monster}
def mapToPlayer(obj: Option[SpaceObject]): Option[Player[_ <: SpaceObject]] = {
  obj match {
    case Some(player: Player[_]) => Some(player)
    case _ => None
  }
}
@main def hello(): Unit = {
  val space = new Room[SpaceObject](5, 5)
  
  // Place various objects in the space
  space.place_object_random(Person("Alice"))
  space.place_object_random(Item("key"))
  space.place_object_random(Item("sword"))
  space.place_object_random(Wall(0))
  space.place_object_random(Monster("goblin", 0, 10))
  
  // Create and place a Player object
  space.place_object_random(Player(space.get_object_by_type("Person"), "Warrior", 2))
  
  // Remove a specific Person object
  space.removeObject(Person("Alice"))
  
  // Find the Player object in the space
  val spaceObject: Option[SpaceObject] = space.get_object_by_type("Player")
  val playerOption: Option[Player[_ <: SpaceObject]] = mapToPlayer(spaceObject)
  var player: Option[Player[_ <: SpaceObject]] = playerOption

  println("Welcome to the game!")
  space.displaySpace()
  var action: Int = 0
  while (action != 5) {
    println("1. Move up \n2. Move down \n3. Move right \n4. Move left \n5. Exit")

    action = scala.io.StdIn.readInt()
    
    player match {
      case Some(p) =>
        action match {
          case 1 => 
            p.moveUp(space.get_space)
            space.displaySpace()
            println("\u001b[H\u001b[2J")
          case 2 => 
            p.moveDown(space.get_space)
            space.displaySpace()
            println("\u001b[H\u001b[2J")
          case 3 => 
            p.moveRight(space.get_space)
            space.displaySpace()
            println("\u001b[H\u001b[2J")
          case 4 => 
            p.moveLeft(space.get_space)
            space.displaySpace()
            println("\u001b[H\u001b[2J")
          case 5 => 
            println("Exiting the game.")
            sys.exit()
          case _ => 
            println("Invalid action. Please choose a number between 1 and 5.")
        }
      case None =>
        println("Player not found. Cannot perform actions.")
    }
  }
}

  

