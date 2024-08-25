package  scala
import entity.SpaceObject as SpaceObject
import entity.Room as Room
import entity.{Person, Item,Player}
@main def hello(): Unit = {
        val space = new Room[SpaceObject](5,5)
        space.place_object_random(Person("Alice"))
        space.place_object_random(Item("key"))
        val player :Player = Player(space.get_object_by_type("Person"), "Warrior")
        space.removeObject(Person("Alice"))
        player.movePlayer(space.get_space,1,1)
        println("Welcome to the game!")
        space.displaySpace()
        var action:Int = 0
        while(action!=4 ){
                println("1. Move up \n2. Pick down \n3. move right \n4. move left \n5. exit ")
                action = scala.io.StdIn.readInt()
                action match
                        case  1 => {
                                player.moveUp(space = space.get_space) 
                                space.displaySpace()
                        }
                        case  2 =>{ 
                                player.moveDown(space = space.get_space)
                                space.displaySpace()
                        }
                        case  3 => {
                                player.moveRight(space = space.get_space)
                                space.displaySpace()
                        }
                        case  4 => {
                                player.moveLeft(space = space.get_space)
                                space.displaySpace()
                        }
                        case  5 => {
                                sys.exit()
                        }
                
        }

}
  

