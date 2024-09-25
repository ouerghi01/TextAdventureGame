package  scala
import entity.SpaceObject as SpaceObject
import scala.util.Random
import entity.Room as Room
import scala.collection.mutable.Map

import entity.{Person, Item,Player,Wall,Monster}
def mapToPlayer(obj: Option[SpaceObject]): Option[Player[_ <: SpaceObject]] = {
  obj match {
    case Some(player: Player[_]) => Some(player)
    case _ => None
  }
}
def initialize_env (space :Room[SpaceObject]):Option[Player[_ <: SpaceObject]] = {
  space.place_object_random(Person("Alice"))
  space.get_space(3)(3)=Some(Item("Exit"))
  space.get_space(1)(1)=Some(Wall(0))
  space.get_space(2)(0)=Some(Wall(0))
  space.get_space(2)(1)=Some(Wall(0))


  space.get_space(0)(0)=Some(Player(space.get_object_by_type("Person"), "Agent", 100))
  space.removeObject(Person("Alice"))
  val spaceObject: Option[SpaceObject] = space.get_object_by_type("Player")
  val playerOption: Option[Player[_ <: SpaceObject]] = mapToPlayer(spaceObject)
  var Agent: Option[Player[_ <: SpaceObject]] = playerOption
  return Agent
}
def reset_env(space :Room[SpaceObject]) :Unit = {
  space.reset_space()

}
@main def Main(): Unit = {
  
  val space = new Room[SpaceObject](4, 4)
  
  println("Welcome to the game!")
  // display the grid word 
  space.displaySpace()
  var a: Int = 0
  while (a != 5) {
    println("1. Lance The Agent to discover the grid   \n2. Exit")

    a = scala.io.StdIn.readInt()
    a match {
      case 1 =>{
        
        var observations = Map[List[Float],List[String]]()
        for (episode <- 0 until 100){
            var player: Option[Player[_ <: SpaceObject]] = initialize_env(space)
            player match {
                   case Some(p) =>
       
            var action = ""
            var reward :Float = 0
            var actions= List[String]()
           
            var done = false 
            while( done== false ){
              action =Random.shuffle(p.agentActions.keys).head
              actions= action :: actions
              p.move(space.get_space, action)
              space.displaySpace()
              reward = p.get_cum_reward
              if (p.get_reward.contains(10) ){
                
                observations(p.get_reward.reverse)=actions.reverse
                p.setReward()
                println("Agent has reached the end of the game")
                done = true 
              }
              
            }
            
            
            
           
            
            
      case None =>
        println("Player not found. Cannot perform actions.")
      }
      reset_env(space)
     
    }
    var ks =observations.keys.toList.reverse
    val max_ks = ks.map(k => k.sum).max
    ks= ks.filter(k => k.sum == max_ks )
    for (k <- ks){
      println(observations(k))
    }

    println()
    
  

    }

      case 2 => {
         println("the game dosen' t start " )
         sys.exit()
      }

    }
    
  }
}

  

