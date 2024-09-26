package  scala
import entity.SpaceObject as SpaceObject
import scala.util.Random
import entity.Room as Room
import scala.collection.mutable.Map
import scala.concurrent.duration._
import entity.{Person, Item,Player,Wall,Monster}
import scala.collection.mutable.{Map => MutableMap}

// Implementation of Q-Learning 



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
  // Initialize Q-table
  val qTable: MutableMap[(Int, Int), MutableMap[String, Float]] = MutableMap()
  val actions = List("up", "down", "left", "right")
  val alpha = 0.1f // Learning rate
  val gamma = 0.9f // Discount factor
  val epsilon = 0.1f // Exploration rate
  // Initialize Q-table with zeros for all state-action pairs
  for (i <- 0 until 4; j <- 0 until 4) {
  qTable((i, j)) = MutableMap(
    "up" -> 0.0f,
    "down" -> 0.0f,
    "left" -> 0.0f,
    "right" -> 0.0f
  )
  }
  def chooseAction(state: (Int, Int),actions:List[String]): String = {
  if (Random.nextFloat() < epsilon) {
    Random.shuffle(actions).head // Explore
  } else {
    // Exploit: choose the action with the highest Q-value for the current state
    val actionValues = qTable(state)
    actionValues.maxBy(_._2)._1
  }
  }
  def updateQValue(state: (Int, Int), action: String, reward: Float, newState: (Int, Int)): Unit = {
    val oldQValue = qTable(state)(action)
    val futureQValue = qTable(newState).values.max
    val newQValue = oldQValue + alpha * (reward + gamma * futureQValue - oldQValue)
    qTable(state)(action) = newQValue
  }
  
  println("Welcome to the game!")

  var a: Int = 0
  while (a != 5) {
    println("1. Lance The Agent to discover the grid   \n2. Exit")

    a = scala.io.StdIn.readInt()
    a match {
      case 1 =>{
       
        //Step 2: Set Hyperparameters
        val learning_rate = 0.8
        val discount_factor = 0.95
        val exploration_prob = 0.2
        val epochs = 100
        for (episode <- 0 until epochs){
            var player: Option[Player[_ <: SpaceObject]] = initialize_env(space)
            player match {
                   case Some(p) =>
       
            var action = ""
            var reward :Float = 0.0f
           
            var done = false 
            while( done== false ){
              val state= p.findPlayer(space.get_space)
              action =chooseAction(state,p.agentActions.keys.toList)
              p.move(space.get_space, action)
              reward = p.get_current_reward
              val new_state=p.findPlayer(space.get_space)
              updateQValue(state, action, reward, new_state)
              print("\u001b[2J")
              space.displaySpace()
              Thread.sleep(500)
              reward = p.get_cum_reward
              if (p.get_reward.contains(10) ){
                
                
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
    
    

    
  

    }

      case 2 => {
         println("the game dosen' t start " )
         sys.exit()
      }

    }
    
  }
}

  

