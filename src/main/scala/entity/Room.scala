package entity

import scala.reflect.ClassTag
import scala.util.Random

class Room[T <: SpaceObject : ClassTag](width: Int, height: Int) {
  private var  space: Array[Array[Option[T]]] = Array.fill(width, height)(None)
  def get_space :Array[Array[Option[T]]] = space

  def place_object_random(obj: T): Unit = {
    val emptyCells = for {
      x <- space.indices
      y <- space(x).indices
      if space(x)(y).isEmpty
    } yield (x, y)

    if (emptyCells.nonEmpty) {
      val (x, y) = emptyCells(Random.nextInt(emptyCells.size))
      space(x)(y) = Some(obj)
    } else {
      throw new IllegalStateException("No empty cells in the room")
    }
  }

  def get_object_by_type(object_type: String): Option[T] = {
    space.flatten.collectFirst {
      case Some(obj) if obj.get_object_type == object_type => obj 
    }
  }

  def placeObject(x: Int, y: Int, obj: T): Unit = {
    if (x < 0 || x >= width || y < 0 || y >= height) {
      throw new IllegalArgumentException("Invalid coordinates")
    } else {
      space(x)(y) = Some(obj)
    }
  }

  def removeObject(obj:T): Unit = {
   for {
          i <- space.indices
          j <- space(i).indices
          if space(i)(j).contains(obj)
        } space(i)(j) = None
  }
     
  
def displaySpace(): Unit = {
  val width = space(0).length
  val height = space.length
  val cellWidth = 7 // Width of each cell, including padding
  val borderChar = "_" // Character for the top border
  val borderChar3D = "¯" // Character for the bottom border (3D effect)
  val indent = 4 // Indentation for 3D effect

  // Function to format a cell's content
  def formatCell(content: String): String = {
    String.format("[%-" + cellWidth + "s]", content)
  }

  // Top edge of the room (3D perspective effect)
  println(" " * indent + ("_" * (width * cellWidth + width + 1)))

  // Display each row with perspective effect
  for (row <- space.indices) {
    // Side edge and shadow effect
    print(" " * (indent - 1) + "|")

    // Print cells with 3D effect
    for (col <- space(row).indices) {
      val cellContent = space(row)(col) match {
        case Some(obj) => s"${obj.get_object_type.take(1)}-${obj.get_name.take(2)}"
        case None => "__"
      }
      // Print cell with padding
      print(formatCell(cellContent))
    }

    println("|")
    println(" " * indent + ("_" * (width * cellWidth + width + 1)))
  }

  // Bottom edge of the room (3D effect)
  println(" " * (indent - 1) + (borderChar3D * (width * cellWidth + width + 1)))
}



}
