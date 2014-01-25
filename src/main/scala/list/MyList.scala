package list

object MyList {
  def main(args: Array[String]) {
    val input = List(1, 1, 2, 3, 5, 8)
    println(length(input))
    println(reverse(input))    
  }
  
  /**
   * P05: Reverse a list
   */
  def reverse[A](input: List[A]): List[A] = input match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case x :: xs => reverse(xs) ++ List(x) //TODO - not the best way probably
  }
  

  /**
   * P04: Find the number of elements in a list
   */
  def length[A](input: List[A]): Int = input match {
    case Nil => 0
    case x::xs => 1 + length(xs)
  }
}