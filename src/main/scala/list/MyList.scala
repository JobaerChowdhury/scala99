package list

object MyList {
  def main(args: Array[String]) {
    val input = List(1, 1, 2, 3, 5, 8)
    println("Input list: " + input)
    println("last(input): " + last(input))
    println("length(input): " + length(input))
    println("reverse(input): " + reverse(input))
    println("penultimate(input): " + penultimate(input))
    println(nth(2, input))
  }
  
  /**
   * P01: Find the last element of a list
   */
  def last[A](input: List[A]): A = input match {
    case x :: Nil => x
    case x :: xs => last(xs)
    case _ => throw new NoSuchElementException("Doesn't have a last element")
  }
  
  /**
   * P02: Find the last but one element of a list
   */
  def penultimate[A](input: List[A]): A = input match {
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException("Doesn't have a penultimate element")
  }
  
  /**
   * P03: Find the Kth element of a list
   */
  def nth[A](index: Int, input: List[A]): A = {
    def go(index: Int, input: List[A], counter: Int): A = {
      if(input.isEmpty || index < 0)
        throw new NoSuchElementException(s"Doesn't contain K-th element for K=$index")
      else if(index == counter) input.head
      else go(index, input.tail, (counter+1))
    }
    
    go(index, input, 0)
  }

  /**
   * P04: Find the number of elements in a list
   */
  def length[A](input: List[A]): Int = input match {
    case Nil => 0
    case x::xs => 1 + length(xs)
  }
  
  /**
   * P05: Reverse a list
   */
  def reverse[A](input: List[A]): List[A] = input match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case x :: xs => reverse(xs) ++ List(x) //TODO - not the best way probably
  }

}