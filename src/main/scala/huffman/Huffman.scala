package huffman

import scala.collection.mutable.{PriorityQueue, ListBuffer}  

/**
 * Encode a list of items to their corresponding Huffman encoding tree. For example, 
 * huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
 * should produce the following list - 
 * List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
 */
object Huffman {
  def main(args: Array[String]) = {
    val result = huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    println(result)
  }

  def huffman(input: List[(String, Integer)]): List[(String, String)] = {
    val tree = buildTree(input) 
    return encoded(tree)
  }
  
  def buildTree(input: List[(String, Integer)]): Node = {
    val queue = new PriorityQueue[Node]()(Ordering.by({n: Node => n.freq}).reverse)
    input.map(x => queue.enqueue(Leaf(x._1, x._2)))
    
    val n = queue.size
    for{i <- 1 to (n-1)} {
      val x = queue.dequeue
      val y = queue.dequeue
      val newFreq = x.freq + y.freq
      queue.enqueue(Branch(x, y, newFreq))
    }

    return queue.dequeue
  }
  
  def encoded(root: Node): List[(String, String)] = {
    def emit(node: Node, prefix: String, acc: ListBuffer[(String, String)]): Unit = node match {
      case Leaf(symbol, _) =>  acc += ((symbol,  prefix))
      case Branch(l, r, f) => {
        emit(l, prefix +"0", acc)
        emit(r, prefix + "1", acc)
      }
    }
    
    val result = new ListBuffer[(String, String)]()
    emit(root, "", result)
    return result.toList
  }
    
  trait Node{    
    def freq(): Integer = this match {
      case Leaf(s, f) => f
      case Branch(l, r, f) => f
    }
  }  
  case class Leaf(symbol: String, frequency: Integer) extends Node
  case class Branch(left: Node, right: Node, frequency: Integer) extends Node      
}