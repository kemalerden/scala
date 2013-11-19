package recfun
import common._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], open: Int): Boolean = {
      chars match {
        case '(' :: t => balance(t, open + 1)
        case ')' :: t => open > 0 && balance(t, open - 1)
        case Nil => open == 0;
        case _ :: t => balance(t, open)
      }
    }
    balance(chars, 0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recurse(m: Int, cs: List[Int]): Int ={
      if (m < 0) 0 
      else if (cs.isEmpty) {
        if (m == 0) 1 else 0 
      } else
        recurse(m, cs.tail) + recurse(m - cs.head, cs)
    }
    if (money > 0)
      recurse(money, coins) else 0
      
  }
}
