package recfun
import common._

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
    if (c == 0 || r < 2 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    else {
      def balanceCount(x: Int, chars: List[Char]): Boolean = {
        if (x < 0) false
        else if (chars.isEmpty) x == 0
        else if (chars.head == '(') balanceCount(x + 1, chars.tail)
        else if (chars.head == ')') balanceCount(x - 1, chars.tail)
        else balanceCount(x, chars.tail)
      }
      balanceCount(0, chars)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 0
    else if (coins.isEmpty) 0
    else {
      def countSimpleDivider(count: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) count
        else {
          val h = coins.head
          val t = coins.tail
          if (money < h) 0
          else if (money % h == 0) countSimpleDivider(count + 1, t)
          else countSimpleDivider(count, t)
        }        
      }
      countSimpleDivider(0, coins.sorted)
    }
  }
}
