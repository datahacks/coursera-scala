package recfun

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
      if (c == 0 || c == r) 1 
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], num: Int ): Boolean = {
        if (chars.isEmpty) num == 0
        else if (chars.head == '(') loop(chars.tail, num + 1)
        else if (chars.head == ')') num>0 && loop(chars.tail, num - 1)
        else loop(chars.tail, num)
      }
      loop(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val coins_sorted = coins.sorted
      if (coins_sorted.isEmpty) 0
      else if (money - coins_sorted.head == 0) 1
      else if (money - coins_sorted.head < 0) 0
      else countChange(money - coins_sorted.head, coins_sorted) + countChange(money, coins_sorted.tail)
    }
  }
