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
    def factorial(n: Int): Int = {
      def inner(acc: Int, n: Int): Int = n match {
        case 0 => acc
        case n => inner(acc * n, n - 1)
      }
      inner(1, n)
    }
    else factorial(r) / (factorial(c) * factorial(r - c))
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def value(char: Char): Int = char match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }
    def inner(count: Int, chars: List[Char]): Boolean = chars match{
      case Nil => (count == 0)
      case head::tail =>
        count + value(head) match {
          case c if c < 0 => false
          case c =>
            inner(c, tail)
        }
    }
    inner(0, chars)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 ) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
