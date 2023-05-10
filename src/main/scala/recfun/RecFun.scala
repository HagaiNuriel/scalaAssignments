package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
//    if (c == 0 || c == r) 1
//    else pascal(c-1, r-1) + pascal(c, r-1)
    @tailrec
    def pascalAux(currCol: Int, currRow: Int, prevTriangleRow: Array[Int], currTriangleRow: Array[Int]): Int = {
      currTriangleRow(currCol) = (if (currCol > 0) prevTriangleRow(currCol - 1) else 0) + (if (currCol < currRow) prevTriangleRow(currCol) else 0)

      if ((currCol == c) && (currRow == r)) currTriangleRow(currCol)
      else if (currCol < currRow) pascalAux(currCol + 1, currRow, prevTriangleRow, currTriangleRow)
      else pascalAux(0, currRow + 1, currTriangleRow, new Array(_length = currRow + 2))
    }

    if ((c == 0) && (r == 0)) 1
    else pascalAux(0, 1, Array(1), Array(0, 0))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(chars: List[Char], openParentheses: Int): Boolean = {
      if (openParentheses < 0) false
      else if (chars.isEmpty) openParentheses == 0
      else {
        val c = chars.head
        val currOpenParentheses =
          if (c == '(') openParentheses + 1
          else if (c == ')') openParentheses - 1
          else openParentheses
        balanceRec(chars.tail, currOpenParentheses)
      }
    }

    balanceRec(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
