package recfun

object Main {
  def main(args: Array[String]) {


    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println()
    val test1 = "(if (zero? x) max (/ 1 x))".toList
    println(balance(test1))
    val test2 = "I told him (that it’s not (yet) done). (But he wasn’t listening)".toList
    println(balance(test2))
    val test3 = "())(".toList
    println(balance(test3))
    val test4 = ":-)".toList
    println(balance(test4))

    println()
    println(countChange(4,List(1, 2)))
    println(countChange(301,List(500,5,50,100,20,200,10)))
    println(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if(c == 0 || r == c) 1 else pascal(c, r-1) + pascal(c-1, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def is_balanced(chars: List[Char], p_open: Int): Boolean = {
        if(chars.isEmpty) p_open == 0
        else {
          val first = chars.head
          val verif = first match {
            case '(' => p_open + 1
            case ')' => p_open - 1
            case _ => p_open
          }
          if(verif >= 0) is_balanced(chars.tail, verif)
          else false
        }
      }

      is_balanced(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =  {
      def count(m: Int, c: List[Int]) : Int = {
        if(c.isEmpty) 0
        else if(m - c.head == 0) 1
        else if(m - c.head < 0) 0
        else countChange(m-c.head, c) + countChange(m, c.tail)
      }

      count(money, coins.sorted)
    }
  }
