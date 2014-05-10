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
    
    println (balance("(".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if( c == r || c == 0 )  1
    else pascal( c-1 , r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def find(t: List[Char], numOpens: Int): Boolean ={
		if( t.isEmpty ) {
		  numOpens == 0
		}else {
			val h = t.head
			val n =
				if( h == '(' ) numOpens + 1
				else if( h == ')' ) numOpens - 1
				else numOpens
				
			if( numOpens >= 0 ) find(t.tail, n)
			else false
		}
    }
    
    find(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	  if(money == 0) 1
	  else if (money < 0) 0
	  else if (coins.isEmpty) 0
	  else {
	    countChange(money - coins.head, coins) +
	    countChange(money, coins.tail)
	  }
	}
}
