package recfun

object Main {
  def main(args: Array[String]) {
    balance("())(".toList)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if(c==0 || r==c  )
        1
      else
        pascal(c-1,r-1)+pascal(c,r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def countParantheses(chars: List[Char],counter:Int): Int={
      if(counter<0)
        counter
      else if(chars.isEmpty)
        counter
      else
        if(chars.head == '(')
          countParantheses(chars.tail,counter+1)

      else
        if(chars.head == ')')
          countParantheses(chars.tail,counter-1)
      else
        countParantheses(chars.tail,counter)
    }
    (countParantheses(chars,0)==0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeRecursion(moneyLeft: Int, coinsList: List[Int], counter: Int): Int = {
      if (moneyLeft < 0)
        counter
      else if (coinsList.isEmpty) {
        if (moneyLeft == 0)
          1 + counter
        else
          counter
      }
      else
        countChangeRecursion(moneyLeft, coinsList.tail, counter) +
          countChangeRecursion(moneyLeft - coinsList.head, coinsList, counter)
    }
        countChangeRecursion(money, coins, 0)
    }
  }
