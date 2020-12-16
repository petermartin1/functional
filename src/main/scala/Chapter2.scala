import scala.annotation.tailrec

object Chapter2 {
  def abs(n: Int): Int =
    if(n < 0) -n
    else n
  private def formatAbs(n: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(n, abs(n))
  }
  def factorial(n: Int): Int ={
    def go(index: Int, acc: Int): Int = {
      if(index <= 0) acc
      else go(index-1,acc*index)
    }
    go(n,1)
  }
  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }
  def fib(n: Int): Int = {
    @tailrec
    def go(index: Int, acc1: Int, acc2: Int): Int = {
      if(index == n) acc1 + acc2
      else go(index +1, acc2, acc1 + acc2)
    }
    if(n < 0) 0
    else if(n == 1) 0
    else if(n == 2) 1
    else go(3,0,1)
  }
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(i: Int): Int = {
      if(i >= as.length) -1
      else if(p(as(i))) i
      else loop(i+1)
    }
    loop(0)
  }
  def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if(i >= as.length - 1)true
      else if(!p(as(i), as(i+1))) false
      else loop(i+1)
    }
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 8, factorial))
  }
}
