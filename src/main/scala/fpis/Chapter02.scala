package fpis

import scala.annotation.tailrec

object Chapter02 {

  //  Exercise 2.1 // todo: implement tailrec version
  def fibo(n: Int): Int = {
    def go(n: Int): Int =
      if (n <= 1) 0
      else if (n == 2) 1
      else go(n - 2) + go(n - 1)

    go(n)
  }

  //  Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if ((n + 1) >= as.length) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  //  Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
  }

  //  Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //  Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatSortResult[A](m: String, as: Array[A], f: (A, A) => Boolean) = {
    val msg = "Array %s is sorted in %s order: %b"
    msg.format(as.mkString(","), m, isSorted(as, f))
  }

  def sortedAsc(i: Int, j: Int): Boolean = {
    i <= j
  }

  def sortedDec(i: Int, j: Int): Boolean = {
    i >= j
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d  is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatFibo(n: Int) = {
    val msg = "The %dth Fibonacci number is %d."
    msg.format(n, fibo(n))
  }

  private def formatResult(m: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(m, n, f(n))
  }

  //  Listing 2.4
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def main(args: Array[String]): Unit = {


  }
}
