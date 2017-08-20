package com.example.practice

object Main {
  def main(args: Array[String]): Unit = {
    println(fib(10))
    println(isSorted[Int](Array(1, 2, 3, 4, 3), _ < _))
    println(curry[Int, Int, Int]((x, y) => x * y)(10)(5))
    val f = (x: Double) => math.Pi / 2 - x
    val g = (x: Int) => math.sin(x)
    println(compose(f, g)(180))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(k: Int, current: Int, next: Int): Int = {
      if (k == n) current
      else go(k + 1, next, current + next)
    }

    go(1, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n > as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else
        false
    }

    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
