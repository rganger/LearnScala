package learn.basics

/**
  * Created by Ganger on 8/20/2016.
  */
object Basics {

  def fib(n: Int): Int = {
    if (n == 0 || n == 1 || n == 2) {
      n
    } else {
      val first = 1
      val second = 2
      val iter = 3
      def loop(prevToLastNum: Int, lastNum: Int, iter: Int): Int = {
        val thisNum = prevToLastNum + lastNum
        if (iter == n) {
          thisNum
        } else {
          loop(lastNum, thisNum, iter + 1)
        }
      }
      loop(first, second, iter)
    }
  }

  def main(args: Array[String]) {
    for (i <- 0 to 10) {
      println(s"Fib for ${i} is ${fib(i)}")
    }
  }
}
