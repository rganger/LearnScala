package learn.basics

import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by ganger on 8/23/2016.
  */
object Chapter2 {
  val logger = LoggerFactory.getLogger(Chapter2.getClass)

  class SortTest {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      if (as.size == 0 ) {
        true
      } else if (as.tail.size > 0 && ordered(as.head, as.tail.head)) {
        true
      } else {
        isSorted(as.tail, ordered)
      }
    }
  }

  val orderFuncInt: (Int, Int) => Boolean = (f:Int, s:Int) =>  {f <= s}
  val orderFuncRInt: (Int, Int) => Boolean = (f:Int, s:Int) => {s <= f}
  val orderFuncStr: (String, String) => Boolean = (s1:String, s2:String) => s1.compareTo(s2) > 0

  val sortTest = new SortTest

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def main(args: Array[String]): Unit = {
    // Test sorting
    val arr1 = Array[Int](1,2,3,4,5)
    val arr2 = Array[Int](5,4,3,2,1)
    val arr3 = Array[Int](1,3,2,4,5)
    val arr4 = Array[String]("aaa", "aab", "aac")
    val arr5 = Array[String]("aba", "aaa", "aac")

    logger.info(s"Arr1 is sorted with orderFuncInt: ${sortTest.isSorted[Int](arr1, orderFuncInt)}")
    logger.info(s"Arr2 is sorted with orderFuncRInt: ${sortTest.isSorted[Int](arr2, orderFuncRInt)}")
    logger.info(s"Arr3 is not sorted with orderFuncInt: ${sortTest.isSorted[Int](arr3, orderFuncInt)}")
    logger.info(s"Arr3 is not sorted with orderFuncRInt: ${sortTest.isSorted[Int](arr3, orderFuncRInt)}")
    logger.info(s"Arr4 is sorted with orderFuncStr: ${sortTest.isSorted[String](arr4, orderFuncStr)}")
    logger.info(s"Arr4 is not sorted with orderFuncStr: ${sortTest.isSorted[String](arr5, orderFuncStr)}")

    val order5 = curry[Int, Int, Boolean](orderFuncInt)(5)
    logger.info(s"Ordered test1: ${order5(6)}")
    logger.info(s"Ordered test1: ${order5(4)}")

  }

}

