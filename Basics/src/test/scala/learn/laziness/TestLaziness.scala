package learn.laziness

import learn.laziness.lazylist.LazyList
import learn.laziness.nonlazylist.NonLazyList
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Ganger on 10/2/2016.
  */
class TestLaziness extends FunSpec with Matchers {

    describe("Creating a non-lazy list") {
      it("should result in a valid list") {
        val list = NonLazyList[Int](1,2,3,4,5,6,7,8,9)
        val list2 = list.takeWhile(x => x < 7)
        println(list2)
      }
    }

  describe("Creating a lazy list") {
    it("should result in a valid list") {
      val list = LazyList[Int](1,2,3,4,5,6,7,8,9)
      val list2 = list.takeWhile(x => x < 7)
      println(list2)
    }
  }

}
