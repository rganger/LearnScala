package learn.structures

import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable

/**
  * Created by Ganger on 9/5/2016.
  */
class TestStructures extends FunSpec with Matchers {

  describe("Testing an immutable list fold") {
    it("should apply the binary opertaor") {
      val l = immutable.List(3,1,7,8,3,6,5)
      val l1 = l.fold(1) {(x, y) => (x * y)/y}
      println(l1)
      val l2 = l.foldLeft(1){(x, y) => (x * y)/y}
      println(l2)
      val l3 = l.foldRight(1){(x, y) => (x * y)/y}
      println(l3)
    }
  }


}
