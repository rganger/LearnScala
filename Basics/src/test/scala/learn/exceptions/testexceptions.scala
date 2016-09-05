package learn.exceptions

import org.scalatest._

import scala.util.{Failure, Success, Try}

/**
  * Created by Ganger on 9/4/2016.
  */
class TestExceptions extends FunSpec with Matchers {

  describe("When creating my Option type") {
    it("should create a Some if the data in non-null") {
      Option[Int](5) should not be None
      Option[String](null) equals None
      Option[String]("abc") shouldBe a [Some[String]]
      Option[String]("bcd").getOrElse("xyz") equals "bcd"
      Option[String](null).getOrElse("xyz") equals "xyz"
      val test = Option[Double](Math.sqrt(-5.0))
      println(s"Test: ${test}")
      test should not be None
      val test1 = Option[Double](Math.sqrt(5.0))
      println(s"Test1: ${test1}")
      test1 shouldBe a [Some[String]]

      val data = (x: Double) => x / 0.0
      println(s"Data: ${data(1)}")
      Option[Double](data(1)) shouldBe Some(Double.PositiveInfinity)
    }
  }

  describe ("When executing 'for' comprehensions") {
    it("should check for errors and return either valid results or exceptions") {
      //val quotient: (Double, Double) = (n: Double, d: Double) => n / d
      //def quotient (n: Double, d: Double): Either[Exception, Double] = {
      //  Try (n / d) match {
      //    case Success(v) => Right(v)
      //    case Failure(e) => Left(new Exception(e))
      //  }
      //}
      def quotient (n: Double, d: Double): Double =  (n / d)

      def result (top: String, bottom: String): Either[Exception, Double] = {
        (for {
          n <- Try(top.toDouble)
          d <- Try(bottom.toDouble)
        } yield quotient(n, d)) match {
          case Success(r) => Right(r)
          case Failure(e) => Left(new Exception(e))
        }
      }

      result("2.0", "3.0").fold(
        error => {println(s"Error: $error"); error},
        r => {println(s"Result: $r"); r}
      ) should not be an [Exception]

      result("2.0", "0.0").fold(
        error => {println(s"Error: $error"); error},
        r => {println(s"Result: $r"); r}
      ) shouldBe Double.PositiveInfinity

      result("---", "1.0").fold(
        error => {println(s"Error: $error"); error},
        r => {println(s"Result: $r"); r}
      ) shouldBe an [Exception]

      result("0.0", "---").fold(
        error => {println(s"Error: $error"); error},
        r => {println(s"Result: $r"); r}
      ) shouldBe an [Exception]
    }
  }
}
