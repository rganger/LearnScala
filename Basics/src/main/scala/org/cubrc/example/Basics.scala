package org.cubrc.example

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.{LinearSeq, mutable}

/**
  * Created by ganger on 5/21/2016.
  */
class Basics {

  val log = LoggerFactory.getLogger(classOf[Basics])

  def testBasics: Unit = {
    interpolation
    expressions
    functions
    varargstest
    testInnerClass
    methodOrFunction.test
    TraitTest.testNumberList
    testApplyMethod
    testCaseClass
    ExceptionTests.testExceptions
    CollectionTests.testCollections
  }

  def interpolation {
    //s
    val str = "one"
    val ucase = (str:String) => str.toUpperCase
    log.debug(s"$str goes to ${ucase(str)}")
    //f
    log.debug(f"$str%20s is padded")
    //raw
    log.debug(raw"This would be a newline \n")
  }

  def expressions {
    //Expressions
    val i = 1
    val j = i + 1
    log.debug(s"val i is $i, and j = $j")

    //Values/vars
    var k = 5
    val l = 6
    k = 7
    log.debug(s"var k is $k, and val l = $l")
  }

  def increment(x:Int): Int = x + 1
  def doubleIncrement(x:Int): Int = increment(increment(x))

  def functions {
    val orig = 8
    log.debug(s"Increment to $orig to ${increment(orig)} to ${doubleIncrement(orig)}")

    var funcVal = (x:Int) => increment(x)
    var much = funcVal(funcVal(funcVal(funcVal(3))))
    log.debug(s"Function pointers added to $much")

    funcVal = (x:Int) => doubleIncrement(x)
    much = funcVal(funcVal(funcVal(funcVal(3))))
    log.debug(s"Function pointers added to $much")

    val power = (x:Int, y:Int) => math.pow(x,y)
    val x = 3
    val y = 3
    val z = power(x,y)
    log.debug(s"Power: $x^$y = $z")

    //curried
    val power2 = (x:Int) => power(x,2)
    val power3 = (x:Int) => power(x,3)
    log.debug(s"Power2: 5^2 = ${power2(5)}")
    log.debug(s"Power3: 5^3 = ${power3(5)}")
  }

  //Variable arguments
  def varargs(args:String*) = {
    val show = (s:String) => log.debug(s"String: $s")
    args.foreach(show)
  }

  def varargstest {
    varargs("blue","red","green")
  }

  //Inner class
  class inner(s:String) {
    val upperS = (s:String) => s.toUpperCase
    override def toString: String = return s
    def toUpperString: String = return upperS(s)
  }

  def testInnerClass {
    log.debug(new inner("blue").toString)
    log.debug(new inner("blue").toUpperString)
  }

  object methodOrFunction {
    def add(i:Int,j:Int): Int = {
      return i + j
    }

    val subtract = (i:Int, j:Int) => i - j

    def test = {
      log.debug(s"Add: ${add(5,6)}")
      log.debug(s"Subtract: ${subtract(5,6)}")
      var s = subtract
      var a = (i:Int,j:Int) => add(i,j)
      log.debug(s"Add: ${a(5,6)}")
      log.debug(s"Subtract: ${s(5,6)}")
      var temp = s
      s = a
      a = s
      log.debug(s"Add: ${s(5,6)}")
      log.debug(s"Subtract: ${a(5,6)}")
    }
  }

  object TraitTest {

    trait Sum {
      def sum: Int
    }

    trait Mean {
      def mean: Int
    }

    trait Median {
      def median: Int
    }

    class NumberList extends Sum with Mean with Median {
      var nums = new mutable.ArraySeq[Int](0)

      override def sum: Int = nums.sum

      override def mean: Int = (sum.toDouble / nums.length.toDouble).toInt

      override def median: Int = ((nums.max + nums.min).toDouble / 2).toInt

      def add(i: Int) {
        nums = nums :+ i
      }
    }

    def testNumberList {
      var nl = new NumberList
      nl add 4
      nl add 5
      nl add 6
      log debug s"Sum = ${nl sum}"
      log debug s"Mean = ${nl mean}"
      log debug s"Median = ${nl median}"
    }
  }

  //Apply methods
  class Stuff {
    def about = log debug s"This is about Stuff"
  }

  object StuffMaker {
    def apply() = new Stuff
  }

  class Stuff2(i:Int) {
    def about = log debug s"This is about Stuff2($i)"
  }

  object StuffMaker2 {
    def apply(i:Int) = new Stuff2(i)
  }

  def testApplyMethod: Unit = {
    val stuff = StuffMaker()
    stuff.about
    StuffMaker2(3).about
  }

  //Case classes
  case class WeatherConditions (desc:String)

  def checkWeather(w:WeatherConditions) = w match {
      case WeatherConditions("Sunny") => "Beautiful"
      case WeatherConditions("Cloudy") => "SoSo"
      case WeatherConditions("Rainy") => "Lousy"
      case WeatherConditions(someCondition) => "Unknown: " + someCondition
      case _ => "Invalid"
  }

  def testCaseClass: Unit = {
    log debug s"Weather is ${checkWeather(WeatherConditions("Sunny"))}"
    log debug s"Weather is ${checkWeather(WeatherConditions("Cloudy"))}"
    log debug s"Weather is ${checkWeather(WeatherConditions("Rainy"))}"
    log debug s"Weather is ${checkWeather(WeatherConditions("Snowy"))}"
  }

  object ExceptionTests {
    class FirstException extends Exception
    class SecondException extends Exception

    @throws(classOf[FirstException])
    @throws(classOf[SecondException])
    def myMethod(i:Int): Unit = {
      if (i == 1) {
        log debug s"Throwing first exception"
        throw new FirstException
      } else if (i == 2) {
        log debug s"Throwing second exception"
        throw new SecondException
      } else {
        log debug s"Throwing runtime exception"
        throw new RuntimeException
      }
    }

    def testExceptions: Unit = {
      var vals = List(1,2,5)
      for (i <- vals) {
        try {
          myMethod(i)
        } catch {
          case e: FirstException => log debug s"Caught first exception"
          case e: SecondException => log debug s"Caught second exception"
          case _: Throwable => log debug s"Caught unknown exception"
        } finally {
          log debug s"After catch"
        }
      }
    }
  }

  object CollectionTests {
    //Lists
    val numList = List(2,4,6,8)
    val strList = List("a","d","z")
    //Sets
    val mySet = Set(1,2,3,"a","b","c")
    //Tuples
    val myTuple = ("a",5)
    val yourTuple = "a"->5
    //Maps
    var myMap = Map[Int,String]()
    for(num <- numList) {myMap += (num->num.toString)}

    //map over a list
    def testMap {
      var otherList = List(1, 2, 3, 4, 5)
      otherList = otherList.map(i => i * i)
      log debug s"testMap: $otherList"
    }

    //forEach over a list
    def testForEach: Unit = {
      var otherList = List(1, 2, 3, 4, 5)
      otherList.foreach((i:Int) => log debug s"forEach ${i}")
    }

    //filter a list
    def testFilter: Unit = {
      var otherList = List(1, 2, 3, 4, 5)
      otherList = otherList.filter((i:Int) => (i/2)*2 == i)
      log debug s"Filtered list: $otherList"
    }

    def testPartition: Unit = {
      var otherList = List(1,2,3,4,5,6,7,8)
      val evenList = otherList.partition((i:Int) => i%2==0)
      val oddList = otherList.partition((i:Int) => i%2!=0)
      log debug s"Partitioned evens: $evenList"
      log debug s"Partitioned odds: $oddList"
    }

    def testCollections: Unit = {
      log debug s"numList: $numList"
      log debug s"strList: $strList"
      log debug s"mySet: $mySet"
      log debug s"myTuple: $myTuple"
      log debug s"yourTuple: $yourTuple"
      log debug s"myMap: $myMap"

      log debug s"myMap has 2: ${!myMap.get(2).isEmpty}"
      log debug s"myMap has 3: ${!myMap.get(3).isEmpty}"

      testMap
      testForEach
      testFilter
      testPartition
    }

  }
}


