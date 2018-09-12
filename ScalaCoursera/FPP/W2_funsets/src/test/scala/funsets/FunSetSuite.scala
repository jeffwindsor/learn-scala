package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect returns the set of all elements that are both in `s` or `t`") {
    new TestSets {
      val s = intersect(union(s1,s2), union(s1,s3))
      assert(contains(s, 1), "intersect 1")
      assert(!contains(s, 2), "intersect 2")
      assert(!contains(s, 3), "intersect 3")
    }
  }

  test("diff returns the set of all elements of `s` that are not in `t`") {
    new TestSets {
      val s = diff(union(s1,s2), union(s1,s3))
      assert(!contains(s, 1), "diff 1")
      assert(contains(s, 2), "diff 2")
      assert(!contains(s, 3), "diff 3")
    }
  }

  trait BoundedSets{
    val smallerSet = (elem:Int) => -100 <= elem && elem <= 100
  }
  test("For All") {
    new BoundedSets {
      //println("For All")
      val p = (elem:Int) => elem % 1 == 0
      assert(forall(smallerSet,p),"For All smaller")
    }
  }
  test("For All not met") {
    new BoundedSets {
      //println("For All not met")
      val p = (elem:Int) => elem % 10 == 0
      assert(!forall(smallerSet,p),"For All smaller")
    }
  }

  test("Exists") {
    new BoundedSets {
      //println("Exists")
      val p = (elem:Int) => elem == 100
      assert(exists(smallerSet,p),"Exists smaller")
    }
  }
  test("Exists not met") {
    new BoundedSets {
      //println("Exists not met")
      val p = (elem:Int) => elem == 1001
      assert(!exists(smallerSet,p),"")
    }
  }

  test("map") {
    new BoundedSets {
      val s = (i:Int) => i==1 || i==3 || i==4 || i==5 || i==7 || i==1000
      val f = (x:Int) => x - 1
      val r = map(s,f)
      assert(FunSets.toString(r) === "{0,2,3,4,6,999}")
    }
  }

}
