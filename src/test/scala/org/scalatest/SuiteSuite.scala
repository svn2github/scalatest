/*
 * Copyright 2001-2011 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import scala.collection.immutable.TreeSet
import org.scalatest.events._
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import java.awt.AWTError

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestFailedException
*/

class SuiteSuite extends Suite with PrivateMethodTester with SharedHelpers with SeveredStackTraces {

  def `test: Suite should discover method names and tags using deprecated Informer form` {

    val a = new Suite {
      def testNames(info: Informer): Unit = ()
    }
    assert(a.expectedTestCount(Filter()) === 1)
    val tnResult: Set[String] = a.testNames
    val gResult: Map[String, Set[String]] = a.tags
    assert(tnResult.size === 1)
    assert(gResult.keySet.size === 0)
  }

  // TODO: ensure suite aborts if same name is used with and without communicator
  def `test: Suite should discover method names and tags` {

    val a = new Suite {
      def testNames(r: Rep): Unit = ()
    }
    assert(a.expectedTestCount(Filter()) === 1)
    val tnResult: Set[String] = a.testNames
    val gResult: Map[String, Set[String]] = a.tags
    assert(tnResult.size === 1)
    assert(gResult.keySet.size === 0)
  }

  def `test: test methods with no tags should not show up in tags map` {
    
    val a = new Suite {
      def `test: not tagged` = ()
    }
    assert(a.tags.keySet.size === 0)
  }

  def `test: test methods that return non-Unit should be discovered using deprecated Informer form` {
    val a = new Suite {
      def testThis(): Int = 1
      def testThat(info: Informer): String = "hi"
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }

  def `test: test methods that return non-Unit should be discovered` {
    val a = new Suite {
      def `test: this`: Int = 1
      def `test: that`(r: Rep): String = "hi"
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }

  def `test: overloaded test methods should be discovered using deprecated Informer form` {
    val a = new Suite {
      def testThis() = ()
      def testThis(info: Informer) = ()
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }

  def `test: overloaded test methods should cause an aborted suite` {
    val a = new Suite {
      def `test: this` = ()
      def `test: this`(r: Rep) = ()
    }
    intercept[NotAllowedException] {
      a.expectedTestCount(Filter())
    }
    intercept[NotAllowedException] {
      a.testNames
    }
    intercept[NotAllowedException] {
        a.tags
    }
  }

  def testThatInterceptCatchesSubtypes() {
    class MyException extends RuntimeException
    class MyExceptionSubClass extends MyException
    intercept[MyException] {
      throw new MyException
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    intercept[MyException] {
      throw new MyExceptionSubClass
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    // Try with a trait
    trait MyTrait {
      def someRandomMethod() {}
    }
    class AnotherException extends RuntimeException with MyTrait
    val caught = intercept[MyTrait] {
      throw new AnotherException
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    // Make sure the result type is the type passed in, so I can 
    // not cast and still invoke any method on it I want
    caught.someRandomMethod()
  }

  def testThatInterceptReturnsTheCaughtException() {
    val e = new RuntimeException
    val result = intercept[RuntimeException] {
      throw e
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    assert(result eq e)
  }

  def testStripDollars() {
    expectResult("MySuite") {
     Suite.stripDollars("line8$object$$iw$$iw$$iw$$iw$$iw$MySuite")
    }
    expectResult("MySuite") {
     Suite.stripDollars("MySuite")
    }
    expectResult("nested.MySuite") {
     Suite.stripDollars("nested.MySuite")
    }
    expectResult("$$$") {
     Suite.stripDollars("$$$") 
    }
    expectResult("DollarAtEnd") {
     Suite.stripDollars("DollarAtEnd$") 
    }
    expectResult("DollarAtEnd") {
     Suite.stripDollars("line8$object$$iw$$iw$$iw$$iw$$iw$DollarAtEnd$")
    }
    expectResult("MySuite$1") {
     Suite.stripDollars("MySuite$1")
    }
    expectResult("ExampleSuite") {
      Suite.stripDollars("$read$$iw$$iw$$iw$$iw$ExampleSuite")
    }
    expectResult("Fred") {
      Suite.stripDollars("$line19.$read$$iw$$iw$Fred$")
    }
  }
  
  def testDiffStrings() {
    expectResult(("[]", "[a]")) { Suite.diffStrings("", "a") }
    expectResult(("[a]", "[]")) { Suite.diffStrings("a", "") }
    expectResult(("a[]", "a[b]")) { Suite.diffStrings("a", "ab") }
    expectResult(("a[b]", "a[]")) { Suite.diffStrings("ab", "a") }
    expectResult(("[a]", "[b]")) { Suite.diffStrings("a", "b") }
    expectResult(("[a]big", "[]big")) { Suite.diffStrings("abig", "big") }
    expectResult(("[]big", "[a]big")) { Suite.diffStrings("big", "abig") }
    expectResult(("big[a]", "big[]")) { Suite.diffStrings("biga", "big") }
    expectResult(("big[]", "big[a]")) { Suite.diffStrings("big", "biga") }
    expectResult(("small[a]big", "small[]big")) { Suite.diffStrings("smallabig", "smallbig") }
    expectResult(("0123456789[]0123456789", "0123456789[a]0123456789")) {
      Suite.diffStrings("01234567890123456789", "0123456789a0123456789")
    }
    expectResult(("...01234567890123456789[]0123456789", "...01234567890123456789[a]0123456789")) {
      Suite.diffStrings("X012345678901234567890123456789", "X01234567890123456789a0123456789")
    }
    expectResult(("01234567890123456789[]01234567890123456789...", "01234567890123456789[a]01234567890123456789...")) {
        Suite.diffStrings("0123456789012345678901234567890123456789X", "01234567890123456789a01234567890123456789X")
    }
    expectResult(("...01234567890123456789[]01234567890123456789...", "...01234567890123456789[a]01234567890123456789...")) {
        Suite.diffStrings("XXXX0123456789012345678901234567890123456789XX", "XXXX01234567890123456789a01234567890123456789XX")
    }
    expectResult(("...01234567890123456789[]01234567890123456789...", "...01234567890123456789[a]01234567890123456789...")) {
        Suite.diffStrings("X0123456789012345678901234567890123456789X", "X01234567890123456789a01234567890123456789X")
    }
  }

  def testDecorateToStringValue() {

    val decorateToStringValue = PrivateMethod[String]('decorateToStringValue)

    expectResult("1") {
      FailureMessages invokePrivate decorateToStringValue(1.toByte)
    }
    expectResult("1") {
      FailureMessages invokePrivate decorateToStringValue(1.toShort)
    }
    expectResult("1") {
      FailureMessages invokePrivate decorateToStringValue(1)
    }
    expectResult("10") {
      FailureMessages invokePrivate decorateToStringValue(10L)
    }
    expectResult("1.0") {
      FailureMessages invokePrivate decorateToStringValue(1.0f)
    }
    expectResult("1.0") {
      FailureMessages invokePrivate decorateToStringValue(1.0)
    }
    expectResult("false") {
      FailureMessages invokePrivate decorateToStringValue(false)
    }
    expectResult("true") {
      FailureMessages invokePrivate decorateToStringValue(true)
    }
    expectResult("<(), the Unit value>") {
      FailureMessages invokePrivate decorateToStringValue(())
    }
    expectResult("\"Howdy!\"") {
      FailureMessages invokePrivate decorateToStringValue("Howdy!")
    }
    expectResult("'c'") {
      FailureMessages invokePrivate decorateToStringValue('c')
    }
    expectResult("Hey!") {
      FailureMessages invokePrivate decorateToStringValue(new AnyRef { override def toString = "Hey!"})
    }
  }

  def testTestDurations() {

    class MySuite extends Suite {
      def testSucceeds() = ()
      def testFails() { fail() }
    }

    val mySuite = new MySuite
    val myReporter = new TestDurationReporter
    mySuite.run(None, Args(myReporter, Stopper.default, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myReporter.testSucceededWasFiredAndHadADuration)
    assert(myReporter.testFailedWasFiredAndHadADuration)
  }

  def testSuiteDurations() {

    // the suite duration is sent by runNestedSuites, so MySuite needs a
    // nested suite
    class MySuite extends Suite {
      override def nestedSuites = Vector(new Suite {})
      def testSucceeds() = ()
      def testFails() { fail() }
    }

    val mySuite = new MySuite
    val myReporter = new SuiteDurationReporter
    mySuite.run(None, Args(myReporter, Stopper.default, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myReporter.suiteCompletedWasFiredAndHadADuration)

    class SuiteThatAborts extends Suite {
      override def run(testName: Option[String], args: Args) {
        throw new RuntimeException("Aborting for testing purposes")
      }
    }

    // the suite duration is sent by runNestedSuites, so MySuite needs a
    // nested suite
    class MyOtherSuite extends Suite {
      override def nestedSuites = Vector(new SuiteThatAborts)
      def testSucceeds() = ()
      def testFails() { fail() }
    }

    val myOtherSuite = new MyOtherSuite
    val myOtherReporter = new SuiteDurationReporter
    myOtherSuite.run(None, Args(myOtherReporter, Stopper.default, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myOtherReporter.suiteAbortedWasFiredAndHadADuration)
  }

  def testPending() {

    class MySuite extends Suite {
      def testPending() { pending }
    }

    val mySuite = new MySuite
    val myReporter = new PendingReporter
    mySuite.run(None, Args(myReporter, Stopper.default, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
    assert(myReporter.testPendingWasFired)
  }

  def testPrettifyArray() {

    import FailureMessages.prettifyArrays

    // non arrays print just a toString
    assert(prettifyArrays(1) === "1")
    assert(prettifyArrays("hi") === "hi")
    assert(prettifyArrays(List(1, 2, 3)) === "List(1, 2, 3)")
    assert(prettifyArrays(Map("one" -> 1)) === "Map(one -> 1)")

    // arrays print pretty
    assert(prettifyArrays(Array(1, 2)) === "Array(1, 2)")

    // arrays of arrays print pretty
    assert(prettifyArrays(Array(Array(1, 2), Array(3, 4))) === "Array(Array(1, 2), Array(3, 4))")
  }

  class TestWasCalledSuite extends Suite {
    var theTestThisCalled = false
    var theTestThatCalled = false
    var theTestTheOtherCalled = false
    var theTestThisConfigMapWasEmpty = true
    var theTestThatConfigMapWasEmpty = true
    var theTestTheOtherConfigMapWasEmpty = true
    override def withFixture(test: NoArgTest) {
      if (test.configMap.size > 0)
        test.name match {
          case "testThis" => theTestThisConfigMapWasEmpty = false
          case "testThat" => theTestThatConfigMapWasEmpty = false
          case "testTheOther" => theTestTheOtherConfigMapWasEmpty = false
          case _ => throw new Exception("Should never happen")
        }
      test()
    }
    def testThis() { theTestThisCalled = true }
    def testThat() { theTestThatCalled = true }
    def testTheOther() { theTestTheOtherCalled = true }
  }

  def testExecute() {

    val s1 = new TestWasCalledSuite
    s1.execute()
    assert(s1.theTestThisCalled)
    assert(s1.theTestThatCalled)
    assert(s1.theTestTheOtherCalled)
    assert(s1.theTestThisConfigMapWasEmpty)
    assert(s1.theTestThatConfigMapWasEmpty)
    assert(s1.theTestTheOtherConfigMapWasEmpty)

    val s2 = new TestWasCalledSuite
    s2.execute("testThis")
    assert(s2.theTestThisCalled)
    assert(!s2.theTestThatCalled)
    assert(!s2.theTestTheOtherCalled)
    assert(s2.theTestThisConfigMapWasEmpty)
    assert(s2.theTestThatConfigMapWasEmpty)
    assert(s2.theTestTheOtherConfigMapWasEmpty)

    val s3 = new TestWasCalledSuite
    s3.execute(configMap = Map("s" -> "s"))
    assert(s3.theTestThisCalled)
    assert(s3.theTestThatCalled)
    assert(s3.theTestTheOtherCalled)
    assert(!s3.theTestThisConfigMapWasEmpty)
    assert(!s3.theTestThatConfigMapWasEmpty)
    assert(!s3.theTestTheOtherConfigMapWasEmpty)

    val s4 = new TestWasCalledSuite
    s4.execute("testThis", Map("s" -> "s"))
    assert(s4.theTestThisCalled)
    assert(!s4.theTestThatCalled)
    assert(!s4.theTestTheOtherCalled)
    assert(!s4.theTestThisConfigMapWasEmpty)
    assert(s4.theTestThatConfigMapWasEmpty)
    assert(s4.theTestTheOtherConfigMapWasEmpty)

    val s5 = new TestWasCalledSuite
    s5.execute(testName = "testThis")
    assert(s5.theTestThisCalled)
    assert(!s5.theTestThatCalled)
    assert(!s5.theTestTheOtherCalled)
    assert(s5.theTestThisConfigMapWasEmpty)
    assert(s5.theTestThatConfigMapWasEmpty)
    assert(s5.theTestTheOtherConfigMapWasEmpty)

    val s6 = new TestWasCalledSuite
    s6.execute(testName = "testThis", configMap = Map("s" -> "s"))
    assert(s6.theTestThisCalled)
    assert(!s6.theTestThatCalled)
    assert(!s6.theTestTheOtherCalled)
    assert(!s6.theTestThisConfigMapWasEmpty)
    assert(s6.theTestThatConfigMapWasEmpty)
    assert(s6.theTestTheOtherConfigMapWasEmpty)
  }

  def `test: execute should use dynamic tagging to enable Doenitz wildcards for non-encoded test names` {
    val s1 = new TestWasCalledSuite
    s1.execute("Th")
    assert(s1.theTestThisCalled)
    assert(s1.theTestThatCalled)
    assert(s1.theTestTheOtherCalled)
    assert(s1.theTestThisConfigMapWasEmpty)
    assert(s1.theTestThatConfigMapWasEmpty)
    assert(s1.theTestTheOtherConfigMapWasEmpty)

    val s2 = new TestWasCalledSuite
    s2.execute("This")
    assert(s2.theTestThisCalled)
    assert(!s2.theTestThatCalled)
    assert(!s2.theTestTheOtherCalled)
    assert(s2.theTestThisConfigMapWasEmpty)
    assert(s2.theTestThatConfigMapWasEmpty)
    assert(s2.theTestTheOtherConfigMapWasEmpty)

    val s3 = new TestWasCalledSuite
    s3.execute("Th", configMap = Map("s" -> "s"))
    assert(s3.theTestThisCalled)
    assert(s3.theTestThatCalled)
    assert(s3.theTestTheOtherCalled)
    assert(!s3.theTestThisConfigMapWasEmpty)
    assert(!s3.theTestThatConfigMapWasEmpty)
    assert(!s3.theTestTheOtherConfigMapWasEmpty)

    val s4 = new TestWasCalledSuite
    s4.execute("Th", Map("s" -> "s"))
    assert(s4.theTestThisCalled)
    assert(s4.theTestThatCalled)
    assert(s4.theTestTheOtherCalled)
    assert(!s4.theTestThisConfigMapWasEmpty)
    assert(!s4.theTestThatConfigMapWasEmpty)
    assert(!s4.theTestTheOtherConfigMapWasEmpty)

    val s5 = new TestWasCalledSuite
    s5.execute(testName = "Th")
    assert(s5.theTestThisCalled)
    assert(s5.theTestThatCalled)
    assert(s5.theTestTheOtherCalled)
    assert(s5.theTestThisConfigMapWasEmpty)
    assert(s5.theTestThatConfigMapWasEmpty)
    assert(s5.theTestTheOtherConfigMapWasEmpty)

    val s6 = new TestWasCalledSuite
    s6.execute(testName = "This", configMap = Map("s" -> "s"))
    assert(s6.theTestThisCalled)
    assert(!s6.theTestThatCalled)
    assert(!s6.theTestTheOtherCalled)
    assert(!s6.theTestThisConfigMapWasEmpty)
    assert(s6.theTestThatConfigMapWasEmpty)
    assert(s6.theTestTheOtherConfigMapWasEmpty)
  }

  def `test: execute should use dynamic tagging to enable Doenitz wildcards for encoded test names` {

    class TestWasCalledSuite extends Suite {
      var theTestThisCalled = false
      var theTestThatCalled = false
      var theTestTheOtherCalled = false
      var theTestThisConfigMapWasEmpty = true
      var theTestThatConfigMapWasEmpty = true
      var theTestTheOtherConfigMapWasEmpty = true
      override def withFixture(test: NoArgTest) {
        if (test.configMap.size > 0)
          test.name match {
            case "test$u0020this" => theTestThisConfigMapWasEmpty = false
            case "test$u0020that" => theTestThatConfigMapWasEmpty = false
            case "test$u0020the$u0020other" => theTestTheOtherConfigMapWasEmpty = false
            case _ => throw new Exception("Should never happen")
          }
        test()
      }
      def `test this` { theTestThisCalled = true }
      def `test that` { theTestThatCalled = true }
      def `test the other` { theTestTheOtherCalled = true }
    }

    val s1 = new TestWasCalledSuite
    s1.execute(" th")
    assert(s1.theTestThisCalled)
    assert(s1.theTestThatCalled)
    assert(s1.theTestTheOtherCalled)
    assert(s1.theTestThisConfigMapWasEmpty)
    assert(s1.theTestThatConfigMapWasEmpty)
    assert(s1.theTestTheOtherConfigMapWasEmpty)

    val s2 = new TestWasCalledSuite
    s2.execute(" this")
    assert(s2.theTestThisCalled)
    assert(!s2.theTestThatCalled)
    assert(!s2.theTestTheOtherCalled)
    assert(s2.theTestThisConfigMapWasEmpty)
    assert(s2.theTestThatConfigMapWasEmpty)
    assert(s2.theTestTheOtherConfigMapWasEmpty)

    val s3 = new TestWasCalledSuite
    s3.execute(" th", configMap = Map("s" -> "s"))
    assert(s3.theTestThisCalled)
    assert(s3.theTestThatCalled)
    assert(s3.theTestTheOtherCalled)
    assert(!s3.theTestThisConfigMapWasEmpty)
    assert(!s3.theTestThatConfigMapWasEmpty)
    assert(!s3.theTestTheOtherConfigMapWasEmpty)

    val s4 = new TestWasCalledSuite
    s4.execute(" th", Map("s" -> "s"))
    assert(s4.theTestThisCalled)
    assert(s4.theTestThatCalled)
    assert(s4.theTestTheOtherCalled)
    assert(!s4.theTestThisConfigMapWasEmpty)
    assert(!s4.theTestThatConfigMapWasEmpty)
    assert(!s4.theTestTheOtherConfigMapWasEmpty)

    val s5 = new TestWasCalledSuite
    s5.execute(testName = " th")
    assert(s5.theTestThisCalled)
    assert(s5.theTestThatCalled)
    assert(s5.theTestTheOtherCalled)
    assert(s5.theTestThisConfigMapWasEmpty)
    assert(s5.theTestThatConfigMapWasEmpty)
    assert(s5.theTestTheOtherConfigMapWasEmpty)

    val s6 = new TestWasCalledSuite
    s6.execute(testName = " this", configMap = Map("s" -> "s"))
    assert(s6.theTestThisCalled)
    assert(!s6.theTestThatCalled)
    assert(!s6.theTestTheOtherCalled)
    assert(!s6.theTestThisConfigMapWasEmpty)
    assert(s6.theTestThatConfigMapWasEmpty)
    assert(s6.theTestTheOtherConfigMapWasEmpty)
  }

  def `test: Suite should order encoded names in alphabetical decoded order` {

    // + comes before -
    // but $plus comes after $minus
    class ASuite extends Suite {

      def `test: the + operator should add` {
        val sum = 1 + 1
        assert(sum === 2)
      }

      def `test: the - operator should subtract` {
        val diff = 4 - 1
        assert(diff === 3)
      }
    }

    val a = new ASuite
    val expectedTestNames = List("" +
      "test$colon$u0020the$u0020$plus$u0020operator$u0020should$u0020add",
      "test$colon$u0020the$u0020$minus$u0020operator$u0020should$u0020subtract"
    )
    assert(a.testNames.iterator.toList === expectedTestNames)
  }

  def testDecodedSuiteName() {
    expectResult("My Test") { new My$u0020Test().decodedSuiteName.get }
    expectResult(None) { new SuiteSuite().decodedSuiteName }
  }
  
  def testDecodedTestName() {
    
    class NormalSuite extends Suite {
      def testSucceed() = {}
      def testFail() = { fail }
      def testPending() = { pending }
      @Ignore
      def testIgnore() = {}
    }
    
    val normalSuite = new NormalSuite
    val normalReporter = new EventRecordingReporter
    normalSuite.run(None, Args(normalReporter, Stopper.default, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
    val normalEventList:List[Event] = normalReporter.eventsReceived
    expectResult(7) { normalEventList.size }
    normalEventList.foreach {event =>
      event match {
        case testStarting:TestStarting => 
          expectResult(None) { testStarting.decodedTestName }
          expectResult(None) { testStarting.decodedSuiteName }
        case testSucceed:TestSucceeded => 
          expectResult("testSucceed") { testSucceed.testName }
          expectResult(None) { testSucceed.decodedTestName }
        case testFail:TestFailed =>
          expectResult("testFail") { testFail.testName }
          expectResult(None) { testFail.decodedTestName }
        case testPending:TestPending =>
          expectResult("testPending") { testPending.testName }
          expectResult(None) { testPending.decodedTestName }
        case testIgnore:TestIgnored => 
          expectResult("testIgnore") { testIgnore.testName }
          expectResult(None) { testIgnore.decodedTestName }
        case _ =>
      }
    }
    
    class DecodedSuite extends Suite {
      def `test Succeed`() {}
      def `test Fail`() = { fail }
      def `test Pending`() = { pending }
      @Ignore
      def `test Ignore`() = {}
    }
    
    val decodedSuite = new DecodedSuite
    val decodedReporter = new EventRecordingReporter
    decodedSuite.run(None, Args(decodedReporter, Stopper.default, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
    val decodedEventList:List[Event] = decodedReporter.eventsReceived
    expectResult(7) { decodedEventList.size }
    decodedEventList.foreach {event =>
      event match {
        case testStarting:TestStarting => 
          testStarting.decodedTestName match {
            case Some(name) => assert(name.length() > 0, "decodedTestName should not be empty.")
            case None => fail("decodedTestName should not be empty.")
          }
          expectResult(None) { testStarting.decodedSuiteName }
        case testSucceed:TestSucceeded => 
          expectResult("test$u0020Succeed") { testSucceed.testName }
          expectResult(Some("test Succeed")) { testSucceed.decodedTestName }
        case testFail:TestFailed =>
          expectResult("test$u0020Fail") { testFail.testName }
          expectResult(Some("test Fail")) { testFail.decodedTestName }
        case testPending:TestPending =>
          expectResult("test$u0020Pending") { testPending.testName }
          expectResult(Some("test Pending")) { testPending.decodedTestName }
        case testIgnore:TestIgnored => 
          expectResult("test$u0020Ignore") { testIgnore.testName }
          expectResult(Some("test Ignore")) { testIgnore.decodedTestName }
        case _ =>
      }
    }
  }
  
  def testTestTags() {
    class TagSuite extends Suite {  
      def testNoTagMethod() {}
      @SlowAsMolasses
      def testTagMethod() {}
    }
    val testTags = new TagSuite().tags
    assert(testTags.size === 1)
    val tagSet = testTags.getOrElse("testTagMethod", null)
    assert(tagSet != null)
    assert(tagSet.size === 1)
    assert(tagSet.toList(0) === classOf[SlowAsMolasses].getName)
  }
  
  def testRunNestedSuite() {
    
    class NoTagSuite extends Suite
    @Ignore
    class IgnoreSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @SlowAsMolasses
    class SlowAsMolassesSuite extends Suite
    @FastAsLight
    class FastAsLightSuite extends Suite
    
    class MasterSuite extends Suite {
      override def nestedSuites = Vector(new NoTagSuite(), new IgnoreSuite(), new SlowAsMolassesSuite(), new FastAsLightSuite())
      override def runNestedSuites(args: Args) {
        super.runNestedSuites(args)
      }
    }
    
    class CounterDistributor extends Distributor {
      var count = 0
      def apply(suite: Suite, args: Args) {
        count += 1
      }
      def apply(suite: Suite, tracker: Tracker) {
        count += 1
      }
    }
    
    val masterSuite = new MasterSuite()
    
    val defaultFilter = new Filter(None, Set.empty)
    val defaultReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(Args(defaultReporter, Stopper.default, defaultFilter, Map.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(defaultReporter.suiteStartingEventsReceived.size === 4)
    assert(defaultReporter.testIgnoredEventsReceived.size === 3)
    val defaultReporterDist = new EventRecordingReporter
    val defaultDistributor = new CounterDistributor
    masterSuite.runNestedSuites(Args(defaultReporterDist, Stopper.default, defaultFilter, Map.empty, Some(defaultDistributor), new Tracker(new Ordinal(99)), Set.empty))
    assert(defaultDistributor.count === 4)
    
    val includeFilter = new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)
    val includeReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(Args(includeReporter, Stopper.default, includeFilter, Map.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(includeReporter.suiteStartingEventsReceived.size === 4) 
    assert(includeReporter.testIgnoredEventsReceived.size === 0) 
    val includeReporterDist = new EventRecordingReporter
    val includeDistributor = new CounterDistributor
    masterSuite.runNestedSuites(Args(includeReporterDist, Stopper.default, includeFilter, Map.empty, Some(includeDistributor), new Tracker(new Ordinal(99)), Set.empty))
    assert(includeDistributor.count === 4) 
    
    val excludeFilter = new Filter(None, Set("org.scalatest.SlowAsMolasses"))
    val excludeReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(Args(excludeReporter, Stopper.default, excludeFilter, Map.empty, None, new Tracker(new Ordinal(99)), Set.empty))
    assert(excludeReporter.suiteStartingEventsReceived.size === 4)
    assert(excludeReporter.testIgnoredEventsReceived.size === 3)
    val excludeReporterDist = new EventRecordingReporter
    val excludeDistributor = new CounterDistributor
    masterSuite.runNestedSuites(Args(excludeReporterDist, Stopper.default, excludeFilter, Map.empty, Some(excludeDistributor), new Tracker(new Ordinal(99)), Set.empty))
    assert(excludeDistributor.count === 4)
  }
  
  def testExpectedTestCount() {
    class NoTagSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @Ignore
    class IgnoreSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @SlowAsMolasses
    class SlowAsMolassesSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @FastAsLight
    class FastAsLightSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    
    class MasterSuite extends Suite {
      override def nestedSuites = Vector(new NoTagSuite(), new IgnoreSuite(), new SlowAsMolassesSuite(), new FastAsLightSuite())
      override def runNestedSuites(args: Args) {
        super.runNestedSuites(args)
      }
    }
    
    val masterSuite = new MasterSuite()
    assert(masterSuite.expectedTestCount(new Filter(None, Set.empty)) === 9)
    assert(masterSuite.expectedTestCount(new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)) === 3)
    assert(masterSuite.expectedTestCount(new Filter(None, Set("org.scalatest.FastAsLight"))) === 6)
    assert(masterSuite.expectedTestCount(new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set.empty)) === 3)
    assert(masterSuite.expectedTestCount(new Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 6)
  }
  
  def testSuiteRunner() {
    assert(new NormalSuite().rerunner.get === classOf[NormalSuite].getName)
    assert(new WrappedSuite(Map.empty).rerunner.get === classOf[WrappedSuite].getName)
    assert(new NotAccessibleSuite("test").rerunner === None)
  }
  
  def testCheckChosenStyles() {
    class SimpleSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    
    val simpleSuite = new SimpleSuite()
    simpleSuite.run(None, Args(SilentReporter))
    simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), Map("org.scalatest.ChosenStyles" -> Set("org.scalatest.Suite")), None, new Tracker, Set.empty))
    val caught =
      intercept[NotAllowedException] {
        simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), Map("org.scalatest.ChosenStyles" -> Set("org.scalatest.FunSpec")), None, new Tracker, Set.empty))
      }
    import OptionValues._
    assert(caught.message.value === Resources("notTheChosenStyle", "org.scalatest.Suite", "org.scalatest.FunSpec"))
    val caught2 =
      intercept[NotAllowedException] {
        simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), Map("org.scalatest.ChosenStyles" -> Set("org.scalatest.FunSpec", "org.scalatest.FreeSpec")), None, new Tracker, Set.empty))
      }
    assert(caught2.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.Suite", Suite.makeListForHumans(Vector("org.scalatest.FunSpec", "org.scalatest.FreeSpec"))))
    val caught3 =
      intercept[NotAllowedException] {
        simpleSuite.run(None, Args(SilentReporter, Stopper.default, Filter(), Map("org.scalatest.ChosenStyles" -> Set("org.scalatest.FunSpec", "org.scalatest.FreeSpec", "org.scalatest.FlatSpec")), None, new Tracker, Set.empty))
      }
    assert(caught3.message.value === Resources("notOneOfTheChosenStyles", "org.scalatest.Suite", Suite.makeListForHumans(Vector("org.scalatest.FunSpec", "org.scalatest.FreeSpec", "org.scalatest.FlatSpec"))))
  }

  def testMakeListForHumans() {
    assert(Suite.makeListForHumans(Vector.empty) === "<empty list>")
    assert(Suite.makeListForHumans(Vector("")) === "\"\"")
    assert(Suite.makeListForHumans(Vector("   ")) === "\"   \"")
    assert(Suite.makeListForHumans(Vector("FunSuite FunSpec")) === "\"FunSuite FunSpec\"")
    assert(Suite.makeListForHumans(Vector("hi")) === "hi")
    assert(Suite.makeListForHumans(Vector("ho")) === "ho")
    assert(Suite.makeListForHumans(Vector("hi", "ho")) === Resources("leftAndRight", "hi", "ho"))
    assert(Suite.makeListForHumans(Vector("fee", "fie", "foe", "fum")) === "fee, fie, " + Resources("leftCommaAndRight", "foe", "fum"))
    assert(Suite.makeListForHumans(Vector("A", "stitch", "in", "time", "saves", "nine")) === "A, stitch, in, time, " + Resources("leftCommaAndRight", "saves", "nine"))
    assert(Suite.makeListForHumans(Vector("fee ", "fie", " foe", "fum")) === "\"fee \", fie, " + Resources("leftCommaAndRight", "\" foe\"", "fum"))
  }

  def testStackDepth() {
    class TestSpec extends Suite {
      def testFailure() {
        assert(1 === 2)
      }
    }
    val rep = new EventRecordingReporter
    val s1 = new TestSpec
    s1.run(None, Args(rep))
    assert(rep.testFailedEventsReceived.size === 1)
    assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "SuiteSuite.scala")
    assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 8)
  }

  def testAnErrorThatShouldCauseAnAbort() {
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new AnnotationFormatError("oops")) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new AWTError("ouch")) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new CoderMalfunctionError(new Exception)) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new FactoryConfigurationError) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new LinkageError) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new ThreadDeath) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new TransformerFactoryConfigurationError) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new VirtualMachineError {}) }
    expectResult(true) { Suite.anErrorThatShouldCauseAnAbort(new OutOfMemoryError) }
    expectResult(false) { Suite.anErrorThatShouldCauseAnAbort(new AssertionError) }
    expectResult(false) { Suite.anErrorThatShouldCauseAnAbort(new RuntimeException) }
    expectResult(false) { Suite.anErrorThatShouldCauseAnAbort(new AssertionError) }
    expectResult(false) { Suite.anErrorThatShouldCauseAnAbort(new AssertionError) }
  }
}

@DoNotDiscover
class `My Test` extends Suite {}
@DoNotDiscover
class NormalSuite extends Suite
@DoNotDiscover
@WrapWith(classOf[ConfigMapWrapperSuite]) 
class WrappedSuite(configMap: Map[_, _]) extends Suite
@DoNotDiscover
class NotAccessibleSuite(name: String) extends Suite
