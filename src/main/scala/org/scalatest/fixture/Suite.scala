/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.fixture

import org.scalatest._
import collection.immutable.TreeSet
import Suite._
import java.lang.reflect.{InvocationTargetException, Method, Modifier}
import org.scalatest.events._
import org.scalatest.Suite._
import exceptions.{TestCanceledException, TestPendingException}

/**
 * <code>Suite</code> that can pass a fixture object into its tests.
 *
 * <p>
 * Note: <code>fixture.Suite</code> is intended for use in special situations, with trait <code>Suite</code> used for general needs. For
 * more insight into where <code>fixture.Suite</code> fits in the big picture, see the <a href="../Suite.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../Suite.html#sharedFixtures">Shared fixtures</a> section in the documentation for trait <code>Suite</code>.
 * </p>
 * 
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>fixture.Suite</code> in situations for which <a href="../Suite.html"><code>Suite</code></a>
 * would be a good choice, such as large projects or static code generation, when all or most tests need the same fixture objects
 * that must be cleaned up afterwords.
 * </td></tr></table>
 * 
 * <p>
 * Trait <code>fixture.Suite</code> behaves similarly to trait <code>org.scalatest.Suite</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is declared as a member of this trait.
 * This trait also declares an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this trait.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This trait's <code>runTest</code> method delegates the actual running of each test to <code>withFixture</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture</code> method (abstract in this trait) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this trait must, therefore, do three things differently from a plain old <code>org.scalatest.Suite</code>:
 * </p>
 * 
 * <ol>
 * <li>define the type of the fixture parameter by specifying type <code>FixtureParam</code></li>
 * <li>define the <code>withFixture(OneArgTest)</code> method</li>
 * <li>write tests that take a fixture parameter</li>
 * <li>(You can also define tests that don't take a fixture parameter.)</li>
 * </ol>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import collection.mutable.Stack
 * import java.util.NoSuchElementException
 *
 * class StackSuite extends fixture.Suite {
 *
 *   // 1. define type FixtureParam
 *   type FixtureParam = Stack[Int]
 *
 *   // 2. define the withFixture method
 *   def withFixture(test: OneArgTest) {
 *     val stack = new Stack[Int]
 *     stack.push(1)
 *     stack.push(2)
 *     test(stack) // "loan" the fixture to the test
 *   }
 *
 *   // 3. write tests that take a fixture parameter
 *   def testPopAValue(stack: Stack[Int]) {
 *     val top = stack.pop()
 *     assert(top === 2)
 *     assert(stack.size === 1)
 *   }
 *
 *   def testPushAValue(stack: Stack[Int]) {
 *     stack.push(9)
 *     assert(stack.size === 3)
 *     assert(stack.head === 9)
 *   }
 *
 *   // 4. You can also write tests that don't take a fixture parameter.
 *   def testPopAnEmptyStack() {
 *     intercept[NoSuchElementException] {
 *       (new Stack[Int]).pop()
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In the previous example, <code>withFixture</code> creates and initializes a stack, then invokes the test function, passing in
 * the stack.  In addition to setting up a fixture before a test, the <code>withFixture</code> method also allows you to
 * clean it up afterwards, if necessary. If you need to do some clean up that must happen even if a test
 * fails, you should invoke the test function from inside a <code>try</code> block and do the cleanup in a
 * <code>finally</code> clause, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: OneArgTest) {
 *   val resource = someResource.open() // set up the fixture
 *   try {
 *     test(resource) // if the test fails, test(...) will throw an exception
 *   }
 *   finally {
 *     // clean up the fixture no matter whether the test succeeds or fails
 *     resource.close()
 *   }
 * }
 * </pre>
 *
 * <p>
 * The reason you must perform cleanup in a <code>finally</code> clause is that <code>withFixture</code> is called by
 * <code>runTest</code>, which expects an exception to be thrown to indicate a failed test. Thus when you invoke
 * the <code>test</code> function, it may complete abruptly with an exception. The <code>finally</code> clause will
 * ensure the fixture cleanup happens as that exception propagates back up the call stack to <code>runTest</code>.
 * </p>
 *
 * <p>
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this trait. One good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import scala.collection.mutable.ListBuffer
 *
 * class ExampleSuite extends fixture.Suite {
 *
 *   case class F(builder: StringBuilder, buffer: ListBuffer[String])
 *   type FixtureParam = F
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     // Create needed mutable objects
 *     val stringBuilder = new StringBuilder("ScalaTest is ")
 *     val listBuffer = new ListBuffer[String]
 *
 *     // Invoke the test function, passing in the mutable objects
 *     test(F(stringBuilder, listBuffer))
 *   }
 *
 *   def testEasy(f: F) {
 *     f.builder.append("easy!")
 *     assert(f.builder.toString === "ScalaTest is easy!")
 *     assert(f.buffer.isEmpty)
 *     f.buffer += "sweet"
 *   }
 *
 *   def testFun(f: F) {
 *     f.builder.append("fun!")
 *     assert(f.builder.toString === "ScalaTest is fun!")
 *     assert(f.buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait Suite extends org.scalatest.Suite { thisSuite =>

  /**
   * The type of the fixture parameter that can be passed into tests in this suite.
   */
  protected type FixtureParam

  /**
   * Trait whose instances encapsulate a test function that takes a fixture and config map.
   *
   * <p>
   * The <code>fixture.Suite</code> trait's implementation of <code>runTest</code> passes instances of this trait
   * to <code>fixture.Suite</code>'s <code>withFixture</code> method, such as:
   * </p>
   *
   * <pre class="stHighlight">
   * def testSomething(fixture: Fixture) {
   *   // ...
   * }
   * def testSomethingElse(fixture: Fixture, info: Informer) {
   *   // ...
   * }
   * </pre>
   *
   * <p>
   * For more detail and examples, see the
   * <a href="Suite.html">documentation for trait <code>fixture.Suite</code></a>.
   * </p>
   */
  protected trait OneArgTest extends (FixtureParam => Unit) { thisOneArgTest =>

    /**
     * The name of this test.
     */
    def name: String

    /**
     * Run the test, using the passed <code>FixtureParam</code>.
     */
    def apply(fixture: FixtureParam)

    /**
     * Return a <code>Map[String, Any]</code> containing objects that can be used
     * to configure the fixture and test.
     */
    def configMap: Map[String, Any]

    /**
     * Convert this <code>OneArgTest</code> to a <code>NoArgTest</code> whose
     * <code>name</code> and <code>configMap</code> methods return the same values
     * as this <code>OneArgTest</code>, and whose <code>apply</code> method invokes
     * this <code>OneArgTest</code>'s apply method,
     * passing in the given <code>fixture</code>.
     *
     * <p>
     * This method makes it easier to invoke the <code>withFixture</code> method
     * that takes a <code>NoArgTest</code>. For example, if a <code>fixture.Suite</code> 
     * mixes in <code>SeveredStackTraces</code>, it will inherit an implementation
     * of <code>withFixture(NoArgTest)</code> provided by
     * <code>SeveredStackTraces</code> that implements the stack trace severing
     * behavior. If the <code>fixture.Suite</code> does not delegate to that
     * <code>withFixture(NoArgTest)</code> method, the stack trace severing behavior
     * will not happen. Here's how that might look in a <code>fixture.Suite</code>
     * whose <code>FixtureParam</code> is <code>StringBuilder</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * def withFixture(test: OneArgTest) {
     *   withFixture(test.toNoArgTest(new StringBuilder))
     * }
     * </pre>
     */
    def toNoArgTest(fixture: FixtureParam) =
      new NoArgTest {
        val name = thisOneArgTest.name
        def configMap = thisOneArgTest.configMap
        def apply() { thisOneArgTest(fixture) }
      }
  }

  /**
   *  Run the passed test function with a fixture created by this method.
   *
   * <p>
   * This method should create the fixture object needed by the tests of the
   * current suite, invoke the test function (passing in the fixture object),
   * and if needed, perform any clean up needed after the test completes.
   * For more detail and examples, see the <a href="Suite.html">main documentation for this trait</a>.
   * </p>
   *
   * @param fun the <code>OneArgTest</code> to invoke, passing in a fixture
   */
  protected def withFixture(test: OneArgTest)

  private[fixture] class TestFunAndConfigMap(val name: String, test: FixtureParam => Any, val configMap: Map[String, Any])
    extends OneArgTest {
    
    def apply(fixture: FixtureParam) {
      test(fixture)
    }
  }

  private[fixture] class FixturelessTestFunAndConfigMap(override val name: String, test: () => Any, override val configMap: Map[String, Any])
    extends NoArgTest {

    def apply() { test() }
  }

  // TODO: add documentation here, so people know they can pass an Informer as the second arg.
  override def testNames: Set[String] = {

    def takesTwoParamsOfTypesAnyAndInformer(m: Method) = {
      val paramTypes = m.getParameterTypes
      val hasTwoParams = paramTypes.length == 2
      hasTwoParams && classOf[Informer].isAssignableFrom(paramTypes(1))
    }

    def takesOneParamOfAnyType(m: Method) = m.getParameterTypes.length == 1

    def isTestMethod(m: Method) = {

      // Factored out to share code with Suite.testNames
      val (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags) = isTestMethodGoodies(m)

      // Also, will discover both
      // testNames(Object) and testNames(Object, Informer). Reason is if I didn't discover these
      // it would likely just be silently ignored, and that might waste users' time
      isInstanceMethod && (firstFour == "test") && ((hasNoParams && !isTestNames && !isTestTags) ||
          takesInformer(m) || takesOneParamOfAnyType(m) || takesTwoParamsOfTypesAnyAndInformer(m))
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m)) yield
        if (takesInformer(m))
          m.getName + InformerInParens
        else if (takesOneParamOfAnyType(m))
          m.getName + FixtureInParens
        else if (takesTwoParamsOfTypesAnyAndInformer(m))
          m.getName + FixtureAndInformerInParens
        else m.getName

    TreeSet[String]() ++ testNameArray
  }

  protected override def runTest(testName: String, args: Args) {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val (stopRequested, report, method, testStartTime) =
      getSuiteRunTestGoodies(stopper, reporter, testName)

    reportTestStarting(thisSuite, report, tracker, testName, testName, getDecodedName(testName), thisSuite.rerunner, Some(getTopOfMethod(testName)))

    val formatter = getEscapedIndentedTextForTest(testName, 1, true)

    val messageRecorderForThisTest = new MessageRecorder(report)
    val informerForThisTest =
      MessageRecordingInformer(
        messageRecorderForThisTest, 
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, payload, 2, location, isConstructingThread, true, Some(testWasPending), Some(testWasCanceled))
      )

    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest, 
        (message, _, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, None, 2, location, isConstructingThread, true, Some(testWasPending)) // TODO: Need a test that fails because testWasCanceleed isn't being passed
      )

// TODO: Use a message recorder in FixtureSuite. Maybe just allow the state and
// use Engine in Suite, though then I'd have two Engines in everything. Or even three down here.
// Nah, go ahead and use message recording informer here, and maybe find some other way to
// reduce the duplication between Suite, FixtureSuite, and Engine.
    try {
      if (testMethodTakesAFixtureAndInformer(testName) || testMethodTakesAFixture(testName)) {
        val testFun: FixtureParam => Unit = {
          (fixture: FixtureParam) => {
            val anyRefFixture: AnyRef = fixture.asInstanceOf[AnyRef] // TODO zap this cast
            val args: Array[Object] =
              if (testMethodTakesAFixtureAndInformer(testName)) {
                Array(anyRefFixture, informerForThisTest)
              }
              else
                Array(anyRefFixture)

            method.invoke(thisSuite, args: _*)
          }
        }
        withFixture(new TestFunAndConfigMap(testName, testFun, configMap))
      }
      else { // Test method does not take a fixture
        val testFun: () => Unit = {
          () => {
            val args: Array[Object] =
              if (testMethodTakesAnInformer(testName)) 
                Array(informerForThisTest)
              else
                Array()

            method.invoke(this, args: _*)
          }
        }
        withFixture(new FixturelessTestFunAndConfigMap(testName, testFun, configMap))
      }

      val duration = System.currentTimeMillis - testStartTime
      reportTestSucceeded(thisSuite, report, tracker, testName, testName, getDecodedName(testName), messageRecorderForThisTest.recordedEvents(false, false), duration, formatter, thisSuite.rerunner, Some(getTopOfMethod(method)))
    }
    catch { 
      case ite: InvocationTargetException =>
        val t = ite.getTargetException
        t match {
          case _: TestPendingException =>
            val duration = System.currentTimeMillis - testStartTime
            // testWasPending = true so info's printed out in the finally clause show up yellow
            reportTestPending(thisSuite, report, tracker, testName, testName, getDecodedName(testName), messageRecorderForThisTest.recordedEvents(true, false), duration, formatter, Some(getTopOfMethod(method)))
          case e: TestCanceledException =>
            val duration = System.currentTimeMillis - testStartTime
            val message = getMessageForException(e)
            val formatter = getEscapedIndentedTextForTest(testName, 1, true)
            // testWasCanceled = true so info's printed out in the finally clause show up yellow
            report(TestCanceled(tracker.nextOrdinal(), message, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), thisSuite.decodedSuiteName, testName, testName, getDecodedName(testName), messageRecorderForThisTest.recordedEvents(false, true), Some(e), Some(duration), Some(formatter), Some(getTopOfMethod(method)), thisSuite.rerunner))
          case e if !anErrorThatShouldCauseAnAbort(e) =>
            val duration = System.currentTimeMillis - testStartTime
            handleFailedTest(t, testName, messageRecorderForThisTest.recordedEvents(false, false), report, tracker, getEscapedIndentedTextForTest(testName, 1, true), duration)
          case e => throw e
        }
      case e if !anErrorThatShouldCauseAnAbort(e) =>
        val duration = System.currentTimeMillis - testStartTime
        handleFailedTest(e, testName, messageRecorderForThisTest.recordedEvents(false, false), report, tracker, getEscapedIndentedTextForTest(testName, 1, true), duration)
      case e => throw e
    }
  }

  // Overriding this in fixture.Suite to reduce duplication of tags method
  private[scalatest] override def getMethodForTestName(testName: String) = {
    val candidateMethods = getClass.getMethods.filter(_.getName == Suite.simpleNameForTest(testName))
    val found =
      if (testMethodTakesAFixtureAndInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 2 && paramTypes(1) == classOf[Informer]
          }
        )
      else if (testMethodTakesAnInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1 && paramTypes(0) == classOf[Informer]
          }
        )
      else if (testMethodTakesAFixture(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1
          }
        )
      else
        candidateMethods.find(_.getParameterTypes.length == 0)

     found match {
       case Some(method) => method
       case None =>
         throw new IllegalArgumentException(Resources("testNotFound", testName))
     }
  }
  
  /**
   * Suite style name.
   */
  override val styleName: String = "org.scalatest.fixture.Suite"
}

private object Suite {

  val FixtureAndInformerInParens = "(FixtureParam, Informer)"
  val FixtureInParens = "(FixtureParam)"

  private def testMethodTakesAFixtureAndInformer(testName: String) = testName.endsWith(FixtureAndInformerInParens)
  private def testMethodTakesAFixture(testName: String) = testName.endsWith(FixtureInParens)

  private def simpleNameForTest(testName: String) =
    if (testName.endsWith(FixtureAndInformerInParens))
      testName.substring(0, testName.length - FixtureAndInformerInParens.length)
    else if (testName.endsWith(FixtureInParens))
      testName.substring(0, testName.length - FixtureInParens.length)
    else if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName

  private def argsArrayForTestName(testName: String): Array[Class[_]] =
    if (testMethodTakesAFixtureAndInformer(testName))
      Array(classOf[Object], classOf[Informer])
    else
      Array(classOf[Informer])
}
