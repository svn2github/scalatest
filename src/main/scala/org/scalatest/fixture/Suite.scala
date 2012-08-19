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
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>fixture.Suite</code> in situations for which <a href="../Suite.html"><code>Suite</code></a>
 * would be a good choice, when all or most tests need the same fixture objects
 * that must be cleaned up afterwords. <em>Note: <code>fixture.Suite</code> is intended for use in special situations, with trait <code>Suite</code> used for general needs. For
 * more insight into where <code>fixture.Suite</code> fits in the big picture, see the <a href="../Suite.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../Suite.html#sharedFixtures">Shared fixtures</a> section in the documentation for trait <code>Suite</code>.</em>
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
 * This trait's <code>runTest</code> method delegates the actual running of each test to <code>withFixture(OneArgTest)</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture(OneArgTest)</code> method (abstract in this trait) is responsible
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
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this trait. One good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * case class F(file: File, writer: FileWriter)
 * type FixtureParam = F
 * </pre>
 *
 * <p>
 * To enable the stacking of traits that define <code>withFixture(NoArgTest)</code>, it is a good idea to let
 * <code>withFixture(NoArgTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgTest</code> to a <code>NoArgTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgTest</code> method of <code>OneArgTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withFixture(NoArgTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre>
 * withFixture(test.toNoArgTest(theFixture))
 * </pre>
 *
 * <p>
 * Here's a complete example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.oneargtest
 * 
 * import org.scalatest.fixture
 * import java.io._
 * 
 * class ExampleSuite extends fixture.Suite {
 * 
 *   case class F(file: File, writer: FileWriter)
 *   type FixtureParam = F
 * 
 *   def withFixture(test: OneArgTest) {
 *
 *     // create the fixture
 *     val file = File.createTempFile("hello", "world")
 *     val writer = new FileWriter(file)
 *     val theFixture = F(file, writer)
 *
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
 *     }
 *     finally writer.close() // clean up the fixture
 *   }
 *
 *   def &#96;test: testing should be easy&#96; (f: F) {
 *     f.writer.write("easy!")
 *     f.writer.flush()
 *     assert(f.file.length === 18)
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; (f: F) {
 *     f.writer.write("fun!")
 *     f.writer.flush()
 *     assert(f.file.length === 17)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If a test fails, the <code>OneArgTest</code> function will complete abruptly with an exception describing the failure.
 * To ensure clean up happens even if a test fails, you should invoke the test function from inside a <code>try</code> block and do the cleanup in a
 * <code>finally</code> clause, as shown in the previous example.
 * </p>
 *
 * <a name="sharingFixturesAcrossClasses"></a><h2>Sharing fixtures across classes</h2>
 *
 * <p>
 * If multiple test classes need the same fixture, you can define the <code>FixtureParam</code> and <code>withFixture(OneArgTest)</code> implementations
 * in a trait, then mix that trait into the test classes that need it. For example, if your application requires a database and your integration tests
 * use that database, you will likely have many test classes that need a database fixture. You can create a "database fixture" trait that creates a
 * database with a unique name, passes the connector into the test, then removes the database once the test completes. This is shown in the following example:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.suite.sharing
 * 
 * import java.util.concurrent.ConcurrentHashMap
 * import org.scalatest.fixture
 * import DbServer._
 * import java.util.UUID.randomUUID
 * 
 * object DbServer { // Simulating a database server
 *   type Db = StringBuffer
 *   private val databases = new ConcurrentHashMap[String, Db]
 *   def createDb(name: String): Db = {
 *     val db = new StringBuffer
 *     databases.put(name, db)
 *     db
 *   }
 *   def removeDb(name: String) {
 *     databases.remove(name)
 *   }
 * }
 * 
 * trait DbFixture { this: fixture.Suite =&gt;
 * 
 *   type FixtureParam = Db
 * 
 *   // Allow clients to populate the database after
 *   // it is created
 *   def populateDb(db: Db) {}
 * 
 *   def withFixture(test: OneArgTest) {
 *     val dbName = randomUUID.toString
 *     val db = createDb(dbName) // create the fixture
 *     try {
 *       populateDb(db) // setup the fixture
 *       withFixture(test.toNoArgTest(db)) // "loan" the fixture to the test
 *     }
 *     finally removeDb(dbName) // clean up the fixture
 *   }
 * }
 * 
 * class ExampleSuite extends fixture.Suite with DbFixture {
 * 
 *   override def populateDb(db: Db) { // setup the fixture
 *     db.append("ScalaTest is ")
 *   }
 * 
 *   def &#96;test: testing should be easy&#96; (db: Db) {
 *       db.append("easy!")
 *       assert(db.toString === "ScalaTest is easy!")
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; (db: Db) {
 *       db.append("fun!")
 *       assert(db.toString === "ScalaTest is fun!")
 *   }
 * 
 *   // This test doesn't need a Db
 *   def &#96;test: test code should be clear&#96; {
 *       val buf = new StringBuffer
 *       buf.append("ScalaTest code is ")
 *       buf.append("clear!")
 *       assert(buf.toString === "ScalaTest code is clear!")
 *   }
 * }
 * </pre>
 *
 * <p>
 * Often when you create fixtures in a trait like <code>DbFixture</code>, you'll still need to enable individual test classes
 * to "setup" a newly created fixture before it gets passed into the tests. A good way to accomplish this is to pass the newly
 * created fixture into a setup method, like <code>populateDb</code> in the previous example, before passing it to the test
 * function. Classes that need to perform such setup can override the method, as does <code>ExampleSuite</code>.
 * </p>
 *
 * <p>
 * If a test doesn't need the fixture, you can indicate that by leaving off the fixture parameter, as is done in the
 * third test in the previous example, &ldquo;<code>test: test code should be clear</code>&rdquo;. For such methods, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
 * </p>
 *
 * <p>
 * Both examples shown above demonstrate the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in these examples. This keeps tests completely isolated, allowing you to run them in parallel if desired. You could mix
 * <code>ParallelTestExecution</code> into either of these <code>ExampleSuite</code> classes, and the tests would run in parallel just fine.
 * </p>
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
  protected trait OneArgTest extends (FixtureParam => Unit) with TestData { thisOneArgTest =>

    /**
     * Run the test, using the passed <code>FixtureParam</code>.
     */
    def apply(fixture: FixtureParam)

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
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, payload, 2, location, isConstructingThread, true)
      )

    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest, 
        (message, _, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, None, 2, location, isConstructingThread, true) // TODO: Need a test that fails because testWasCanceleed isn't being passed
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
      case e: Throwable => throw e
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
