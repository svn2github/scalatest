/*
 * Copyright 2001-2009 Artima, Inc.
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

/**
 * Trait that can be mixed into suites that need methods invoked before and after executing the
 * suite.
 *
 * <p>
 * This trait allows code to be executed before and/or after all the tests and nested suites of a
 * suite are run. This trait overrides <code>run</code> and calls the
 * <code>beforeAll</code> method, then calls <code>super.run</code>. After the <code>super.run</code>
 * invocation completes, whether it returns normally or completes abruptly with an exception,
 * this trait's <code>run</code> method will invoke <code>afterAll</code>.
 * </p>
 *
 * <p>
 * Trait <code>BeforeAndAfterAll</code> defines two overloaded variants each of <code>beforeAll</code>
 * and <code>afterAll</code>, one that takes a <code>configMap</code> and another that takes no
 * arguments. This trait's implemention of the variant that takes the <code>configMap</code>
 * simply invokes the variant that takes no parameters, which does nothing. Thus you can override
 * whichever variant you want. If you need something from the <code>configMap</code> before
 * all tests and nested suites are run, override <code>beforeAll(Map[String, Any])</code>. Otherwise,
 * override <code>beforeAll()</code>.
 * </p>
 *
 * <p>
 * For example, the following <code>ExampleSpec</code> mixes in <code>BeforeAndAfterAll</code> and
 * in <code>beforeAll</code>, creates and writes to a temp file, taking the name of the temp file
 * from the <code>configMap</code>. This same <code>configMap</code> is then passed to the <code>run</code>
 * methods of the nested suites, <code>OneSpec</code>, <code>TwoSpec</code>, <code>RedSpec</code>,
 * and <code>BlueSpec</code>, so those suites can access the filename and, therefore, the file's
 * contents. After all of the nested suites have executed, <code>afterAll</code> is invoked, which
 * again grabs the file name from the <code>configMap</code> and deletes the file. Each of these five
 * test classes extend trait <code>TempFileExistsSpec</code>, which defines a test that ensures the temp file exists.
 * (Note: if you're unfamiliar with the <code>withFixture(OneArgTest)</code> approach to shared fixtures, check out
 * the documentation for trait <a href="fixture/FlatSpec.html"><code>fixture.FlatSpec</code></a>.)
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.beforeandafterall
 *
 * import org.scalatest._
 * import java.io._
 * 
 * trait TempFileExistsSpec extends fixture.FlatSpec {
 * 
 *   type FixtureParam = File
 *   override def withFixture(test: OneArgTest) {
 *     val fileName = test.configMap("tempFileName").asInstanceOf[String]
 *     val file = new File(fileName)
 *     withFixture(test.toNoArgTest(file)) // loan the fixture to the test
 *   }
 * 
 *   "The temp file" should ("exist in " + suiteName) in { file =>
 *     assert(file.exists)
 *   }
 * }
 * 
 * class OneSpec extends TempFileExistsSpec
 * class TwoSpec extends TempFileExistsSpec
 * class RedSpec extends TempFileExistsSpec
 * class BlueSpec extends TempFileExistsSpec
 * 
 * class ExampleSpec extends Specs(
 *   new OneSpec,
 *   new TwoSpec,
 *   new RedSpec,
 *   new BlueSpec
 * ) with TempFileExistsSpec with BeforeAndAfterAll {
 * 
 *   private val tempFileName = "tempFileName"
 * 
 *   // Set up the temp file needed by the test, taking
 *   // a file name from the configMap
 *   override def beforeAll(configMap: Map[String, Any]) {
 * 
 *     require(
 *       configMap.isDefinedAt(tempFileName),
 *       "must place a temp file name in the configMap under the key: " + tempFileName
 *     )
 * 
 *     val fileName = configMap(tempFileName).asInstanceOf[String]
 * 
 *     val writer = new FileWriter(fileName)
 *     try {
 *       writer.write("Hello, suite of tests!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *   }
 * 
 *   // Delete the temp file
 *   override def afterAll(configMap: Map[String, Any]) {
 *     // No need to require that configMap contains the key again because it won't get
 *     // here if it didn't contain the key in beforeAll
 *     val fileName = configMap("tempFileName").asInstanceOf[String]
 *     val file = new File(fileName)
 *     file.delete()
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running the above class in the interpreter will give an error if you don't supply a mapping for <code>"tempFileName"</code> in the config map:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSpec execute
 * <span class="stGreen">ExampleSpec:</span>
 * <span class="stRed">Exception encountered when invoking run on a suite. *** ABORTED ***
 *   java.lang.IllegalArgumentException: requirement failed: must place a temp file name in the configMap under the key: tempFileName
 *   ...
 * *** RUN ABORTED ***
 *   java.lang.IllegalArgumentException: requirement failed: must place a temp file name in the configMap under the key: tempFileName
 *   ...</span>
 * </pre>
 *
 * <p>
 * If you do supply a mapping for <code>"tempFileName"</code> in the config map, you'll see that the temp file is available to all the tests:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSpec execute (configMap = Map("tempFileName" -> "tmp.txt"))
 * <span class="stGreen">ExampleSpec:
 * OneSpec:
 * The temp file
 * - should exist in OneSpec
 * TwoSpec:
 * The temp file
 * - should exist in TwoSpec
 * RedSpec:
 * The temp file
 * - should exist in RedSpec
 * BlueSpec:
 * The temp file
 * - should exist in BlueSpec
 * The temp file
 * - should exist in ExampleSpec</span>
 * </pre>
 *
 * <p>
 * <strong>Note: This trait does not currently ensure that the code in <code>afterAll</code> is executed after
 * all the tests and nested suites are executed if a <code>Distributor</code> is passed. The
 * plan is to do that eventually (in fact, in a soon-to-be-released 2.0 milestone), but in the meantime, be aware that <code>afterAll</code> is
 * guaranteed to be run after all the tests and nested suites only when they are run
 * sequentially.</strong>
 * </p>
 *
 * @author Bill Venners
 */
trait BeforeAndAfterAll  extends AbstractStyle { this: Suite =>

  /**
   * Defines a method to be run before any of this suite's tests or nested suites are run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes the overloaded form of this method that
   * takes a <code>configMap</code> before executing
   * any tests or nested suites. This trait's implementation of that <code>beforeAll(Map[String, Any])</code>
   * method simply invokes this <code>beforeAll()</code>
   * method. Thus this method can be used to set up a test fixture
   * needed by the entire suite, when you don't need anything from the <code>configMap</code>.
   * This trait's implementation of this method does nothing.
   * </p>
   */
  protected def beforeAll() = ()

  /**
   * Defines a method (that takes a <code>configMap</code>) to be run before any
   * of this suite's tests or nested suites are run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes this method before executing
   * any tests or nested suites (passing in the <code>configMap</code> passed to it), thus this
   * method can be used to set up a test fixture
   * needed by the entire suite. This trait's implementation of this method invokes the
   * overloaded form of <code>beforeAll</code> that takes no <code>configMap</code>.
   * </p>
   */
  protected def beforeAll(configMap: Map[String, Any]) {
    beforeAll()
  }

  /**
   * Defines a method to be run after all of this suite's tests and nested suites have
   * been run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes the overloaded form of this method that
   * takes a <code>configMap</code> after executing
   * all tests and nested suites. This trait's implementation of that <code>afterAll(Map[String, Any])</code> method simply invokes this
   * <code>afterAll()</code> method. Thus this method can be used to tear down a test fixture
   * needed by the entire suite, when you don't need anything from the <code>configMap</code>.
   * This trait's implementation of this method does nothing.
   * </p>
   */
  protected def afterAll() = ()

  /**
   * Defines a method (that takes a <code>configMap</code>) to be run after
   * all of this suite's tests and nested suites have been run.
   *
   * <p>
   * This trait's implementation
   * of <code>run</code> invokes this method after executing all tests
   * and nested suites (passing in the <code>configMap</code> passed to it), thus this
   * method can be used to tear down a test fixture
   * needed by the entire suite. This trait's implementation of this method invokes the
   * overloaded form of <code>afterAll</code> that takes no <code>configMap</code>.
   * </p>
   */
  protected def afterAll(configMap: Map[String, Any]) {
    afterAll()
  }

  /**
   * Execute a suite surrounded by calls to <code>beforeAll</code> and <code>afterAll</code>.
   *
   * <p>
   * This trait's implementation of this method ("this method") invokes <code>beforeAll(Map[String, Any])</code>
   * before executing any tests or nested suites and <code>afterAll(Map[String, Any])</code>
   * after executing all tests and nested suites. It runs the suite by invoking <code>super.run</code>, passing along
   * the seven parameters passed to it.
   * </p>
   *
   * <p>
   * If any invocation of <code>beforeAll</code> completes abruptly with an exception, this
   * method will complete abruptly with the same exception. If any call to
   * <code>super.run</code> completes abruptly with an exception, this method
   * will complete abruptly with the same exception, however, before doing so, it will
   * invoke <code>afterAll</code>. If <cod>afterAll</code> <em>also</em> completes abruptly with an exception, this
   * method will nevertheless complete abruptly with the exception previously thrown by <code>super.run</code>.
   * If <code>super.run</code> returns normally, but <code>afterAll</code> completes abruptly with an
   * exception, this method will complete abruptly with the same exception.
   * </p>
  */
  abstract override def run(testName: Option[String], args: Args) {
    var thrownException: Option[Throwable] = None

    beforeAll(args.configMap)
    try {
      super.run(testName, args)
    }
    catch {
      case e: Exception => thrownException = Some(e)
    }
    finally {
      try {
        afterAll(args.configMap) // Make sure that afterAll is called even if run completes abruptly.
        thrownException match {
          case Some(e) => throw e
          case None =>
        }
      }
      catch {
        case laterException: Exception =>
          thrownException match { // If both run and afterAll throw an exception, report the test exception
            case Some(earlierException) => throw earlierException
            case None => throw laterException
          }
      }
    }
  }
}
