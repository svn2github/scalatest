/*
 * Copyright 2001-2012 Artima, Inc.
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

import java.lang.annotation._
import java.io.Serializable
import java.lang.reflect.Constructor
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import Suite.simpleNameForTest
import Suite.parseSimpleName
import Suite.stripDollars
import Suite.formatterForSuiteStarting
import Suite.formatterForSuiteCompleted
import Suite.checkForPublicNoArgConstructor
import Suite.checkChosenStyles
import Suite.formatterForSuiteAborted
import Suite.anErrorThatShouldCauseAnAbort
import Suite.getSimpleNameOfAnObjectsClass
import Suite.takesInformer
import Suite.takesCommunicator
import Suite.isTestMethodGoodies
import Suite.testMethodTakesAnInformer
import scala.collection.immutable.TreeSet
import Suite.getIndentedTextForTest
import Suite.getEscapedIndentedTextForTest
import Suite.getDecodedName
import org.scalatest.events._
import org.scalatest.tools.StandardOutReporter
import Suite.getMessageForException
import Suite.reportTestStarting
import Suite.reportTestIgnored
import Suite.reportTestSucceeded
import Suite.reportTestPending
import Suite.reportInfoProvided
import Suite.createInfoProvided
import Suite.createMarkupProvided
import scala.reflect.NameTransformer
import tools.SuiteDiscoveryHelper
import tools.Runner
import exceptions.StackDepthExceptionHelper.getStackDepthFun
import exceptions._

/**
 * A suite of tests. A <code>Suite</code> instance encapsulates a conceptual
 * suite (<em>i.e.</em>, a collection) of tests.
 *
 * <p>
 * This trait provides an interface composed of "lifecycle methods" that allow suites of tests to be run.
 * Its implementation enables a default way of writing and executing tests.  Subtraits and subclasses can
 * override <code>Suite</code>'s lifecycle methods to enable other ways of writing and executing tests.
 * This trait's default approach allows tests to be defined as methods whose name is given in backticks and starts with "<code>test: </code>".
 * </p>
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>Suite</code> allows you to define tests as methods, which saves one generated class file per test compared to style traits that represent tests as functions.
 * As a result, using <code>Suite</code> can be a good choice in large projects where class file generation is a concern as well as when generating tests programatically
 * via a static code generator.
 * </td></tr></table>
 * 
 * <p>
 * To use this trait's approach to writing tests, simply create classes that
 * extend <code>Suite</code> and define test methods. Test methods have names of the form <code>`test: ...`</code>, 
 * where <code>...</code> is a string that specifies a bit of behavior required of the subject under test. A test method must be public and
 * can have any result type, but the most common result type is <code>Unit</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite
 *
 * import org.scalatest.Suite
 *
 * class SetSuite extends Suite {
 *
 *   def &#96;test: an empty Set should have size 0&#96; {
 *     assert(Set.empty.size === 0)
 *   }
 *
 *   def &#96;test: invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * You can run a <code>Suite</code> by invoking <code>execute</code> on it.
 * This method, which prints test results to the standard output, is intended to serve as a
 * convenient way to run tests from within the Scala interpreter. For example,
 * to run <code>SetSuite</code> from within the Scala interpreter, you could write:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * </pre>
 *
 * <p>
 * And you would see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:
 * - an empty Set should have size 0
 * - invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 *
 * <p>
 * Or, to run just the &ldquo;<code>test: an empty Set should have size 0</code>&rdquo; method, you could pass that test's name, or any unique substring of the
 * name, such as <code>"size 0"</code> or even just <code>"0"</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute "size 0"
 * <span class="stGreen">SetSuite:
 * - an empty Set should have size 0</span>
 * </pre>
 *
 * <p>
 * You can also pass to <code>execute</code> a <a href="#configMapSection"><em>config map</em></a> of key-value
 * pairs, which will be passed down into suites and tests, as well as other parameters that configure the run itself.
 * For more information on running in the Scala interpreter, see the documentation for <code>execute</code> (below) and the
 * <a href="Shell.html">ScalaTest shell</a>.
 * </p>
 *
 * <p>
 * The <code>execute</code> method invokes a <code>run</code> method takes two
 * parameters. This <code>run</code> method, which actually executes the suite, will usually be invoked by a test runner, such
 * as <a href="run$.html"><code>run</code></a>, <a href="tools/Runner$.html"><code>tools.Runner</code></a>, a build tool, or an IDE.
 * </p>
 *
 * <p>
 * The test methods shown in this example are parameterless. This is recommended even for test methods with obvious side effects. In production code
 * you would normally declare no-arg, side-effecting methods as <em>empty-paren</em> methods, and call them with
 * empty parentheses, to make it more obvious to readers of the code that they have a side effect. Whether or not a test method has
 * a side effect, however, is a less important distinction than it is for methods in production code. Moreover, test methods are not
 * normally invoked directly by client code, but rather through reflection by running the <code>Suite</code> that contains them, so a
 * lack of parentheses on an invocation of a side-effecting test method would not normally appear in any client code. Given the empty
 * parentheses do not add much value in the test methods case, the recommended style is to simply always leave them off.
 * </p>
 *
 * <p>
 * <em>Note: The approach of using backticks around test method names to make it easier to write descriptive test names was
 * inspired by the <a href="http://github.com/SimpleFinance/simplespec" target="_blank"><code>SimpleSpec</code></a> test framework, originally created by Coda Hale.</em>
 * </p>
 *
 * <a name="specialTreatment"></a>
 * <h2>Deprecation of old-style test method names</h2>
 * 
 * <p>
 * Prior to ScalaTest 2.0, trait <code>Suite</code> considered any method whose name begins with "test" as a test method. This form of test
 * method has been deprecated, to encourage more descriptive test names. Old-style test method names will continue to work during the deprecation period, but you'll
 * get a deprecation warning. Support for old-style method names will be removed in a future version of ScalaTest, so please change all old-style methods to
 * the new form as time permits. (It will be a long deprecation cycle.) If a test name is given in backticks and starts with <code>"test: "</code>, the name
 * will be shown by ScalaTest's reporters without the <code>"test: "</code> prefix to increase readability. Old-style test method names will show up intact, as
 * they did before. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.Suite
 *
 * class ExampleSuite extends Suite {
 *
 *   // This is the new style
 *   def &#96;test: this will be reported without the prefix&#96; {
 *     assert(1 + 1 === 2)
 *   }
 *
 *   // This is the deprecated form, because the space is missing
 *   def &#96;test:missing space&#96; {
 *     assert(1 + 1 === 2)
 *   }
 *
 *   // This is an example of the deprecated form
 *   def testThisFormIsSoOneDotOh {
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running the above class in the interpreter would give you:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSuite execute
 * <span class="stGreen">ExampleSuite:</span>
 * The name "test:missing space" has been deprecated. Please use the &#96;test: ...&#96; form instead.
 * The name "testThisIsSoOneDotOh" has been deprecated. Please use the &#96;test: ...&#96; form instead.
 * <span class="stGreen">- this will be reported without the prefix
 * - test:missing space
 * - testThisFormIsSoOneDotOh</span>
 * </pre>
 *
 * <h2>Assertions and <code>=</code><code>=</code><code>=</code></h2>
 *
 * <p>
 * Inside test methods in a <code>Suite</code>, you can write assertions by invoking <code>assert</code> and passing in a <code>Boolean</code> expression,
 * such as:
 * </p>
 *
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left == right)
 * </pre>
 *
 * <p>
 * If the passed expression is <code>true</code>, <code>assert</code> will return normally. If <code>false</code>,
 * <code>assert</code> will complete abruptly with a <code>TestFailedException</code>. This exception is usually not caught
 * by the test method, which means the test method itself will complete abruptly by throwing the <code>TestFailedException</code>. Any
 * test method that completes abruptly with an exception is considered a failed
 * test. A test method that returns normally is considered a successful test.
 * </p>
 *
 * <p>
 * If you pass a <code>Boolean</code> expression to <code>assert</code>, a failed assertion will be reported, but without
 * reporting the left and right values. You can alternatively encode these values in a <code>String</code> passed as
 * a second argument to <code>assert</code>, as in:
 * </p>
 * 
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left == right, left + " did not equal " + right)
 * </pre>
 *
 * <p>
 * Using this form of <code>assert</code>, the failure report will include the left and right values, 
 * helping you debug the problem. However, ScalaTest provides the <code>===</code> operator to make this easier.
 * (The <code>===</code> operator is defined in trait <a href="Assertions.html"><code>Assertions</code></a> which trait <code>Suite</code> extends.)
 * You use it like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val left = 2
 * val right = 1
 * assert(left === right)
 * </pre>
 *
 * <p>
 * Because you use <code>===</code> here instead of <code>==</code>, the failure report will include the left
 * and right values. For example, the detail message in the thrown <code>TestFailedException</code> from the <code>assert</code>
 * shown previously will include, "2 did not equal 1".
 * From this message you will know that the operand on the left had the value 2, and the operand on the right had the value 1.
 * </p>
 *
 * <p>
 * If you're familiar with JUnit, you would use <code>===</code>
 * in a ScalaTest <code>Suite</code> where you'd use <code>assertEquals</code> in a JUnit <code>TestCase</code>.
 * The <code>===</code> operator is made possible by an implicit conversion from <code>Any</code>
 * to <code>Equalizer</code>. If you're curious to understand the mechanics, see the <a href="Assertions$Equalizer.html">documentation for
 * <code>Equalizer</code></a> and the <code>convertToEqualizer</code> method.
 * </p>
 *
 * <h2>Expected results</h2>
 *
 * Although <code>===</code> provides a natural, readable extension to Scala's <code>assert</code> mechanism,
 * as the operands become lengthy, the code becomes less readable. In addition, the <code>===</code> comparison
 * doesn't distinguish between actual and expected values. The operands are just called <code>left</code> and <code>right</code>,
 * because if one were named <code>expected</code> and the other <code>actual</code>, it would be difficult for people to
 * remember which was which. To help with these limitations of assertions, <code>Suite</code> includes a method called <code>expectResult</code> that
 * can be used as an alternative to <code>assert</code> with <code>===</code>. To use <code>expectResult</code>, you place
 * the expected value in parentheses after <code>expectResult</code>, followed by curly braces containing code 
 * that should result in the expected value. For example:
 *
 * <pre class="stHighlight">
 * val a = 5
 * val b = 2
 * expectResult(2) {
 *   a - b
 * }
 * </pre>
 *
 * <p>
 * In this case, the expected value is <code>2</code>, and the code being tested is <code>a - b</code>. This expectation will fail, and
 * the detail message in the <code>TestFailedException</code> will read, "Expected 2, but got 3."
 * </p>
 *
 * <h2>Intercepted exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. You can do this in the JUnit style, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * try {
 *   s.charAt(-1)
 *   fail()
 * }
 * catch {
 *   case _: IndexOutOfBoundsException => // Expected, so continue
 * }
 * </pre>
 *
 * <p>
 * If <code>charAt</code> throws <code>IndexOutOfBoundsException</code> as expected, control will transfer
 * to the catch case, which does nothing. If, however, <code>charAt</code> fails to throw an exception,
 * the next statement, <code>fail()</code>, will be executed. The <code>fail</code> method always completes abruptly with
 * a <code>TestFailedException</code>, thereby signaling a failed test.
 * </p>
 *
 * <p>
 * To make this common use case easier to express and read, ScalaTest provides an <code>intercept</code>
 * method. You use it like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * intercept[IndexOutOfBoundsException] {
 *   s.charAt(-1)
 * }
 * </pre>
 *
 * <p>
 * This code behaves much like the previous example. If <code>charAt</code> throws an instance of <code>IndexOutOfBoundsException</code>,
 * <code>intercept</code> will return that exception. But if <code>charAt</code> completes normally, or throws a different
 * exception, <code>intercept</code> will complete abruptly with a <code>TestFailedException</code>. The <code>intercept</code> method returns the
 * caught exception so that you can inspect it further if you wish, for example, to ensure that data contained inside
 * the exception has the expected values. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val s = "hi"
 * val caught =
 *   intercept[IndexOutOfBoundsException] {
 *     s.charAt(-1)
 *   }
 * assert(caught.getMessage === "String index out of range: -1")
 * </pre>
 *
 * <h2>Using matchers and other assertions</h2>
 *
 * <p>
 * ScalaTest also supports another style of assertions via its matchers DSL. By mixing in
 * trait <a href="matchers/ShouldMatchers.html"><code>ShouldMatchers</code></a>, you can 
 * write suites that look like:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.matchers
 *
 * import org.scalatest._
 *
 * class SetSuite extends Suite with ShouldMatchers {
 *
 *   def &#96;test: an empty Set should have size 0&#96; {
 *     Set.empty.size should equal (0)
 *   }
 *
 *   def &#96;test: invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     evaluating { Set.empty.head } should produce [NoSuchElementException]
 *   }
 * }
 * </pre>
 * 
 * <p>If you prefer the word "<code>must</code>" to the word "<code>should</code>," you can alternatively mix in
 * trait <a href="matchers/MustMatchers.html"><code>MustMatchers</code></a>.
 * </p>
 *
 * <p>
 * If you are comfortable with assertion mechanisms from other test frameworks, chances
 * are you can use them with ScalaTest. Any assertion mechanism that indicates a failure with an exception
 * can be used as is with ScalaTest. For example, to use the <code>assertEquals</code>
 * methods provided by JUnit or TestNG, simply import them and use them. (You will of course need
 * to include the relevant JAR file for the framework whose assertions you want to use on either the
 * classpath or runpath when you run your tests.) 
 * </p>
 *
 * <h2>Nested suites</h2>
 *
 * <p>
 * A <code>Suite</code> can refer to a collection of other <code>Suite</code>s,
 * which are called <em>nested</em> <code>Suite</code>s. Those nested  <code>Suite</code>s can in turn have
 * their own nested  <code>Suite</code>s, and so on. Large test suites can be organized, therefore, as a tree of
 * nested <code>Suite</code>s.
 * This trait's <code>run</code> method, in addition to invoking its
 * test methods, invokes <code>run</code> on each of its nested <code>Suite</code>s.
 * </p>
 *
 * <p>
 * A <code>List</code> of a <code>Suite</code>'s nested <code>Suite</code>s can be obtained by invoking its
 * <code>nestedSuites</code> method. If you wish to create a <code>Suite</code> that serves as a
 * container for nested <code>Suite</code>s, whether or not it has test methods of its own, simply override <code>nestedSuites</code>
 * to return a <code>List</code> of the nested <code>Suite</code>s. Because this is a common use case, ScalaTest provides
 * a convenience <code>Suites</code> class, which takes a variable number of nested <code>Suite</code>s as constructor
 * parameters. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.nested
 *
 * import org.scalatest._
 *
 * class ASuite extends Suite {
 *   def &#96;test: A should have ASCII value 41 hex&#96; {
 *     assert('A' === 0x41)
 *   }
 *   def &#96;test: a should have ASCII value 61 hex&#96; {
 *     assert('a' === 0x61)
 *   }
 * }
 * class BSuite extends Suite {
 *   def &#96;test: B should have ASCII value 42 hex&#96; {
 *     assert('B' === 0x42)
 *   }
 *   def &#96;test: b should have ASCII value 62 hex&#96; {
 *     assert('b' === 0x62)
 *   }
 * }
 * class CSuite extends Suite {
 *   def &#96;test: C should have ASCII value 43 hex&#96; {
 *     assert('C' === 0x43)
 *   }
 *   def &#96;test: c should have ASCII value 63 hex&#96; {
 *     assert('c' === 0x63)
 *   }
 * }
 *
 * class ASCIISuite extends Suites(
 *   new ASuite,
 *   new BSuite,
 *   new CSuite
 * )
 * </pre>
 *
 * <p>
 * If you now run <code>ASCIISuite</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ASCIISuite execute
 * </pre>
 *
 * <p>
 * You will see reports printed to the standard output that indicate the nested
 * suites&#8212;<code>ASuite</code>, <code>BSuite</code>, and
 * <code>CSuite</code>&#8212;were run:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">ASCIISuite:
 * ASuite:
 * - A should have ASCII value 41 hex
 * - a should have ASCII value 61 hex
 * BSuite:
 * - B should have ASCII value 42 hex
 * - b should have ASCII value 62 hex
 * CSuite:
 * - C should have ASCII value 43 hex
 * - c should have ASCII value 63 hex</span>
 * </pre>
 *
 * <p>
 * Note that <code>Runner</code> can discover <code>Suite</code>s automatically, so you need not
 * necessarily define nested <code>Suites</code> explicitly. See the <a href="tools/Runner$.html#membersOnlyWildcard">documentation
 * for <code>Runner</code></a> for more information.
 * </p>
 *
 * <a name="configMapSection"></a><h2>The config map</h2>
 *
 * <p>
 * In some cases you may need to pass information to a suite of tests.
 * For example, perhaps a suite of tests needs to grab information from a file, and you want
 * to be able to specify a different filename during different runs.  You can accomplish this in ScalaTest by passing
 * the filename in a <em>config map</em> of key-value pairs, which is passed to <code>run</code> as a <code>Map[String, Any]</code>.
 * The values in the config map are called "config objects," because they can be used to <em>configure</em>
 * suites, reporters, and tests.
 * </p>
 *
 * <p>
 * You can specify a string config object is via the ScalaTest <code>Runner</code>, either via the command line
 * or ScalaTest's ant task.
 * (See the <a href="tools/Runner$.html#configMapSection">documentation for Runner</a> for information on how to specify 
 * config objects on the command line.)
 * The config map is passed to <code>run</code>, <code>runNestedSuites</code>, <code>runTests</code>, and <code>runTest</code>,
 * so one way to access it in your suite is to override one of those methods. If you need to use the config map inside your tests, you
 * can access it from the <code>NoArgTest</code> passed to <code>withFixture</code>, or the <code>OneArgTest</code> passed to
 * <code>withFixture</code> in the traits in the <code>org.scalatest.fixture</code> package. (See the
 * <a href="fixture/Suite.html">documentation for <code>fixture.Suite</code></a>
 * for instructions on how to access the config map in tests.)
 * </p>
 *
 * <h2>Ignored tests</h2>
 *
 * <p>
 * Another common use case is that tests must be &#8220;temporarily&#8221; disabled, with the
 * good intention of resurrecting the test at a later time. ScalaTest provides an <code>Ignore</code>
 * annotation for this purpose. You use it like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.ignore
 *
 * import org.scalatest._
 *
 * class SetSuite extends Suite {
 *
 *   @Ignore def &#96;test: an empty Set should have size 0&#96; {
 *     assert(Set.empty.size === 0)
 *   }
 *
 *   def &#96;test: invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>SetSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * </pre>
 *
 * <p>
 * It will run only the second test and report that the first test was ignored. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:
 * <span class="stYellow">- an empty Set should have size 0 !!! IGNORED !!!</span>
 * <span class="stGreen">- invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 * 
 * <p>
 * <code>Ignore</code> is implemented as a tag. The <code>Filter</code> class effectively 
 * adds <code>org.scalatest.Ignore</code> to the <code>tagsToExclude</code> <code>Set</code> if it not already
 * in the <code>tagsToExclude</code> set passed to its primary constructor.  The only difference between
 * <code>org.scalatest.Ignore</code> and the tags you may define and exclude is that ScalaTest reports
 * ignored tests to the <code>Reporter</code>. The reason ScalaTest reports ignored tests is 
 * to encourage ignored tests to be eventually fixed and added back into the active suite of tests.
 * </p>
 *
 * <h2>Pending tests</h2>
 *
 * <p>
 * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
 * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
 * out before tests are written to verify that behavior (and often, before the behavior of
 * the system being tested is itself implemented). Such sketches form a kind of specification of
 * what tests and functionality to implement later.
 * </p>
 *
 * <p>
 * To support this style of testing, a test can be given a name that specifies one
 * bit of behavior required by the system being tested. The test can also include some code that
 * sends more information about the behavior to the reporter when the tests run. At the end of the test,
 * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
 * </p>
 *
 * <p>
 * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
 * sent to the reporter when running the test can appear in the report of a test run. (In other words,
 * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
 * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
 * the actual test, and possibly the functionality it is intended to test, has not yet been implemented.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.pending
 *
 * import org.scalatest._
 *
 * class SetSuite extends Suite {
 *
 *   def &#96;test: an empty Set should have size 0&#96; { pending }
 *
 *   def &#96;test: invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>SetSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * </pre>
 *
 * <p>
 * It will run both tests but report that the first one is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:
 * <span class="stYellow">- an empty Set should have size 0 (pending)</span>
 * <span class="stGreen">- invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 * 
 * 
 * <h2>Using <code>info</code> and <code>markup</code></h2>
 *
 * <p>
 * One of the parameters to the <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <a href="Informer.html"><code>Informer</code></a> that will forward information
 * to the current <code>Reporter</code> is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example that shows both a direct use as well as an indirect use through the methods
 * of <a href="GivenWhenThen.html"><code>GivenWhenThen</code></a>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.info
 *
 * import collection.mutable
 * import org.scalatest._
 * 
 * class SetSuite extends Suite with GivenWhenThen {
 *
 *   def &#96;test: an element can be added to an empty mutable Set&#96; {
 *
 *     given("an empty mutable Set")
 *     val set = mutable.Set.empty[String]
 *
 *     when("an element is added")
 *     set += "clarity"
 *
 *     then("the Set should have size 1")
 *     assert(set.size === 1)
 *
 *     and("the Set should contain the added element")
 *     assert(set.contains("clarity"))
 *
 *     info("That's all folks!")
 *   }
 * }
 * </pre>
 *
 * If you run this <code>Suite</code> from the interpreter, you will see the messages
 * included in the output:
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * <span class="stGreen">SetSuite:
 * - an element can be added to an empty mutable Set
 *   + Given an empty mutable Set
 *   + When an element is added
 *   + Then the Set should have size 1
 *   + And the Set should contain the added element
 *   + That's all folks!</span>
 * </pre>
 *
 * <p>
 * Trait <code>Suite</code> also carries a <a href="Documenter.html"><code>Documenter</code></a> named <code>markup</code>, which
 * you can use to transmit markup text to the <code>Reporter</code>.
 * </p>
 *
 * <h2>Executing suites in parallel</h2>
 *
 * <p>
 * The <code>run</code> method takes as one of its parameters an optional <code>Distributor</code>. If 
 * a <code>Distributor</code> is passed in, this trait's implementation of <code>run</code> puts its nested
 * <code>Suite</code>s into the distributor rather than executing them directly. The caller of <code>run</code>
 * is responsible for ensuring that some entity runs the <code>Suite</code>s placed into the 
 * distributor. The <code>-P</code> command line parameter to <code>Runner</code>, for example, will cause
 * <code>Suite</code>s put into the <code>Distributor</code> to be run in parallel via a pool of threads.
 * If you wish to execute the tests themselves in parallel, mix in <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a>.
 * </p>
 *
 * <a name="TaggingTests"></a><h2>Tagging tests</h2>
 *
 * <p>
 * A <code>Suite</code>'s tests may be classified into groups by <em>tagging</em> them with string names. When executing
 * a <code>Suite</code>, groups of tests can optionally be included and/or excluded. In this
 * trait's implementation, tags are indicated by annotations attached to the test method. To
 * create a new tag type to use in <code>Suite</code>s, simply define a new Java annotation that itself is annotated with
 * the <code>org.scalatest.TagAnnotation</code> annotation.
 * (Currently, for annotations to be
 * visible in Scala programs via Java reflection, the annotations themselves must be written in Java.) For example,
 * to create a tag named <code>SlowAsMolasses</code>, to use to mark slow tests, you would
 * write in Java:
 * </p>
 *
 * <pre>
 * import java.lang.annotation.*; 
 * import org.scalatest.TagAnnotation
 * 
 * @TagAnnotation
 * @Retention(RetentionPolicy.RUNTIME)
 * @Target({ElementType.METHOD, ElementType.TYPE})
 * public @interface SlowAsMolasses {}
 * </pre>
 *
 * <p>
 * Given this new annotation, you could place a <code>Suite</code> test method into the <code>SlowAsMolasses</code> group
 * (<em>i.e.</em>, tag it as being <code>SlowAsMolasses</code>) like this:
 * </p>
 *
 * <pre class="stHighlight">
 * @SlowAsMolasses
 * def &#96;test: to sleep, perchance to dream&#96; { Thread.sleep(1000000) }
 * </pre>
 *
 * <p>
 * The <code>run</code> method takes a <code>Filter</code>, whose constructor takes an optional
 * <code>Set[String]</code> called <code>tagsToInclude</code> and a <code>Set[String]</code> called
 * <code>tagsToExclude</code>. If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
 * except those those belonging to tags listed in the
 * <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
 * belonging to tags mentioned in the <code>tagsToInclude</code> set, and not mentioned in <code>tagsToExclude</code>,
 * will be run.
 * </p>
 *
 * <a name="sharedFixtures"></a><h2>Shared fixtures</h2>
 *
 * <p>
 * A test <em>fixture</em> is composed of the objects and other artifacts (files, sockets, database
 * connections, <em>etc.</em>) tests use to do their work.
 * When multiple tests need to work with the same fixtures, it is important to try and avoid
 * duplicating the fixture code across those tests. The more code duplication you have in your
 * tests, the greater drag the tests will have on refactoring the actual production code.
 * ScalaTest recommends several techniques to eliminate such code duplication, and provides several
 * traits to help. Each technique is geared towards helping you reduce code duplication without introducing
 * instance <code>var</code>s, shared mutable objects, or other dependencies between tests. Eliminating shared
 * mutable state across tests will make your test code easier to reason about and more amenable for parallel
 * test execution.
 * </p>
 *
 * <p>
 * The following sections
 * describe these techniques, including explaining the recommended usage
 * for each. But first, here's a table summarizing the options:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">Technique</th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">Recommended uses</th></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#getFixtureMethods">get-fixture methods</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need the same mutable fixture objects in multiple tests, and don't need to clean up after.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#fixtureContextObjects">fixture-context objects</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need different combinations of mutable fixture objects in different tests, and don't need to clean up after. </td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#oneInstancePerTest"><code>OneInstancePerTest</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when porting JUnit tests to ScalaTest, or if you prefer JUnit's approach to test isolation: running each test in its own instance of the test class.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#withFixtureNoArgTest"><code>withFixture(NoArgTest)</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need to perform side effects at the beginning and end of all or most tests, or want to stack traits that perform such side-effects.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#loanFixtureMethods">loan-fixture methods</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when different tests need different fixtures that must be cleaned up afterwords.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when all or most tests need the same fixtures that must be cleaned up afterwords.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#beforeAndAfter"><code>BeforeAndAfter</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need to perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#composingFixtures"><code>BeforeAndAfterEach</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you want to stack traits that perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.</td></td></tr>
 * </table>
 *
 * <a name="getFixtureMethods"></a>
 * <h4>Calling get-fixture methods</h4>
 *
 * <p>
 * If you need to create the same mutable fixture objects in multiple tests, and don't need to clean them up after using them, the simplest approach is to write one or
 * more <em>get-fixture</em> methods. A get-fixture method returns a new instance of a needed fixture object (or an holder object containing
 * multiple fixture objects) each time it is called. You can call a get-fixture method at the beginning of each
 * test that needs the fixture, storing the returned object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.getfixture
 *
 * import org.scalatest.Suite
 * import collection.mutable.ListBuffer
 *
 * class ExampleSuite extends Suite {
 * 
 *   def fixture =
 *     new {
 *       val builder = new StringBuilder("ScalaTest is ")
 *       val buffer = new ListBuffer[String]
 *     }
 * 
 *   def &#96;test: testing should be easy&#96; {
 *     val f = fixture
 *     f.builder.append("easy!")
 *     assert(f.builder.toString === "ScalaTest is easy!")
 *     assert(f.buffer.isEmpty)
 *     f.buffer += "sweet"
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; {
 *     val f = fixture
 *     f.builder.append("fun!")
 *     assert(f.builder.toString === "ScalaTest is fun!")
 *     assert(f.buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * The &ldquo;<code>f.</code>&rdquo; in front of each use of a fixture object provides a visual indication of which objects 
 * are part of the fixture, but if you prefer, you can import the the members with &ldquo;<code>import f._</code>&rdquo; and use the names directly.
 * </p>
 *
 * <p>
 * If you need to configure fixture objects differently in different tests, you can pass configuration into the get-fixture method. For example, if you could pass
 * in an initial value for a mutable fixture object as a parameter to the get-fixture method.
 * </p>
 *
 * <a name="fixtureContextObjects"></a>
 * <h4>Instantiating fixture-context objects </h4>
 *
 * <p>
 * A alternate technique that is especially useful when different tests need different combinations of fixture objects is to define the fixture objects as instance variables
 * of <em>fixture-context objects</em> whose instantiation forms the body of tests. Like get-fixture methods, fixture-context objects are anly
 * appropriate if you don't need to clean up the fixtures after using them.
 * </p>
 *
 * To use this technique, you define instance variables intialized with fixture objects in traits and/or classes, then in each test instantiate an object that
 * contains just the fixture objects needed by the test. Keep in mind that traits allow you to mix together just the fixture objects needed by each test, whereas classes
 * allow you to pass data in via a constructor to configure the fixture objects. Here's an example in which fixture objects are partitioned into two traits
 * and each test just gets mixes together the traits it needs:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.fixturecontext
 *
 * import collection.mutable.ListBuffer
 * import org.scalatest.Suite
 * 
 * class ExampleSuite extends Suite {
 * 
 *   trait Builder {
 *     val builder = new StringBuilder("ScalaTest is ")
 *   }
 * 
 *   trait Buffer {
 *     val buffer = ListBuffer("ScalaTest", "is")
 *   }
 * 
 *   // This test needs the StringBuilder fixture
 *   def &#96;test: testing should be productive&#96; {
 *     new Builder {
 *       builder.append("productive!")
 *       assert(builder.toString === "ScalaTest is productive!")
 *     }
 *   }
 * 
 *   // This test needs the ListBuffer[String] fixture
 *   def &#96;test: test code should be readable&#96; {
 *     new Buffer {
 *       buffer += ("readable!")
 *       assert(buffer === List("ScalaTest", "is", "readable!"))
 *     }
 *   }
 * 
 *   // This test needs both the StringBuilder and ListBuffer
 *   def &#96;test: test code should be clear and concise&#96; {
 *     new Builder with Buffer {
 *       builder.append("clear!")
 *       buffer += ("concise!")
 *       assert(builder.toString === "ScalaTest is clear!")
 *       assert(buffer === List("ScalaTest", "is", "concise!"))
 *     }
 *   }
 * }
 * </pre>
 *
 * <a name="oneInstancePerTest"></a>
 * <h4>Mixing in <code>OneInstancePerTest</code></h4>
 *
 * <p>
 * If every test method requires the same set of
 * mutable fixture objects, and none require cleanup, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>Suite</code>, similar to the way JUnit tests are executed. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.oneinstancepertest
 *
 * import org.scalatest._
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSuite extends Suite with OneInstancePerTest {
 * 
 *   val builder = new StringBuilder("ScalaTest is ")
 *   val buffer = new ListBuffer[String]
 * 
 *   def &#96;test: testing should be easy&#96; {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * One way to think of <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a> is that the entire <code>Suite</code> instance is like a fixture-context object,
 * but with the difference that the test code doesn't run during construction as it does with the real fixture-context object technique. Because this trait emulates JUnit's manner
 * of running tests, this trait can be helpful when porting JUnit tests to ScalaTest. The primary intended use of <code>OneInstancePerTest</code> is to serve as a supertrait for
 * <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a> and the <a href="path/package.html">path traits</a>, but you can also mix it in
 * directly to help you port JUnit tests to ScalaTest or if you prefer JUnit's approach to test isolation.
 * </p>
 *
 * <a name="withFixtureNoArgTest"></a>
 * <h4>Overriding <code>withFixture(NoArgTest)</code></h4>
 *
 * <p>
 * Although the get-fixture method, fixture-context object, and <code>OneInstancePerTest</code> approaches take care of setting up a fixture at the beginning of each
 * test, they don't address the problem of cleaning up a fixture at the end of the test. If you just need to perform a side-effect at the beginning or end of
 * a test, and don't need to actually pass any fixture objects into the test, you can override <code>withFixture(NoArgTest)</code>, one of ScalaTest's
 * lifecycle methods defined in trait <a href="AbstractSuite.html"><code>AbstractSuite</code></a>.
 * </p>
 *
 * <p>
 * Trait <code>Suite</code>'s implementation of <code>runTest</code> passes a no-arg test function to <code>withFixture(NoArgTest)</code>. It is <code>withFixture</code>'s
 * responsibility to invoke that test function. <code>Suite</code>'s implementation of <code>withFixture</code> simply
 * invokes the function, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Default implementation in trait Suite
 * protected def withFixture(test: NoArgTest) {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * You can, therefore, override <code>withFixture</code> to perform setup before and/or cleanup after invoking the test function. If
 * you have cleanup to perform, you should invoke the test function inside a <code>try</code> block and perform the cleanup in
 * a <code>finally</code> clause, because the exception that causes a test to fail will propagate through <code>withFixture</code> back
 * to <code>runTest</code>. (In other words, if the test fails, the test function invoked by <code>withFixture</code> will throw an exception.)
 * </p>
 *
 * <p>
 * The <code>withFixture</code> method is designed to be stacked, and to enable this, you should always call the <code>super</code> implementation
 * of <code>withFixture</code>, and let it invoke the test function rather than invoking the test function directly. In other words, instead of writing
 * &ldquo;<code>test()</code>&rdquo;, you should write &ldquo;<code>super.withFixture(test)</code>&rdquo;, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgTest) {
 *   // Perform setup
 *   try {
 *     super.withFixture(test) // Invoke the test function
 *   }
 *   finally {
 *     // Perform cleanup
 *   }
 * }
 * </pre>
 *
 * <p>
 * Here's an example in which <code>withFixture(NoArgTest)</code> is used to take a snapshot of the working directory if a test fails, and 
 * and send that information to the reporter:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.noargtest
 *
 * import java.io.File
 * import org.scalatest.FunSuite
 *
 * class ExampleSuite extends FunSuite {
 *
 *   final val tmpDir = "tmpDir"
 *
 *   override def withFixture(test: NoArgTest) {
 *     
 *     try {
 *       super.withFixture(test)
 *     }
 *     catch {
 *       case e: Exception =&gt;
 *         val currDir = new File(".")
 *         val fileNames = currDir.list()
 *         info("Dir snapshot: " + fileNames.mkString(", "))
 *         throw e
 *     }
 *   }
 *
 *   test("this test should succeed") {
 *     assert(1 + 1 === 2)
 *   }
 *
 *   test("this test should fail") {
 *     assert(1 + 1 === 3)
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running this version of <code>ExampleSuite</code> in the interpreter in a directory with two files, <code>hello.txt</code> and <code>world.txt</code>
 * would give the following output:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSuite execute
 * <span class="stGreen">ExampleSuite:
 * - this test should succeed
 * <span class="stRed">- this test should fail *** FAILED ***
 *   2 did not equal 3 (<console>:33)
 *   + Dir snapshot: hello.txt, world.txt </span>
 * </pre>
 *
 * <p>
 * Note that the <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> passed to <code>withFixture</code>, in addition to
 * an <code>apply</code> method that executes the test, also includes the test name and the <a href="#configMapSection">config
 * map</a> passed to <code>runTest</code>. Thus you can also use the test name and configuration objects in your <code>withFixture</code>
 * implementation.
 * </p>
 *
 * <a name="loanFixtureMethods"></a>
 * <h4>Calling loan-fixture methods</h4>
 *
 * <p>
 * If you need to both pass a fixture object into a test <em>and</em> and perform cleanup at the end of the test, you'll need to use the <em>loan pattern</em>.
 * If different tests need different fixtures that require cleanup, you can implement the loan pattern directly by writing <em>loan-fixture</em> methods.
 * A loan-fixture method takes a function whose body forms part or all of a test's code. It creates a fixture, passes it to the test code by invoking the
 * function, then cleans up the fixture after the function returns.
 * </p>
 *
 * <p>
 * The following example shows three tests that use two fixtures, a database and a file. Both require cleanup after, so each is provided via a
 * loan-fixture method. (In this example, the database is simulated with a <code>StringBuffer</code>.)
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.loanfixture
 *
 * import java.util.concurrent.ConcurrentHashMap
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
 * import org.scalatest.Suite
 * import DbServer._
 * import java.util.UUID.randomUUID
 * import java.io._
 * 
 * class ExampleSuite extends Suite {
 * 
 *   def withDatabase(testCode: Db =&gt; Any) {
 *     val dbName = randomUUID.toString
 *     val db = createDb(dbName) // create the fixture
 *     try {
 *       db.append("ScalaTest is ") // perform setup
 *       testCode(db) // "loan" the fixture to the test
 *     }
 *     finally {
 *       removeDb(dbName) // clean up the fixture
 *     }
 *   }
 * 
 *   def withFile(testCode: (File, FileWriter) =&gt; Any) {
 *     val file = File.createTempFile("hello", "world") // create the fixture
 *     val writer = new FileWriter(file)
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       testCode(file, writer) // "loan" the fixture to the test
 *     }
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   // This test needs the file fixture
 *   def &#96;test: testing should be productive&#96; {
 *     withFile { (file, writer) =&gt;
 *       writer.write("productive!")
 *       writer.flush()
 *       assert(file.length === 24)
 *     }
 *   }
 * 
 *   // This test needs the database fixture
 *   def &#96;test: test code should be readable&#96; {
 *     withDatabase { db =&gt;
 *       db.append("readable!")
 *       assert(db.toString === "ScalaTest is readable!")
 *     }
 *   }
 * 
 *   // This test needs both the file and the database
 *   def &#96;test: test code should be clear and concise&#96; {
 *     withDatabase { db =&gt;
 *       withFile { (file, writer) =&gt; // loan-fixture methods compose
 *         db.append("clear!")
 *         writer.write("concise!")
 *         writer.flush()
 *         assert(db.toString === "ScalaTest is clear!")
 *         assert(file.length === 21)
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * As demonstrated by the last test, loan-fixture methods compose. Not only do loan-fixture methods allow you to
 * give each test the fixture it needs, they allow you to give a test multiple fixtures and clean everything up afterwords.
 * </p>
 *
 * <p>
 * Also demonstrated in this example is the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in this example. This keeps tests completely isolated, allowing you to run them in parallel if desired.
 * </p>
 *
 * </pre>
 * <a name="withFixtureOneArgTest"></a>
 * <h4>Overriding <code>withFixture(OneArgTest)</code></h4>
 *
 * <p>
 * If all or most tests need the same fixture, you can avoid some of the boilerplate of the loan-fixture method approach by using a <code>fixture.Suite</code>
 * and overriding <code>withFixture(OneArgTest)</code>.
 * Each test in a <code>fixture.Suite</code> takes a fixture as a parameter, allowing you to pass the fixture into
 * the test. You must indicate the type of the fixture parameter by specifying <code>FixtureParam</code>, and implement a
 * <code>withFixture</code> method that takes a <code>OneArgTest</code>. This <code>withFixture</code> method is responsible for
 * invoking the one-arg test function, so you can perform fixture set up before, and clean up after, invoking and passing
 * the fixture into the test function.
 * </p>
 *
 * <p>
 * As with <code>withFixture(NoArgTest)</code>, to enable trait stacking it is a good idea to always delegate to the <code>super</code> implementation
 * of <code>withFixture(NoArgTest)</code> instead of invoking the test function directly. In this case, however, you'll need to convert the
 * <code>OneArgTest</code> to a <code>NoArgTest</code>. You can do that by supplying the fixture object to <code>toNoArgTest</code> method of
 * <code>OneArgTest</code>. In other words, instead of writing &ldquo;<code>test("fixture")</code>&rdquo;, you'd write
 * &ldquo;<code>super.withFixture(test.toNoArgTest("fixture"))</code>&rdquo;.  Here's a complete example:
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
 *     val file = File.createTempFile("hello", "world") // create the fixture
 *     val writer = new FileWriter(file)
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       super.withFixture(test.toNoArgTest(F(file, writer))) // "loan" the fixture to the test
 *     }
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   def &#96;test: testing should be easy&#96; (f: F) {
 *     f.writer.write("easy!")
 *     f.writer.flush()
 *     assert(f.file.length === 12)
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; (f: F) {
 *     f.writer.write("fun!")
 *     f.writer.flush()
 *     assert(f.file.length === 9)
 *   }
 * }
 * </pre>
 *
 * <p>
 * In this example, the tests actually required two fixture objects, a <code>File</code> and a <code>FileWriter</code>. In such situations you can
 * simply define the <code>FixtureParam</code> type to be a tuple containing the objects, or as is done in this example, a case class containing
 * the objects.  For more information on the <code>withFixture(OneArgTest)</code> technique, see the <a href="fixture/Suite.html">documentation for <code>fixture.Suite</code></a>.
 * </p>
 *
 * <a name="beforeAndAfter"></a>
 * <h4>Mixing in <code>BeforeAndAfter</code></h4>
 *
 * <p>
 * In all the shared fixture examples shown so far, the activities of creating, setting up, and cleaning up the fixture objects have been
 * performed <em>during</em> the test.  This means that if an exception occurs during any of these activities, it will be reported as a test failure.
 * Sometimes, however, you may want setup to happen <em>before</em> the test starts, and cleanup <em>after</em> the test has completed, so that if an
 * exception occurs during setup or cleanup, the entire suite aborts and no more tests are attempted. The simplest way to accomplish this in ScalaTest is
 * to mix in trait <a href="BeforeAndAfter.html"><code>BeforeAndAfter</code></a>.  With this trait you can denote a bit of code to run before each test
 * with <code>before</code> and/or after each test each test with <code>after</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.beforeandafter
 *
 * import org.scalatest.Suite
 * import org.scalatest.BeforeAndAfter
 * import collection.mutable.ListBuffer
 *
 * class ExampleSuite extends Suite with BeforeAndAfter {
 *
 *   val builder = new StringBuilder
 *   val buffer = new ListBuffer[String]
 *
 *   before {
 *     builder.append("ScalaTest is ")
 *   }
 *
 *   after {
 *     builder.clear()
 *     buffer.clear()
 *   }
 *
 *   def &#96;test: testing should be easy&#96; {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 *
 *   def &#96;test: testing should be fun&#96; {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note that the only way <code>before</code> and <code>after</code> code can communicate with test code is via some side-effecting mechanism, commonly by
 * reassigning instance <code>var</code>s or by changing the state of mutable objects held from instance <code>val</code>s (as in this example). If using
 * instance <code>var</code>s or mutable objects held from instance <code>val</code>s you wouldn't be able to run tests in parallel in the same instance
 * of the test class unless you synchronized access to the shared, mutable state. This is why ScalaTest's <code>ParallelTestExecution</code> trait extends
 * <code>OneInstancePerTest</code>. By running each test in its own instance of the class, each test has its own copy of the instance variables, so you
 * don't need to synchronize. If you mixed <code>ParallelTestExecution</code> into the <code>ExampleSuite</code> above, the tests would run in parallel just fine
 * without any synchronization needed on the mutable <code>StringBuilder</code> and <code>ListBuffer[String]</code> objects.
 * </p>
 *
 * <p>
 * Although <code>BeforeAndAfter</code> provides a minimal-boilerplate way to execute code before and after tests, it isn't designed to enable stackable
 * traits, because the order of execution would be non-obvious.  If you want to factor out before and after code that is common to multiple test suites, you 
 * should use trait <code>BeforeAndAfterEach</code> instead, as shown later in the next section,
 * <a href="#composingFixtures.html">composing fixtures by stacking traits</a>.
 * </p>
 *
 * <a name="composingFixtures"></a><h2>Composing fixtures by stacking traits</h2>
 *
 * <p>
 * In larger projects, teams often end up with several different fixtures that test classes need in different combinations,
 * and possibly initialized (and cleaned up) in different orders. A good way to accomplish this in ScalaTest is to factor the individual
 * fixtures into traits that can be composed using the <em>stackable trait</em> pattern. This can be done, for example, by placing
 * <code>withFixture</code> methods in several traits, each of which call <code>super.withFixture</code>. Here's an example in
 * which the <code>StringBuilder</code> and <code>ListBuffer[String]</code> fixtures used in the previous examples have been
 * factored out into two <em>stackable fixture traits</em> named <code>Builder</code> and <code>Buffer</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.composingwithfixture
 *
 * import org.scalatest.Suite
 * import org.scalatest.AbstractSuite
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends AbstractSuite { this: Suite =>
 * 
 *   val builder = new StringBuilder
 * 
 *   abstract override def withFixture(test: NoArgTest) {
 *     builder.append("ScalaTest is ")
 *     try {
 *       super.withFixture(test) // To be stackable, must call super.withFixture
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends AbstractSuite { this: Suite =>
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   abstract override def withFixture(test: NoArgTest) {
 *     try {
 *       super.withFixture(test) // To be stackable, must call super.withFixture
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSuite extends Suite with Builder with Buffer {
 * 
 *   def &#96;test: testing should be easy&#96; {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *     buffer += "clear"
 *   }
 * }
 * </pre>
 *
 * <p>
 * By mixing in both the <code>Builder</code> and <code>Buffer</code> traits, <code>ExampleSuite</code> gets both fixtures, which will be
 * initialized before each test and cleaned up after. The order the traits are mixed together determines the order of execution.
 * In this case, <code>Builder</code> is "super" to </code>Buffer</code>. If you wanted <code>Buffer</code> to be "super"
 * to <code>Builder</code>, you need only switch the order you mix them together, like this: 
 * </p>
 *
 * <pre class="stHighlight">
 * class Example2Suite extends Suite with Buffer with Builder
 * </pre>
 *
 * <p>
 * And if you only need one fixture you mix in only that trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class Example3Suite extends Suite with Builder
 * </pre>
 *
 * <p>
 * Another way to create stackable fixture traits is by extending the <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>
 * and/or <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a> traits.
 * <code>BeforeAndAfterEach</code> has a <code>beforeEach</code> method that will be run before each test (like JUnit's <code>setUp</code>),
 * and an <code>afterEach</code> method that will be run after (like JUnit's <code>tearDown</code>).
 * Similarly, <code>BeforeAndAfterAll</code> has a <code>beforeAll</code> method that will be run before all tests,
 * and an <code>afterAll</code> method that will be run after all tests. Here's what the previously shown example would look like if it
 * were rewritten to use the <code>BeforeAndAfterEach</code> methods instead of <code>withFixture</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.composingbeforeandaftereach
 *
 * import org.scalatest.Suite
 * import org.scalatest.BeforeAndAfterEach
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends BeforeAndAfterEach { this: Suite =>
 * 
 *   val builder = new StringBuilder
 * 
 *   override def beforeEach() {
 *     builder.append("ScalaTest is ")
 *     super.beforeEach() // To be stackable, must call super.beforeEach
 *   }
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends BeforeAndAfterEach { this: Suite =>
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSuite extends Suite with Builder with Buffer {
 * 
 *   def &#96;test: testing should be easy&#96; {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   def &#96;test: testing should be fun&#96; {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *     buffer += "clear"
 *   }
 * }
 * </pre>
 *
 * <p>
 * To get the same ordering as <code>withFixture</code>, place your <code>super.beforeEach</code> call at the end of each
 * <code>beforeEach</code> method, and the <code>super.afterEach</code> call at the beginning of each <code>afterEach</code>
 * method, as shown in the previous example. It is a good idea to invoke <code>super.afterEach</code> in a <code>try</code>
 * block and perform cleanup in a <code>finally</code> clause, as shown in the previous example, because this ensures the
 * cleanup code is performed even if <code>super.afterEach</code> throws an exception.
 * </p>
 *
 * <p>
 * The difference between stacking traits that extend <code>BeforeAndAfterEach</code> versus traits that implement <code>withFixture</code> is
 * that setup and cleanup code happens before and after the test in <code>BeforeAndAfterEach</code>, but at the beginning and
 * end of the test in <code>withFixture</code>. Thus if a <code>withFixture</code> method completes abruptly with an exception, it is
 * considered a failed test. By contrast, if any of the <code>beforeEach</code> or <code>afterEach</code> methods of <code>BeforeAndAfterEach</code> 
 * complete abruptly, it is considered a failed suite, which will result in a <a href="events/SuiteAborted.html"><code>SuiteAborted</code></a> event.
 * </p>
 * 
 * <a name="errorHandling"></a>
 * <h2>Treatment of <code>java.lang.Error</code>s</h2>
 *
 * <p>
 * The Javadoc documentation for <code>java.lang.Error</code> states:
 * </p>
 *
 * <blockquote>
 * An <code>Error</code> is a subclass of <code>Throwable</code> that indicates serious problems that a reasonable application should not try to catch. Most
 * such errors are abnormal conditions.
 * </blockquote>
 *
 * <p>
 * Because <code>Error</code>s are used to denote serious errors, trait <code>Suite</code> and its subtypes in the ScalaTest API do not always treat a test
 * that completes abruptly with an <code>Error</code> as a test failure, but sometimes as an indication that serious problems
 * have arisen that should cause the run to abort. For example, if a test completes abruptly with an <code>OutOfMemoryError</code>, 
 * it will not be reported as a test failure, but will instead cause the run to abort. Because not everyone uses <code>Error</code>s only to represent serious
 * problems, however, ScalaTest only behaves this way for the following exception types (and their subclasses):
 * </p>
 *
 * <ul>
 * <li><code>java.lang.annotation.AnnotationFormatError</code></li>
 * <li><code>java.awt.AWTError</code></li>
 * <li><code>java.nio.charset.CoderMalfunctionError</code></li>
 * <li><code>javax.xml.parsers.FactoryConfigurationError</code></li>
 * <li><code>java.lang.LinkageError</code></li>
 * <li><code>java.lang.ThreadDeath</code></li>
 * <li><code>javax.xml.transform.TransformerFactoryConfigurationError</code></li>
 * <li><code>java.lang.VirtualMachineError</code></li>
 * </ul>
 *
 * <p>
 * The previous list includes all <code>Error</code>s that exist as part of Java 1.5 API, excluding <code>java.lang.AssertionError</code>. ScalaTest
 * does treat a thrown <code>AssertionError</code> as an indication of a test failure. In addition, any other <code>Error</code> that is not an instance of a
 * type mentioned in the previous list will be caught by the <code>Suite</code> traits in the ScalaTest API and reported as the cause of a test failure. 
 * </p>
 *
 * <p>
 * Although trait <code>Suite</code> and all its subtypes in the ScalaTest API consistently behave this way with regard to <code>Error</code>s,
 * this behavior is not required by the contract of <code>Suite</code>. Subclasses and subtraits that you define, for example, may treat all
 * <code>Error</code>s as test failures, or indicate errors in some other way that has nothing to do with exceptions.
 * </p>
 *
 * <h2>Extensibility</h2>
 *
 * <p>
 * Trait <code>Suite</code> provides default implementations of its methods that should
 * be sufficient for most applications, but many methods can be overridden when desired. Here's
 * a summary of the methods that are intended to be overridden:
 * </p>
 *
 * <ul>
 * <li><code>run</code> - override this method to define custom ways to run suites of
 *   tests.</li>
 * <li><code>runNestedSuites</code> - override this method to define custom ways to run nested suites.</li>
 * <li><code>runTests</code> - override this method to define custom ways to run a suite's tests.</li>
 * <li><code>runTest</code> - override this method to define custom ways to run a single named test.</li>
 * <li><code>testNames</code> - override this method to specify the <code>Suite</code>'s test names in a custom way.</li>
 * <li><code>tags</code> - override this method to specify the <code>Suite</code>'s test tags in a custom way.</li>
 * <li><code>nestedSuites</code> - override this method to specify the <code>Suite</code>'s nested <code>Suite</code>s in a custom way.</li>
 * <li><code>suiteName</code> - override this method to specify the <code>Suite</code>'s name in a custom way.</li>
 * <li><code>expectedTestCount</code> - override this method to count this <code>Suite</code>'s expected tests in a custom way.</li>
 * </ul>
 *
 * <p>
 * For example, this trait's implementation of <code>testNames</code> performs reflection to discover methods starting with <code>test</code>,
 * and places these in a <code>Set</code> whose iterator returns the names in alphabetical order. If you wish to run tests in a different
 * order in a particular <code>Suite</code>, perhaps because a test named <code>testAlpha</code> can only succeed after a test named
 * <code>testBeta</code> has run, you can override <code>testNames</code> so that it returns a <code>Set</code> whose iterator returns
 * <code>testBeta</code> <em>before</em> <code>testAlpha</code>. (This trait's implementation of <code>run</code> will invoke tests
 * in the order they come out of the <code>testNames</code> <code>Set</code> iterator.)
 * </p>
 *
 * <p>
 * Alternatively, you may not like starting your test methods with <code>test</code>, and prefer using <code>@Test</code> annotations in
 * the style of Java's JUnit 4 or TestNG. If so, you can override <code>testNames</code> to discover tests using either of these two APIs
 * <code>@Test</code> annotations, or one of your own invention. (This is in fact
 * how <code>org.scalatest.junit.JUnitSuite</code> and <code>org.scalatest.testng.TestNGSuite</code> work.)
 * </p>
 *
 * <p>
 * Moreover, <em>test</em> in ScalaTest does not necessarily mean <em>test method</em>. A test can be anything that can be given a name,
 * that starts and either succeeds or fails, and can be ignored. In <code>org.scalatest.FunSuite</code>, for example, tests are represented
 * as function values. This
 * approach might look foreign to JUnit users, but may feel more natural to programmers with a functional programming background.
 * To facilitate this style of writing tests, <code>FunSuite</code> overrides <code>testNames</code>, <code>runTest</code>, and <code>run</code> such that you can 
 * define tests as function values.
 * </p>
 *
 * <p>
 * You can also model existing JUnit 3, JUnit 4, or TestNG tests as suites of tests, thereby incorporating tests written in Java into a ScalaTest suite.
 * The "wrapper" classes in packages <code>org.scalatest.junit</code> and <code>org.scalatest.testng</code> exist to make this easy.
 * No matter what legacy tests you may have, it is likely you can create or use an existing <code>Suite</code> subclass that allows you to model those tests
 * as ScalaTest suites and tests and incorporate them into a ScalaTest suite. You can then write new tests in Scala and continue supporting
 * older tests in Java.
 * </p>
 *
 * @author Bill Venners
 */
@Style("org.scalatest.finders.MethodFinder")
trait Suite extends Assertions with AbstractSuite with Serializable { thisSuite =>

  import Suite.TestMethodPrefix, Suite.InformerInParens, Suite.IgnoreAnnotation

  /**
   * A test function taking no arguments, which also provides a test name and config map.
   *
   * <p>
   * <code>Suite</code>'s implementation of <code>runTest</code> passes instances of this trait
   * to <code>withFixture</code> for every test method it executes. It invokes <code>withFixture</code>
   * for every test, including test methods that take an <code>Informer</code>. For the latter case,
   * the <code>Informer</code> to pass to the test method is already contained inside the
   * <code>NoArgTest</code> instance passed to <code>withFixture</code>.
   * </p>
   */
  protected trait NoArgTest extends (() => Unit) with TestData {
    
    /**
     * Runs the code of the test.
     */
    def apply()

  }

  /**
  * A <code>List</code> of this <code>Suite</code> object's nested <code>Suite</code>s. If this <code>Suite</code> contains no nested <code>Suite</code>s,
  * this method returns an empty <code>List</code>. This trait's implementation of this method returns an empty <code>List</code>.
  */
  def nestedSuites: IndexedSeq[Suite] = Vector.empty

  /**
   * Executes one or more tests in this <code>Suite</code>, printing results to the standard output.
   *
   * <p>
   * This method invokes <code>run</code> on itself, passing in values that can be configured via the parameters to this
   * method, all of which have default values. This behavior is convenient when working with ScalaTest in the Scala interpreter.
   * Here's a summary of this method's parameters and how you can use them:
   * </p>
   *
   * <p>
   * <strong>The <code>testName</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave <code>testName</code> at its default value (of <code>null</code>), this method will pass <code>None</code> to
   * the <code>testName</code> parameter of <code>run</code>, and as a result all the tests in this suite will be executed. If you
   * specify a <code>testName</code>, this method will pass <code>Some(testName)</code> to <code>run</code>, and only that test
   * will be run. Thus to run all tests in a suite from the Scala interpreter, you can write:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute()
   * </pre>
   *
   * <p>
   * To run just the test named <code>"my favorite test"</code> in a suite from the Scala interpreter, you would write:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute("my favorite test")
   * </pre>
   *
   * <p>
   * Or:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(testName = "my favorite test")
   * </pre>
   *
   * <p>
   * <strong>The <code>configMap</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you provide a value for the <code>configMap</code> parameter, this method will pass it to <code>run</code>. If not, the default value
   * of an empty <code>Map</code> will be passed. For more information on how to use a config map to configure your test suites, see
   * the <a href="#configMapSection">config map section</a> in the main documentation for this trait. Here's an example in which you configure
   * a run with the name of an input file:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(configMap = Map("inputFileName" -> "in.txt")
   * </pre>
   *
   * <p>
   * <strong>The <code>color</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave the <code>color</code> parameter unspecified, this method will configure the reporter it passes to <code>run</code> to print
   * to the standard output in color (via ansi escape characters). If you don't want color output, specify false for <code>color</code>, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(color = false)
   * </pre>
   *
   * <p>
   * <strong>The <code>durations</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave the <code>durations</code> parameter unspecified, this method will configure the reporter it passes to <code>run</code> to
   * <em>not</em> print durations for tests and suites to the standard output. If you want durations printed, specify true for <code>durations</code>,
   * like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(durations = true)
   * </pre>
   *
   * <p>
   * <strong>The <code>shortstacks</code> and <code>fullstacks</code> parameters</strong>
   * </p>
   *
   * <p>
   * If you leave both the <code>shortstacks</code> and <code>fullstacks</code> parameters unspecified, this method will configure the reporter
   * it passes to <code>run</code> to <em>not</em> print stack traces for failed tests if it has a stack depth that identifies the offending
   * line of test code. If you prefer a short stack trace (10 to 15 stack frames) to be printed with any test failure, specify true for
   * <code>shortstacks</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(shortstacks = true)
   * </pre>
   *
   * <p>
   * For full stack traces, set <code>fullstacks</code> to true:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(fullstacks = true)
   * </pre>
   *
   * <p>
   * If you specify true for both <code>shortstacks</code> and <code>fullstacks</code>, you'll get full stack traces.
   * </p>
   *
   * <p>
   * <strong>The <code>stats</code> parameter</strong>
   * </p>
   *
   * <p>
   * If you leave the <code>stats</code> parameter unspecified, this method will <em>not</em> fire <code>RunStarting</code> and either <code>RunCompleted</code>
   * or <code>RunAborted</code> events to the reporter it passes to <code>run</code>.
   * If you specify true for <code>stats</code>, this method will fire the run events to the reporter, and the reporter will print the
   * expected test count before the run, and various statistics after, including the number of suites completed and number of tests that
   * succeeded, failed, were ignored or marked pending. Here's how you get the stats:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; (new ExampleSuite).execute(stats = true)
   * </pre>
   *
   *
   * <p>
   * To summarize, this method will pass to <code>run</code>:
   * </p>
   * <ul>
   * <li><code>testName</code> - <code>None</code> if this method's <code>testName</code> parameter is left at its default value of <code>null</code>, else <code>Some(testName)</code>.
   * <li><code>reporter</code> - a reporter that prints to the standard output</li>
   * <li><code>stopper</code> - a <code>Stopper</code> whose <code>apply</code> method always returns <code>false</code></li>
   * <li><code>filter</code> - a <code>Filter</code> constructed with <code>None</code> for <code>tagsToInclude</code> and <code>Set()</code>
   *   for <code>tagsToExclude</code></li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method</li>
   * <li><code>distributor</code> - <code>None</code></li>
   * <li><code>tracker</code> - a new <code>Tracker</code></li>
   * </ul>
   *
   * <p>
   * Note:  In ScalaTest, the terms "execute" and "run" basically mean the same thing and
   * can be used interchangably. The reason this method isn't named <code>run</code> is that it takes advantage of
   * default arguments, and you can't mix overloaded methods and default arguments in Scala. (If named <code>run</code>,
   * this method would have the same name but different arguments than the main <a href="#run"><code>run</code> method</a> that
   * takes seven arguments. Thus it would overload and couldn't be used with default argument values.)
   * </p>
   *
   * <p>
   * Design note: This method has two "features" that may seem unidiomatic. First, the default value of <code>testName</code> is <code>null</code>.
   * Normally in Scala the type of <code>testName</code> would be <code>Option[String]</code> and the default value would
   * be <code>None</code>, as it is in this trait's <code>run</code> method. The <code>null</code> value is used here for two reasons. First, in
   * ScalaTest 1.5, <code>execute</code> was changed from four overloaded methods to one method with default values, taking advantage of
   * the default and named parameters feature introduced in Scala 2.8.
   * To not break existing source code, <code>testName</code> needed to have type <code>String</code>, as it did in two of the overloaded
   * <code>execute</code> methods prior to 1.5. The other reason is that <code>execute</code> has always been designed to be called primarily
   * from an interpeter environment, such as the Scala REPL (Read-Evaluate-Print-Loop). In an interpreter environment, minimizing keystrokes is king.
   * A <code>String</code> type with a <code>null</code> default value lets users type <code>suite.execute("my test name")</code> rather than
   * <code>suite.execute(Some("my test name"))</code>, saving several keystrokes.
   * </p>
   *
   * <p>
   * The second non-idiomatic feature is that <code>shortstacks</code> and <code>fullstacks</code> are all lower case rather than
   * camel case. This is done to be consistent with the <a href="Shell.html"><code>Shell</code></a>, which also uses those forms. The reason 
   * lower case is used in the <code>Shell</code> is to save keystrokes in an interpreter environment.  Most Unix commands, for
   * example, are all lower case, making them easier and quicker to type.  In the ScalaTest
   * <code>Shell</code>, methods like <code>shortstacks</code>, <code>fullstacks</code>, and <code>nostats</code>, <em>etc.</em>, are 
   * designed to be all lower case so they feel more like shell commands than methods.
   * </p>
   *
   * @param testName the name of one test to run.
   * @param configMap a <code>Map</code> of key-value pairs that can be used by the executing <code>Suite</code> of tests.
   * @param color a boolean that configures whether output is printed in color
   * @param durations a boolean that configures whether test and suite durations are printed to the standard output
   * @param shortstacks a boolean that configures whether short stack traces should be printed for test failures
   * @param fullstacks a boolean that configures whether full stack traces should be printed for test failures
   * @param stats a boolean that configures whether test and suite statistics are printed to the standard output
   *
   * @throws NullPointerException if the passed <code>configMap</code> parameter is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  final def execute(
    testName: String = null,
    configMap: Map[String, Any] = Map(),
    color: Boolean = true,
    durations: Boolean = false,
    shortstacks: Boolean = false,
    fullstacks: Boolean = false,
    stats: Boolean = false
  ) {
    if (configMap == null)
      throw new NullPointerException("configMap was null")
    val SelectedTag = "Selected"
    val SelectedSet = Set(SelectedTag)
    val desiredTests: Set[String] =
      if (testName == null) Set.empty
      else {
        testNames.filter { s =>
          s.indexOf(testName) >= 0 || NameTransformer.decode(s).indexOf(testName) >= 0
        }
      }
    if (testName != null && desiredTests.isEmpty)
      throw new IllegalArgumentException(Resources("testNotFound", testName))

    val dispatch = new DispatchReporter(List(new StandardOutReporter(durations, color, shortstacks, fullstacks)))
    val tracker = new Tracker
    val filter =
      if (testName == null) Filter()
      else {
        val taggedTests: Map[String, Set[String]] = desiredTests.map(_ -> SelectedSet).toMap
        Filter(
          tagsToInclude = Some(SelectedSet),
          excludeNestedSuites = true,
          dynaTags = DynaTags(Map.empty, Map(suiteId -> taggedTests))
        )
      }
    val runStartTime = System.currentTimeMillis
    if (stats)
      dispatch(RunStarting(tracker.nextOrdinal(), expectedTestCount(filter), configMap))

    val suiteStartTime = System.currentTimeMillis
    def dispatchSuiteAborted(e: Throwable) {
      val eMessage = e.getMessage
      val rawString = 
        if (eMessage != null && eMessage.length > 0)
          Resources("runOnSuiteException")
        else
          Resources("runOnSuiteExceptionWithMessage", eMessage)
      val formatter = formatterForSuiteAborted(thisSuite, rawString)
      val duration = System.currentTimeMillis - suiteStartTime
      dispatch(SuiteAborted(tracker.nextOrdinal(), rawString, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), thisSuite.decodedSuiteName, Some(e), Some(duration), formatter, Some(SeeStackDepthException)))
    }

    try {

      val formatter = formatterForSuiteStarting(thisSuite)
      dispatch(SuiteStarting(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.suiteId, thisSuite.decodedSuiteName, Some(thisSuite.getClass.getName), formatter, Some(getTopOfClass)))

      run(
        None,
        Args(dispatch,
        new Stopper {},
        filter,
        configMap,
        None,
        tracker,
        Set.empty)
      )
    // TODO: Go through and change all "new Stopper {}" with new JustCantStop or something to save class files
      val suiteCompletedFormatter = formatterForSuiteCompleted(thisSuite)
      val duration = System.currentTimeMillis - suiteStartTime
      dispatch(SuiteCompleted(tracker.nextOrdinal(), thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), thisSuite.decodedSuiteName, Some(duration), suiteCompletedFormatter, Some(getTopOfClass)))
      if (stats) {
        val duration = System.currentTimeMillis - runStartTime
        dispatch(RunCompleted(tracker.nextOrdinal(), Some(duration)))
      }
    }
    catch {
      case e: InstantiationException =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources("cannotInstantiateSuite", e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: IllegalAccessException =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources("cannotInstantiateSuite", e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: NoClassDefFoundError =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources("cannotLoadClass", e.getMessage), Some(e), Some(System.currentTimeMillis - runStartTime)))
      case e: Throwable =>
        dispatchSuiteAborted(e)
        dispatch(RunAborted(tracker.nextOrdinal(), Resources.bigProblems(e), Some(e), Some(System.currentTimeMillis - runStartTime)))
    }
    finally {
      dispatch.dispatchDisposeAndWaitUntilDone()
    }
  }

  final def execute { execute() }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names with which tests in this <code>Suite</code> are marked, and
   * whose values are the <code>Set</code> of test names marked with each tag.  If this <code>Suite</code> contains no tags, this
   * method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation of this method uses Java reflection to discover any Java annotations attached to its test methods. The
   * fully qualified name of each unique annotation that extends <code>TagAnnotation</code> is considered a tag. This trait's
   * implementation of this method, therefore, places one key/value pair into to the
   * <code>Map</code> for each unique tag annotation name discovered through reflection. The mapped value for each tag name key will contain
   * the test method name, as provided via the <code>testNames</code> method. 
   * </p>
   *
   * <p>
   * Subclasses may override this method to define and/or discover tags in a custom manner, but overriding method implementations
   * should never return an empty <code>Set</code> as a value. If a tag has no tests, its name should not appear as a key in the
   * returned <code>Map</code>.
   * </p>
   */
  def tags: Map[String, Set[String]] = {
    def getTags(testName: String) =
      for {
        a <- getMethodForTestName(testName).getDeclaredAnnotations
        annotationClass = a.annotationType
        if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
      } yield annotationClass.getName

    val testNameList = testNames
      
    val testTags = Map() ++ 
      (for (testName <- testNameList; if !getTags(testName).isEmpty)
        yield testName -> (Set() ++ getTags(testName)))

    val suiteTags = for { 
      a <- getClass.getDeclaredAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName
    
    val autoTestTags = 
      if (suiteTags.size > 0)
        Map() ++ testNameList.map(tn => (tn, suiteTags.toSet))
      else
        Map.empty[String, Set[String]]
    
    Runner.mergeMap[String, Set[String]](List(testTags, autoTestTags)) ( _ ++ _ ) 
  }

  /**
  * A <code>Set</code> of test names. If this <code>Suite</code> contains no tests, this method returns an empty <code>Set</code>.
  *
  * <p>
  * This trait's implementation of this method uses Java reflection to discover all public methods whose name starts with <code>"test"</code>,
  * which take either nothing or a single <code>Informer</code> as parameters. For each discovered test method, it assigns a test name
  * comprised of just the method name if the method takes no parameters, or the method name plus <code>(Informer)</code> if the
  * method takes a <code>Informer</code>. Here are a few method signatures and the names that this trait's implementation assigns them:
  * </p>
  *
  * <pre class="stHighlight">
  * def testCat() {}         // test name: "testCat"
  * def testCat(Informer) {} // test name: "testCat(Informer)"
  * def testDog() {}         // test name: "testDog"
  * def testDog(Informer) {} // test name: "testDog(Informer)"
  * def test() {}            // test name: "test"
  * def test(Informer) {}    // test name: "test(Informer)"
  * </pre>
  *
  * <p>
  * This trait's implementation of this method returns an immutable <code>Set</code> of all such names, excluding the name
  * <code>testNames</code>. The iterator obtained by invoking <code>elements</code> on this
  * returned <code>Set</code> will produce the test names in their <em>natural order</em>, as determined by <code>String</code>'s
  * <code>compareTo</code> method.
  * </p>
  *
  * <p>
  * This trait's implementation of <code>runTests</code> invokes this method
  * and calls <code>runTest</code> for each test name in the order they appear in the returned <code>Set</code>'s iterator.
  * Although this trait's implementation of this method returns a <code>Set</code> whose iterator produces <code>String</code>
  * test names in a well-defined order, the contract of this method does not required a defined order. Subclasses are free to
  * override this method and return test names in an undefined order, or in a defined order that's different from <code>String</code>'s
  * natural order.
  * </p>
  *
  * <p>
  * Subclasses may override this method to produce test names in a custom manner. One potential reason to override <code>testNames</code> is
  * to run tests in a different order, for example, to ensure that tests that depend on other tests are run after those other tests.
  * Another potential reason to override is allow tests to be defined in a different manner, such as methods annotated <code>@Test</code> annotations
  * (as is done in <code>JUnitSuite</code> and <code>TestNGSuite</code>) or test functions registered during construction (as is
  * done in <code>FunSuite</code> and <code>FunSpec</code>).
  * </p>
  */
  def testNames: Set[String] = {

    def isTestMethod(m: Method) = {

      // Factored out to share code with fixture.Suite.testNames
      val (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags) = isTestMethodGoodies(m)

      isInstanceMethod && (firstFour == "test") && ((hasNoParams && !isTestNames && !isTestTags) || takesInformer(m) || takesCommunicator(m))
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m)) 
        yield if (takesInformer(m)) m.getName + InformerInParens else m.getName

    val result = TreeSet.empty[String](EncodedOrdering) ++ testNameArray
    if (result.size != testNameArray.length) {
      throw new NotAllowedException("Howdy", 0)
    }
    result
  }

  /*
  Old style method names will have (Informer) at the end still, but new ones will
  not. This method will find the one without a Rep if the same name is used
  with and without a Rep.
   */
  private[scalatest] def getMethodForTestName(testName: String) =
    try {
      getClass.getMethod(
        simpleNameForTest(testName),
        (if (testMethodTakesAnInformer(testName)) Array(classOf[Informer]) else new Array[Class[_]](0)): _*
      )
    }
    catch {
      case e: NoSuchMethodException =>
        // Try (Rep) on the end
        try {
          getClass.getMethod(simpleNameForTest(testName), classOf[Rep])
        }
        catch {
          case e: NoSuchMethodException =>
            throw new IllegalArgumentException(Resources("testNotFound", testName))
        }
      case e =>
        throw e
    }

  /**
   *  Run the passed test function in the context of a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as reassigning instance <code>var</code>s in this <code>Suite</code> or initializing
   * a globally accessible external database. If you want to avoid reassigning instance <code>var</code>s
   * you can use <a href="fixture/Suite.html">fixture.Suite</a>.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTest</code> invokes this method for each test, passing
   * in a <code>NoArgTest</code> whose <code>apply</code> method will execute the code of the test.
   * </p>
   *
   * <p>
   * This trait's implementation of this method simply invokes the passed <code>NoArgTest</code> function.
   * </p>
   *
   * @param test the no-arg test function to run with a fixture
   */
  protected def withFixture(test: NoArgTest) {
    test()
  }

  // Factored out to share this with fixture.Suite.runTest
  private[scalatest] def getSuiteRunTestGoodies(stopper: Stopper, reporter: Reporter, testName: String) = {
    val (stopRequested, report, testStartTime) = getRunTestGoodies(stopper, reporter, testName)
    val method = getMethodForTestName(testName)
    (stopRequested, report, method, testStartTime)
  }

  // Sharing this with FunSuite and fixture.FunSuite as well as Suite and fixture.Suite
  private[scalatest] def getRunTestGoodies(stopper: Stopper, reporter: Reporter, testName: String) = {

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)

    val testStartTime = System.currentTimeMillis

    (stopRequested, report, testStartTime)
  }

  /**
   * Run a test.
   *
   * <p>
   * This trait's implementation uses Java reflection to invoke on this object the test method identified by the passed <code>testName</code>.
   * </p>
   *
   * <p>
   * Implementations of this method are responsible for ensuring a <code>TestStarting</code> event
   * is fired to the <code>Reporter</code> before executing any test, and either <code>TestSucceeded</code>,
   * <code>TestFailed</code>, or <code>TestPending</code> after executing any nested
   * <code>Suite</code>. (If a test is marked with the <code>org.scalatest.Ignore</code> tag, the
   * <code>runTests</code> method is responsible for ensuring a <code>TestIgnored</code> event is fired and that
   * this <code>runTest</code> method is not invoked for that ignored test.)
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, <code>configMap</code>
   *     or <code>tracker</code> is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected def runTest(testName: String, args: Args) {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    if (!testName.startsWith("test$colon$u0020"))
      println("The name \"" + NameTransformer.decode(testName) + "\" has been deprecated. Please use the `test: ...` form instead.")

    import args._

    val (stopRequested, report, method, testStartTime) =
      getSuiteRunTestGoodies(stopper, reporter, testName)

    reportTestStarting(this, report, tracker, testName, testName, getDecodedName(testName), rerunner, Some(getTopOfMethod(testName)))

    val formatter = getEscapedIndentedTextForTest(testName, 1, true)

    val messageRecorderForThisTest = new MessageRecorder(report)
    val informerForThisTest =
      MessageRecordingInformer(
        messageRecorderForThisTest, 
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, report, tracker, Some(testName), message, payload, 2, location, isConstructingThread, true, Some(testWasPending), Some(testWasCanceled))
      )

    // TODO: Was using reportInfoProvided here before, to double check with Bill for changing to markup provided.
    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest, 
        (message, _, isConstructingThread, testWasPending, testWasCanceled, location) => createMarkupProvided(thisSuite, report, tracker, Some(testName), message, 2, location, isConstructingThread, Some(testWasPending)) // TODO: Need a test that fails because testWasCanceleed isn't being passed
      )

    class RepImpl(val info: Informer, val markup: Documenter) extends Rep

    def testMethodTakesARep(method: Method): Boolean = {
      val paramTypes = method.getParameterTypes
      (paramTypes.size == 1) && (paramTypes(0) eq classOf[Rep])
    }

    val argsArray: Array[Object] =
      if (testMethodTakesAnInformer(testName)) {
        Array(informerForThisTest)  
      }
      else if (testMethodTakesARep(method)) {
        Array(new RepImpl(informerForThisTest, documenterForThisTest))
      }
      else Array()

    try {
      val theConfigMap = configMap
      withFixture(
        new NoArgTest {
          def name = testName
          def apply() { method.invoke(thisSuite, argsArray: _*) }
          def configMap = theConfigMap
        }
      )
      val duration = System.currentTimeMillis - testStartTime
      reportTestSucceeded(this, report, tracker, testName, testName, getDecodedName(testName), messageRecorderForThisTest.recordedEvents(false, false), duration, formatter, rerunner, Some(getTopOfMethod(method)))
    }
    catch { 
      case ite: InvocationTargetException =>
        val t = ite.getTargetException
        t match {
          case _: TestPendingException =>
            val duration = System.currentTimeMillis - testStartTime
            // testWasPending = true so info's printed out in the finally clause show up yellow
            reportTestPending(this, report, tracker, testName, testName, getDecodedName(testName), messageRecorderForThisTest.recordedEvents(true, false), duration, formatter, Some(getTopOfMethod(method)))
          case e: TestCanceledException =>
            val duration = System.currentTimeMillis - testStartTime
            val message = getMessageForException(e)
            val formatter = getEscapedIndentedTextForTest(testName, 1, true)
            // testWasCanceled = true so info's printed out in the finally clause show up yellow
            report(TestCanceled(tracker.nextOrdinal(), message, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), thisSuite.decodedSuiteName, 
                                testName, testName, getDecodedName(testName), messageRecorderForThisTest.recordedEvents(false, true), Some(e), Some(duration), Some(formatter), Some(TopOfMethod(thisSuite.getClass.getName, method.toGenericString())), rerunner))
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
  
  /**
   * Run zero to many of this <code>Suite</code>'s tests.
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is defined, this trait's implementation of this method 
   * invokes <code>runTest</code> on this object, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> value of the <code>testName</code> <code>Option</code> passed
   *   to this method</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> <code>Map</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * This method takes a <code>Filter</code>, which encapsulates an optional <code>Set</code> of tag names that should be included
   * (<code>tagsToInclude</code>) and a <code>Set</code> that should be excluded (<code>tagsToExclude</code>), when deciding which
   * of this <code>Suite</code>'s tests to run.
   * If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
   * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
   * belonging to tags mentioned in the <code>tagsToInclude</code> <code>Set</code>, and not mentioned in the <code>tagsToExclude</code <code>Set</code>
   * will be run. However, if <code>testName</code> is defined, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
   * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
   * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. This trait's implementation
   * behaves this way, and it is part of the general contract of this method, so all overridden forms of this method should behave
   * this way as well.  For more information on test tags, see the main documentation for this trait and for class <a href="Filter"><code>Filter</code></a>.
   * Note that this means that even if a test is marked as ignored, for example a test method in a <code>Suite</code> annotated with
   * <code>org.scalatest.Ignore</code>, if that test name is passed as <code>testName</code> to <code>runTest</code>, it will be invoked
   * despite the <code>Ignore</code> annotation.
   * </p>
   *
   * <p>
   * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially run.
   * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
   * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be run.)
   * For each test in the <code>testName</code> <code>Set</code>, in the order
   * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
   * of this method checks whether the test should be run based on the <code>Filter</code>.
   * If so, this implementation invokes <code>runTest</code>, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> name of the test to run (which will be one of the names in the <code>testNames</code> <code>Set</code>)</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * If a test is marked with the <code>org.scalatest.Ignore</code> tag, implementations
   * of this method are responsible for ensuring a <code>TestIgnored</code> event is fired for that test
   * and that <code>runTest</code> is not called for that test.
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected def runTests(testName: Option[String], args: Args) {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val theTestNames = testNames
    if (theTestNames.size > 0)
      checkChosenStyles(configMap, styleName)

    val stopRequested = stopper

    // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
    // so that exceptions are caught and transformed
    // into error messages on the standard error stream.
    val report = wrapReporterIfNecessary(reporter)
    val newArgs = args.copy(reporter = report)

    // If a testName is passed to run, just run that, else run the tests returned
    // by testNames.
    testName match {

      case Some(tn) =>
        val (filterTest, ignoreTest) = filter(tn, tags, suiteId)
        if (!filterTest) {
          if (ignoreTest)
            reportTestIgnored(thisSuite, report, tracker, tn, tn, getDecodedName(tn), getEscapedIndentedTextForTest(tn, 1, true), Some(getTopOfMethod(tn)))
          else
            runTest(tn, newArgs)
        }

      case None =>
        for ((tn, ignoreTest) <- filter(theTestNames, tags, suiteId)) {
          if (!stopRequested()) {
            if (ignoreTest)
              reportTestIgnored(thisSuite, report, tracker, tn, tn, getDecodedName(tn), getEscapedIndentedTextForTest(tn, 1, true), Some(getTopOfMethod(tn)))
            else
              runTest(tn, newArgs)
          }
      }
    }
  }

  /**
   * Runs this suite of tests.
   *
   * <p>If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * calls these two methods on this object in this order:</p>
   *
   * <ol>
   * <li><code>runNestedSuites(report, stopper, tagsToInclude, tagsToExclude, configMap, distributor)</code></li>
   * <li><code>runTests(testName, report, stopper, tagsToInclude, tagsToExclude, configMap)</code></li>
   * </ol>
   *
   * <p>
   * If <code>testName</code> is defined, then this trait's implementation of this method
   * calls <code>runTests</code>, but does not call <code>runNestedSuites</code>. This behavior
   * is part of the contract of this method. Subclasses that override <code>run</code> must take
   * care not to call <code>runNestedSuites</code> if <code>testName</code> is defined. (The
   * <code>OneInstancePerTest</code> trait depends on this behavior, for example.)
   * </p>
   *
   * <p>
   * Subclasses and subtraits that override this <code>run</code> method can implement them without
   * invoking either the <code>runTests</code> or <code>runNestedSuites</code> methods, which
   * are invoked by this trait's implementation of this method. It is recommended, but not required,
   * that subclasses and subtraits that override <code>run</code> in a way that does not
   * invoke <code>runNestedSuites</code> also override <code>runNestedSuites</code> and make it
   * final. Similarly it is recommended, but not required,
   * that subclasses and subtraits that override <code>run</code> in a way that does not
   * invoke <code>runTests</code> also override <code>runTests</code> (and <code>runTest</code>,
   * which this trait's implementation of <code>runTests</code> calls) and make it
   * final. The implementation of these final methods can either invoke the superclass implementation
   * of the method, or throw an <code>UnsupportedOperationException</code> if appropriate. The
   * reason for this recommendation is that ScalaTest includes several traits that override
   * these methods to allow behavior to be mixed into a <code>Suite</code>. For example, trait
   * <code>BeforeAndAfterEach</code> overrides <code>runTests</code>s. In a <code>Suite</code>
   * subclass that no longer invokes <code>runTests</code> from <code>run</code>, the
   * <code>BeforeAndAfterEach</code> trait is not applicable. Mixing it in would have no effect.
   * By making <code>runTests</code> final in such a <code>Suite</code> subtrait, you make
   * the attempt to mix <code>BeforeAndAfterEach</code> into a subclass of your subtrait
   * a compiler error. (It would fail to compile with a complaint that <code>BeforeAndAfterEach</code>
   * is trying to override <code>runTests</code>, which is a final method in your trait.) 
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   *         
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  def run(testName: Option[String], args: Args) {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)
    val newArgs = args.copy(reporter = report)

    testName match {
      case None => runNestedSuites(newArgs)
      case Some(_) =>
    }
    runTests(testName, newArgs)

    if (stopRequested()) {
      val rawString = Resources("executeStopping")
      report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), thisSuite.decodedSuiteName, 
             testName match {
               case Some(name) => Some(TestNameInfo(name, getDecodedName(name)))
               case None => None
             }
        ))))
    }
  }

  // TODO see if I can take away the [scalatest] from the private
  private[scalatest] def handleFailedTest(throwable: Throwable, testName: String, recordedEvents: IndexedSeq[RecordableEvent], report: Reporter, tracker: Tracker, formatter: Formatter, duration: Long) {

    val message = getMessageForException(throwable)
    //val formatter = getEscapedIndentedTextForTest(testName, 1, true)
    val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
    report(TestFailed(tracker.nextOrdinal(), message, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), thisSuite.decodedSuiteName, testName, testName, getDecodedName(testName), recordedEvents, Some(throwable), Some(duration), Some(formatter), Some(SeeStackDepthException), rerunner, payload))
  }

  /**
   *
   * Run zero to many of this <code>Suite</code>'s nested <code>Suite</code>s.
   *
   * <p>
   * If the passed <code>distributor</code> is <code>None</code>, this trait's
   * implementation of this method invokes <code>run</code> on each
   * nested <code>Suite</code> in the <code>List</code> obtained by invoking <code>nestedSuites</code>.
   * If a nested <code>Suite</code>'s <code>run</code>
   * method completes abruptly with an exception, this trait's implementation of this
   * method reports that the <code>Suite</code> aborted and attempts to run the
   * next nested <code>Suite</code>.
   * If the passed <code>distributor</code> is defined, this trait's implementation
   * puts each nested <code>Suite</code> 
   * into the <code>Distributor</code> contained in the <code>Some</code>, in the order in which the
   * <code>Suite</code>s appear in the <code>List</code> returned by <code>nestedSuites</code>, passing
   * in a new <code>Tracker</code> obtained by invoking <code>nextTracker</code> on the <code>Tracker</code>
   * passed to this method.
   * </p>
   *
   * <p>
   * Implementations of this method are responsible for ensuring <code>SuiteStarting</code> events
   * are fired to the <code>Reporter</code> before executing any nested <code>Suite</code>, and either <code>SuiteCompleted</code>
   * or <code>SuiteAborted</code> after executing any nested <code>Suite</code>.
   * </p>
   *
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   */
  protected def runNestedSuites(args: Args) {

    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)

    def callExecuteOnSuite(nestedSuite: Suite) {

      if (!stopRequested()) {

        // Create a Rerunner if the Suite has a no-arg constructor 
        val hasPublicNoArgConstructor = Suite.checkForPublicNoArgConstructor(nestedSuite.getClass)

        val rawString = Resources("suiteExecutionStarting")
        val formatter = formatterForSuiteStarting(nestedSuite)

        val suiteStartTime = System.currentTimeMillis

        report(SuiteStarting(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(nestedSuite.getClass.getName), nestedSuite.decodedSuiteName, formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))

        try { // TODO: pass runArgs down and that will get the chosenStyles passed down
          // Same thread, so OK to send same tracker
          nestedSuite.run(None, Args(report, stopRequested, filter, configMap, distributor, tracker, Set.empty))

          val rawString = Resources("suiteCompletedNormally")
          val formatter = formatterForSuiteCompleted(nestedSuite)

          val duration = System.currentTimeMillis - suiteStartTime
          report(SuiteCompleted(tracker.nextOrdinal(), nestedSuite.suiteName, nestedSuite.suiteId, Some(nestedSuite.getClass.getName), nestedSuite.decodedSuiteName, Some(duration), formatter, Some(TopOfClass(nestedSuite.getClass.getName)), nestedSuite.rerunner))
        }
        catch {       
          case e: RuntimeException => {
            val eMessage = e.getMessage
            val rawString = 
              if (eMessage != null && eMessage.length > 0)
                Resources("executeExceptionWithMessage", eMessage)
              else
                Resources("executeException")
            val formatter = formatterForSuiteAborted(nestedSuite, rawString)

            val duration = System.currentTimeMillis - suiteStartTime
            report(SuiteAborted(tracker.nextOrdinal(), rawString, nestedSuite.suiteName, nestedSuite.suiteId, Some(nestedSuite.getClass.getName), nestedSuite.decodedSuiteName, Some(e), Some(duration), formatter, Some(SeeStackDepthException), nestedSuite.rerunner))
          }
        }
      }
    }
    
    if (!filter.excludeNestedSuites) {
      val nestedSuitesArray = nestedSuites.toArray
      distributor match {
        case None =>
          for (nestedSuite <- nestedSuitesArray) {
            if (!stopRequested()) 
              callExecuteOnSuite(nestedSuite)
          }
        case Some(distribute) =>
          for (nestedSuite <- nestedSuitesArray) 
            distribute(nestedSuite, args.copy(tracker = tracker.nextTracker))
      }
    }
  }

  /**
   * A user-friendly suite name for this <code>Suite</code>.
   *
   * <p>
   * This trait's
   * implementation of this method returns the simple name of this object's class. This
   * trait's implementation of <code>runNestedSuites</code> calls this method to obtain a
   * name for <code>Report</code>s to pass to the <code>suiteStarting</code>, <code>suiteCompleted</code>,
   * and <code>suiteAborted</code> methods of the <code>Reporter</code>.
   * </p>
   *
   * @return this <code>Suite</code> object's suite name.
   */
  def suiteName = getSimpleNameOfAnObjectsClass(thisSuite)

  // Decoded suite name enclosed using backtick (`), currently for internal use only.
  private[scalatest] val decodedSuiteName:Option[String] = getDecodedName(suiteName)

  /**
   * A string ID for this <code>Suite</code> that is intended to be unique among all suites reported during a run.
   *
   * <p>
   * This trait's
   * implementation of this method returns the fully qualified name of this object's class. 
   * Each suite reported during a run will commonly be an instance of a different <code>Suite</code> class,
   * and in such cases, this default implementation of this method will suffice. However, in special cases
   * you may need to override this method to ensure it is unique for each reported suite. For example, if you write
   * a <code>Suite</code> subclass that reads in a file whose name is passed to its constructor and dynamically
   * creates a suite of tests based on the information in that file, you will likely need to override this method
   * in your <code>Suite</code> subclass, perhaps by appending the pathname of the file to the fully qualified class name. 
   * That way if you run a suite of tests based on a directory full of these files, you'll have unique suite IDs for
   * each reported suite.
   * </p>
   *
   * <p>
   * The suite ID is <em>intended</em> to be unique, because ScalaTest does not enforce that it is unique. If it is not
   * unique, then you may not be able to uniquely identify a particular test of a particular suite. This ability is used,
   * for example, to dynamically tag tests as having failed in the previous run when rerunning only failed tests.
   * </p>
   *
   * @return this <code>Suite</code> object's ID.
   */
  def suiteId = thisSuite.getClass.getName

  /**
   * Throws <code>TestPendingException</code> to indicate a test is pending.
   *
   * <p>
   * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
   * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
   * out before tests are written to verify that behavior (and often, the before the behavior of
   * the system being tested is itself implemented). Such sketches form a kind of specification of
   * what tests and functionality to implement later.
   * </p>
   *
   * <p>
   * To support this style of testing, a test can be given a name that specifies one
   * bit of behavior required by the system being tested. The test can also include some code that
   * sends more information about the behavior to the reporter when the tests run. At the end of the test,
   * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
   * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
   * sent to the reporter when running the test can appear in the report of a test run. (In other words,
   * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
   * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
   * the actual test, and possibly the functionality it is intended to test, has not yet been implemented.
   * </p>
   *
   * <p>
   * Note: This method always completes abruptly with a <code>TestPendingException</code>. Thus it always has a side
   * effect. Methods with side effects are usually invoked with parentheses, as in <code>pending()</code>. This
   * method is defined as a parameterless method, in flagrant contradiction to recommended Scala style, because it 
   * forms a kind of DSL for pending tests. It enables tests in suites such as <code>FunSuite</code> or <code>FunSpec</code>
   * to be denoted by placing "<code>(pending)</code>" after the test name, as in:
   * </p>
   *
   * <pre class="stHighlight">
   * test("that style rules are not laws") (pending)
   * </pre>
   *
   * <p>
   * Readers of the code see "pending" in parentheses, which looks like a little note attached to the test name to indicate
   * it is pending. Whereas "<code>(pending())</code> looks more like a method call, "<code>(pending)</code>" lets readers
   * stay at a higher level, forgetting how it is implemented and just focusing on the intent of the programmer who wrote the code.
   * </p>
   */
  def pending: PendingNothing = { throw new TestPendingException }

  /**
   * Execute the passed block of code, and if it completes abruptly, throw <code>TestPendingException</code>, else
   * throw <code>TestFailedException</code>.
   *
   * <p>
   * This method can be used to temporarily change a failing test into a pending test in such a way that it will
   * automatically turn back into a failing test once the problem originally causing the test to fail has been fixed.
   * At that point, you need only remove the <code>pendingUntilFixed</code> call. In other words, a
   * <code>pendingUntilFixed</code> surrounding a block of code that isn't broken is treated as a test failure.
   * The motivation for this behavior is to encourage people to remove <code>pendingUntilFixed</code> calls when
   * there are no longer needed.
   * </p>
   *
   * <p>
   * This method facilitates a style of testing in which tests are written before the code they test. Sometimes you may
   * encounter a test failure that requires more functionality than you want to tackle without writing more tests. In this
   * case you can mark the bit of test code causing the failure with <code>pendingUntilFixed</code>. You can then write more
   * tests and functionality that eventually will get your production code to a point where the original test won't fail anymore.
   * At this point the code block marked with <code>pendingUntilFixed</code> will no longer throw an exception (because the
   * problem has been fixed). This will in turn cause <code>pendingUntilFixed</code> to throw <code>TestFailedException</code>
   * with a detail message explaining you need to go back and remove the <code>pendingUntilFixed</code> call as the problem orginally
   * causing your test code to fail has been fixed.
   * </p>
   *
   * @param f a block of code, which if it completes abruptly, should trigger a <code>TestPendingException</code> 
   * @throws TestPendingException if the passed block of code completes abruptly with an <code>Exception</code> or <code>AssertionError</code>
   */
  def pendingUntilFixed(f: => Unit) {
    val isPending =
      try {
        f
        false
      }
      catch {
        case _: Exception => true
        case _: AssertionError => true
      }
      if (isPending)
        throw new TestPendingException
      else
        throw new TestFailedException(Resources("pendingUntilFixed"), 2)
  }

  /**
   * The total number of tests that are expected to run when this <code>Suite</code>'s <code>run</code> method is invoked.
   *
   * <p>
   * This trait's implementation of this method returns the sum of:
   * </p>
   *
   * <ul>
   * <li>the size of the <code>testNames</code> <code>List</code>, minus the number of tests marked as ignored and
   * any tests that are exluded by the passed <code>Filter</code></li>
   * <li>the sum of the values obtained by invoking
   *     <code>expectedTestCount</code> on every nested <code>Suite</code> contained in
   *     <code>nestedSuites</code></li>
   * </ul>
   *
   * @param filter a <code>Filter</code> with which to filter tests to count based on their tags
   */
  def expectedTestCount(filter: Filter): Int = {

    // [bv: here was another tricky refactor. How to increment a counter in a loop]
    def countNestedSuiteTests(nestedSuites: List[Suite], filter: Filter): Int =
      nestedSuites.toList match {
        case List() => 0
        case nestedSuite :: nestedSuites => 
          nestedSuite.expectedTestCount(filter) + countNestedSuiteTests(nestedSuites, filter)
    }

    filter.runnableTestCount(testNames, tags, suiteId) + countNestedSuiteTests(nestedSuites.toList, filter)
  }

  // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
  // so that exceptions are caught and transformed
  // into error messages on the standard error stream.
  private[scalatest] def wrapReporterIfNecessary(reporter: Reporter) = reporter match {
    case dr: DispatchReporter => dr
    case cr: CatchReporter => cr
    case _ => new CatchReporter(reporter)
  }
  
  /**
   * The fully qualified class name of the rerunner to rerun this suite.  This implementation will look at this.getClass and see if it is
   * either an accessible Suite, or it has a WrapWith annotation. If so, it returns the fully qualified class name wrapped in a Some, 
   * or else it returns None.
   */
  def rerunner: Option[String] = {
    val suiteClass = getClass
    val isAccessible = SuiteDiscoveryHelper.isAccessibleSuite(suiteClass)
    val hasWrapWithAnnotation = suiteClass.getAnnotation(classOf[WrapWith]) != null
    if (isAccessible || hasWrapWithAnnotation)
      Some(suiteClass.getName)
    else
      None
  }
  
  private[scalatest] def getTopOfClass = TopOfClass(this.getClass.getName)
  private[scalatest] def getTopOfMethod(method:Method) = TopOfMethod(this.getClass.getName, method.toGenericString())
  private[scalatest] def getTopOfMethod(testName:String) = TopOfMethod(this.getClass.getName, getMethodForTestName(testName).toGenericString())
  
  /**
   * Suite style name.
   */
  val styleName: String = "org.scalatest.Suite"
}

private[scalatest] object Suite {

  private[scalatest] val TestMethodPrefix = "test"
  private[scalatest] val InformerInParens = "(Informer)"
  private[scalatest] val IgnoreAnnotation = "org.scalatest.Ignore"

  private[scalatest] def getSimpleNameOfAnObjectsClass(o: AnyRef) = stripDollars(parseSimpleName(o.getClass.getName))

  // [bv: this is a good example of the expression type refactor. I moved this from SuiteClassNameListCellRenderer]
  // this will be needed by the GUI classes, etc.
  private[scalatest] def parseSimpleName(fullyQualifiedName: String) = {

    val dotPos = fullyQualifiedName.lastIndexOf('.')

    // [bv: need to check the dotPos != fullyQualifiedName.length]
    if (dotPos != -1 && dotPos != fullyQualifiedName.length)
      fullyQualifiedName.substring(dotPos + 1)
    else
      fullyQualifiedName
  }
  
  private[scalatest] def checkForPublicNoArgConstructor(clazz: java.lang.Class[_]) = {
    
    try {
      val constructor = clazz.getConstructor(new Array[java.lang.Class[T] forSome { type T }](0): _*)

      Modifier.isPublic(constructor.getModifiers)
    }
    catch {
      case nsme: NoSuchMethodException => false
    }
  }

  // This attempts to strip dollar signs that happen when using the interpretter. It is quite fragile
  // and already broke once. In the early days, all funky dollar sign encrusted names coming out of
  // the interpreter started with "line". Now they don't, but in both cases they seemed to have at
  // least one "$iw$" in them. So now I leave the string alone unless I see a "$iw$" in it. Worst case
  // is sometimes people will get ugly strings coming out of the interpreter. -bv April 3, 2012
  private[scalatest] def stripDollars(s: String): String = {
    val lastDollarIndex = s.lastIndexOf('$')
    if (lastDollarIndex < s.length - 1)
      if (lastDollarIndex == -1 || !s.contains("$iw$")) s else s.substring(lastDollarIndex + 1)
    else {
      // The last char is a dollar sign
      val lastNonDollarChar = s.reverse.find(_ != '$')
      lastNonDollarChar match {
        case None => s
        case Some(c) => {
          val lastNonDollarIndex = s.lastIndexOf(c)
          if (lastNonDollarIndex == -1) s
          else stripDollars(s.substring(0, lastNonDollarIndex + 1))
        }
      }
    }
  }
  
  private[scalatest] def diffStrings(s: String, t: String): Tuple2[String, String] = {
    def findCommonPrefixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the prefix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(i) != t.charAt(i))
        if (!found)
          i = i + 1
      }
      i
    }
    def findCommonSuffixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the suffix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
        if (!found)
          i = i + 1
      }
      i
    }
    val commonPrefixLength = findCommonPrefixLength(s, t)
    val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
    val prefix = s.substring(0, commonPrefixLength)
    val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
    val sMiddleEnd = s.length - commonSuffixLength
    val tMiddleEnd = t.length - commonSuffixLength
    val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
    val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
    val MaxContext = 20
    val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
    val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
    (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
  }
  
  // If the objects are two strings, replace them with whatever is returned by diffStrings.
  // Otherwise, use the same objects.
  private[scalatest] def getObjectsForFailureMessage(a: Any, b: Any) = 
    a match {
      case aStr: String => {
        b match {
          case bStr: String => {
            Suite.diffStrings(aStr, bStr)    
          }
          case _ => (a, b)
        }
      } 
      case _ => (a, b)
    }

  private[scalatest] def formatterForSuiteStarting(suite: Suite): Option[Formatter] =
      Some(IndentedText(suite.suiteName + ":", suite.suiteName, 0))

  private[scalatest] def formatterForSuiteCompleted(suite: Suite): Option[Formatter] =
      Some(MotionToSuppress)

  private[scalatest] def formatterForSuiteAborted(suite: Suite, message: String): Option[Formatter] =
      Some(IndentedText(message, message, 0))

  private def simpleNameForTest(testName: String) =
    if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName

  private[scalatest] def anErrorThatShouldCauseAnAbort(throwable: Throwable) =
    throwable match {
      case _: AnnotationFormatError | 
           _: CoderMalfunctionError |
           _: FactoryConfigurationError | 
           _: LinkageError | 
           _: ThreadDeath | 
           _: TransformerFactoryConfigurationError | 
           _: VirtualMachineError => true
      // Don't use AWTError directly because it doesn't exist on Android, and a user
      // got ScalaTest to compile under Android.
      case e if e.getClass.getName == "java.awt.AWTError" => true
      case _ => false
    }

  def takesInformer(m: Method) = {
    val paramTypes = m.getParameterTypes
    paramTypes.length == 1 && classOf[Informer].isAssignableFrom(paramTypes(0))
  }

  def takesCommunicator(m: Method) = {
    val paramTypes = m.getParameterTypes
    paramTypes.length == 1 && classOf[Rep].isAssignableFrom(paramTypes(0))
  }

  def isTestMethodGoodies(m: Method) = {

    val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

    // name must have at least 4 chars (minimum is "test")
    val simpleName = m.getName
    val firstFour = if (simpleName.length >= 4) simpleName.substring(0, 4) else "" 

    val paramTypes = m.getParameterTypes
    val hasNoParams = paramTypes.length == 0

    // Discover testNames(Informer) because if we didn't it might be confusing when someone
    // actually wrote a testNames(Informer) method and it was silently ignored.
    val isTestNames = simpleName == "testNames"
    val isTestTags = simpleName == "testTags"

    (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags)
  }

  def testMethodTakesAnInformer(testName: String): Boolean = testName.endsWith(InformerInParens)

  /*
   For info and test names, the formatted text should have one level shaved off so that the text will
   line up correctly, and the icon is over to the left of that even with the enclosing level.

   If a test is at the top level (not nested inside a describe, it's level is 0. So no need to subtract 1
   to make room for the icon in that case. An info inside such a test will have level 1. And agin, in that
   case no need to subtract 1. Such a test is "outermost test" and the info inside is "in outermost test" in:

class ArghSpec extends Spec with GivenWhenThen {
  info("in ArghSpec")
  it("outermost test") {
    info("in outermost test")
  }
  describe("Apple") {
    info("in Apple")
    describe("Boat") {
      info("in Boat")
      describe("Cat") {
        info("in Cat")
        describe("Dog") {
          info("in Dog")
          describe("Elephant") {
            info("in Elephant")
            it("Factory") {
              info("in Factory (test)")
              given("an empty Stack")
              when("push is invoked")
              then("it should have size 1")
              and("pop should return the pushed value")
            }
          }
        }
      }
    }
  }
}

It should (and at this point does) output this:

[scalatest] ArghSpec:
[scalatest] + in ArghSpec 
[scalatest] - outermost test (5 milliseconds)
[scalatest]   + in outermost test 
[scalatest] Apple 
[scalatest] + in Apple 
[scalatest]   Boat 
[scalatest]   + in Boat 
[scalatest]     Cat 
[scalatest]     + in Cat 
[scalatest]       Dog 
[scalatest]       + in Dog 
[scalatest]         Elephant 
[scalatest]         + in Elephant 
[scalatest]         - Factory (1 millisecond)
[scalatest]           + in Factory (test) 
[scalatest]           + Given an empty Stack 
[scalatest]           + When push is invoked 
[scalatest]           + Then it should have size 1 
[scalatest]           + And pop should return the pushed value 

FeatureSpec doesn't want any icons printed out. So adding includeIcon here. It
was already in getIndentedTextForInfo because of descriptions being printed out
without icons.

This should really be named getIndentedTextForTest maybe, because I think it is just
used for test events like succeeded/failed, etc.
  */
  def getIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = NameTransformer.decode(testText)
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = Resources("testSucceededIconChar")
        ("  " * (if (level == 0) 0 else (level - 1))) + Resources("iconPlusShortName", testSucceededIcon, decodedTestText)
      }
      else {
        ("  " * level) + decodedTestText
      }
    IndentedText(formattedText, decodedTestText, level)
  }
  
  def getEscapedIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = NameTransformer.decode(testText)
    val escapedTestText = 
      if (decodedTestText.startsWith("test: "))
        decodedTestText.drop(6)
      else
        decodedTestText
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = Resources("testSucceededIconChar")
        ("  " * (if (level == 0) 0 else (level - 1))) + Resources("iconPlusShortName", testSucceededIcon, escapedTestText)
      }
      else {
        ("  " * level) + escapedTestText
      }
    IndentedText(formattedText, decodedTestText, level)
  }

  // The icon is not included for branch description text, but is included for things sent via info(), given(),
  // when(), then(), etc. When it is included, reduce the level by 1, unless it is already 1 or 0.
  def getIndentedTextForInfo(message: String, level: Int, includeIcon: Boolean, infoIsInsideATest: Boolean) = {
    val formattedText =
      if (includeIcon) {
        val infoProvidedIcon = Resources("infoProvidedIconChar")
        //
        // Inside a test, you want level 1 to stay 1
        // [scalatest] - outermost test (5 milliseconds)
        // [scalatest]   + in outermost test
        //
        // But outside a test, level 1 should be transformed to 0
        // [scalatest] Apple
        // [scalatest] + in Apple
        //
        val indentationLevel =
          level match {
            case 0 => 0
            case 1 if infoIsInsideATest => 1
            case _ => level - 1
          }
        ("  " * indentationLevel) + Resources("iconPlusShortName", infoProvidedIcon, message)
        // ("  " * (if (level <= 1) level else (level - 1))) + Resources("iconPlusShortName", infoProvidedIcon, message)
      }
      else {
        ("  " * level) + message
      }
    IndentedText(formattedText, message, level)
  }

  def getMessageForException(e: Throwable): String =
    if (e.getMessage != null)
      e.getMessage
    else
      Resources("exceptionThrown", e.getClass.getName) // Say something like, "java.lang.Exception was thrown."

  def indentation(level: Int) = "  " * level
  
  // Decode suite name enclosed using backtick (`)
  def getDecodedName(name:String): Option[String] = {
    val decoded = NameTransformer.decode(name)
    if(decoded == name) None else Some(decoded)
  }

  def reportTestFailed(theSuite: Suite, report: Reporter, throwable: Throwable, testName: String, testText: String,
      decodedTestName:Option[String], recordedEvents: IndexedSeq[RecordableEvent], rerunnable: Option[String], tracker: Tracker, duration: Long, formatter: Formatter, location: Option[Location]) {

    val message = getMessageForException(throwable)
    //val formatter = getEscapedIndentedTextForTest(testText, level, includeIcon)
    val payload = 
      throwable match {
        case optPayload: PayloadField => 
          optPayload.payload
        case _ => 
          None
      }
    report(TestFailed(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName),theSuite.decodedSuiteName, testName, testText, decodedTestName, recordedEvents, Some(throwable), Some(duration), Some(formatter), location, theSuite.rerunner, payload))
  }

  // TODO: Possibly separate these out from method tests and function tests, because locations are different
  // Update: Doesn't seems to need separation, to be confirmed with Bill.
  def reportTestStarting(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, decodedTestName:Option[String], rerunnable: Option[String], location: Option[Location]) {
    report(TestStarting(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, testName, testText, decodedTestName, Some(MotionToSuppress),
      location, rerunnable))
  }

  def reportTestPending(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, decodedTestName:Option[String], recordedEvents: IndexedSeq[RecordableEvent], duration: Long, formatter: Formatter, location: Option[Location]) {
    report(TestPending(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, testName, testText, decodedTestName, recordedEvents, Some(duration), Some(formatter),
      location))
  }

/*
  def reportTestCanceled(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, duration: Long, formatter: Formatter, location: Option[Location]) {
    val message = getMessageForException(throwable)
    report(TestCanceled(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteID, Some(theSuite.getClass.getName), testName, Some(duration), Some(formatter),
      location))
  }
*/

  def reportTestCanceled(theSuite: Suite, report: Reporter, throwable: Throwable, testName: String, testText: String,
      decodedTestName:Option[String], recordedEvents: IndexedSeq[RecordableEvent], rerunnable: Option[String], tracker: Tracker, duration: Long, formatter: Formatter, location: Option[Location]) {

    val message = getMessageForException(throwable)
    //val formatter = getEscapedIndentedTextForTest(testText, level, includeIcon)
    report(TestCanceled(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, testName, testText, decodedTestName, recordedEvents, Some(throwable), Some(duration), Some(formatter), location, rerunnable))
  }

  def reportTestSucceeded(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, decodedTestName:Option[String], recordedEvents: IndexedSeq[RecordableEvent], duration: Long, formatter: Formatter, rerunnable: Option[String], location: Option[Location]) {
    report(TestSucceeded(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, testName, testText, decodedTestName, recordedEvents, Some(duration), Some(formatter),
      location, rerunnable))
  }

  def reportTestIgnored(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, decodedTestName:Option[String], formatter: Formatter, location: Option[Location]) {
    val testSucceededIcon = Resources("testSucceededIconChar")
    report(TestIgnored(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, testName, testText, decodedTestName, Some(formatter),
      location))
  }
  
  def createInfoProvided(theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true,
    aboutAPendingTest: Option[Boolean] = None,
    aboutACanceledTest: Option[Boolean] = None) = {
    InfoProvided(
        tracker.nextOrdinal(),
        message,
        if (includeNameInfo)
          Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, 
               testName match {
                 case Some(name) => Some(TestNameInfo(name, getDecodedName(name)))
                 case None => None
               }
          ))
        else
          None,
        aboutAPendingTest,
        aboutACanceledTest,
        None,
        Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
        location,
        payload
      )
  }

  // If not fired in the context of a test, then testName will be None
  def reportInfoProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    payload: Option[Any], 
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    includeIcon: Boolean = true,
    aboutAPendingTest: Option[Boolean] = None,
    aboutACanceledTest: Option[Boolean] = None
  ) {
    report(
      createInfoProvided(
        theSuite,
        report,
        tracker,
        testName,
        message,
        payload, 
        level,
        location,
        includeNameInfo,
        includeIcon,
        aboutAPendingTest,
        aboutACanceledTest
      )
    )
  }
  
  def createMarkupProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    aboutAPendingTest: Option[Boolean] = None,
    aboutACanceledTest: Option[Boolean] = None  
  ) = {
    MarkupProvided(
      tracker.nextOrdinal(),
      message,
      if (includeNameInfo)
        Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, 
            testName match {
              case Some(name) => Some(TestNameInfo(name, getDecodedName(name)))
              case None => None
            }
          ))
      else
        None,
      aboutAPendingTest,
      aboutACanceledTest,
      None, // Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined))  for now don't send a formatter
      location
    )
  }
  

  // If not fired in the context of a test, then testName will be None
  def reportMarkupProvided(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    level: Int,
    location: Option[Location],
    includeNameInfo: Boolean,
    aboutAPendingTest: Option[Boolean] = None,
    aboutACanceledTest: Option[Boolean] = None
  ) {
    report(
      createMarkupProvided(
        theSuite,
        report,
        tracker,
        testName,
        message,
        level,
        location,
        includeNameInfo,
        aboutAPendingTest,
        aboutACanceledTest
      )
    )
  }

  // If not fired in the context of a test, then testName will be None
  def reportScopeOpened(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    level: Int,
    includeIcon: Boolean = true,
    aboutAPendingTest: Option[Boolean] = None,
    aboutACanceledTest: Option[Boolean] = None, 
    location: Option[Location]
  ) {
    report(
      ScopeOpened(
        tracker.nextOrdinal(),
        message,
        NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, 
                 testName match {
                   case Some(name) => Some(TestNameInfo(name, getDecodedName(name)))
                   case None => None
                 }
          ),
        aboutAPendingTest,
        aboutACanceledTest,
        Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)), 
        location
      )
    )
  }

  // If not fired in the context of a test, then testName will be None
  def reportScopeClosed(
    theSuite: Suite,
    report: Reporter,
    tracker: Tracker,
    testName: Option[String],
    message: String,
    level: Int,
    includeIcon: Boolean = true,
    aboutAPendingTest: Option[Boolean] = None,
    aboutACanceledTest: Option[Boolean] = None,
    location: Option[Location]
  ) {
    report(
      ScopeClosed(
        tracker.nextOrdinal(),
        message,
        NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), theSuite.decodedSuiteName, 
                 testName match {
                   case Some(name) => Some(TestNameInfo(name, getDecodedName(name)))
                   case None => None
                 }
          ),
        aboutAPendingTest,
        aboutACanceledTest,
        Some(MotionToSuppress),
        location
      )
    )
  }
  
  /*def getLineInFile(stackTraceList:List[StackTraceElement], sourceFileName:String, methodName: String):Option[LineInFile] = {
    val baseStackDepth = stackTraceList.takeWhile(stackTraceElement => sourceFileName != stackTraceElement.getFileName || stackTraceElement.getMethodName != methodName).length
    val stackTraceOpt = stackTraceList.drop(baseStackDepth).find(stackTraceElement => stackTraceElement.getMethodName() == "<init>")
    stackTraceOpt match {
      case Some(stackTrace) => Some(LineInFile(stackTrace.getLineNumber, stackTrace.getFileName))
      case None => None
    }
  }*/
  
  def getLineInFile(stackTraceList: Array[StackTraceElement], stackDepth: Int) = {
    if(stackDepth >= 0 && stackDepth < stackTraceList.length) {
      val stackTrace = stackTraceList(stackDepth)
      if(stackTrace.getLineNumber >= 0 && stackTrace.getFileName != null)
        Some(LineInFile(stackTrace.getLineNumber, stackTrace.getFileName))
      else
        None
    }
    else
      None
  }

  def checkChosenStyles(configMap: Map[String, Any], styleName: String) {
    val chosenStyleSet = 
        if (configMap.isDefinedAt("org.scalatest.ChosenStyles"))
          configMap("org.scalatest.ChosenStyles").asInstanceOf[Set[String]]
        else
          Set.empty[String]
    
    if (chosenStyleSet.size > 0 && !chosenStyleSet.contains(styleName)) {
      val e =
        if (chosenStyleSet.size == 1)
          new NotAllowedException(Resources("notTheChosenStyle", styleName, chosenStyleSet.head), getStackDepthFun("Scala.scala", "checkChosenStyles"))
        else
          new NotAllowedException(Resources("notOneOfTheChosenStyles", styleName, Suite.makeListForHumans(Vector.empty ++ chosenStyleSet.iterator)), getStackDepthFun("Scala.scala", "checkChosenStyles"))
      throw e
    }
  }

  // If it contains a space, or is an empty string, put quotes around it. OTherwise people might
  // get confused by the chosenStyles error message.
  def makeListForHumans(words: Vector[String]): String = {
    val quotedWords = words map { w =>
      if (w.length == 0 || w.indexOf(' ') >= 0) "\"" + w + "\""
      else w
    }
    quotedWords.length match {
      case 0 => "<empty list>"
      //case 1 if quotedWords(0).isEmpty => "\"\""
      case 1 => quotedWords(0)
      case 2 => Resources("leftAndRight", quotedWords(0), quotedWords(1))
      case _ =>
        val (leading, trailing) = quotedWords.splitAt(quotedWords.length - 2)
        leading.mkString(", ") + ", " + Resources("leftCommaAndRight", trailing(0), trailing(1))
    }
  }
}

