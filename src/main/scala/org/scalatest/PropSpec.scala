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
package org.scalatest

import scala.collection.immutable.ListSet

/**
 * A suite of property-based tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>PropSpec</code> is a good fit for teams that want to write tests exclusively in terms of property checks, and is also a good choice
 * for writing the occasional test matrix when a different style trait is chosen as the main unit testing style.
 * </td></tr></table>
 * 
 * Here's an example <code>PropSpec</code>:
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec
 * 
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 *   
 *   property("an empty Set should have size 0") {
 *     forAll(examples) { set =>
 *       set.size should be (0)
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     forAll(examples) { set =>
 *       evaluating { set.head } should produce [NoSuchElementException]
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * You can run a <code>PropSpec</code> by invoking <code>execute</code> on it.
 * This method, which prints test results to the standard output, is intended to serve as a
 * convenient way to run tests from within the Scala interpreter. For example,
 * to run <code>SetSpec</code> from within the Scala interpreter, you could write:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSpec execute
 * </pre>
 *
 * <p>
 * And you would see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSpec:
 * - an empty Set should have size 0
 * - invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 *
 * <p>
 * Or, to run just the &ldquo;<code>an empty Set should have size 0</code>&rdquo; method, you could pass that test's name, or any unique substring of the
 * name, such as <code>"size 0"</code> or even just <code>"0"</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSpec execute "size 0"
 * <span class="stGreen">SetSpec:
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
 * &#8220;<code>test</code>&#8221; is a method, defined in <code>FunSuite</code>, which will be invoked
 * by the primary constructor of <code>SetSuite</code>. You specify the name of the test as
 * a string between the parentheses, and the test code itself between curly braces.
 * The test code is a function passed as a by-name parameter to <code>test</code>, which registers
 * it for later execution. One benefit of <code>FunSuite</code> compared to <code>Suite</code> is you need not name all your
 * tests starting with &#8220;<code>test: </code>.&#8221;
 * </p>
 *
 * <p>
 * A <code>FunSuite</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered with the <code>test</code> method while the <code>FunSuite</code> is
 * in its registration phase. Any attempt to register a test after the <code>FunSuite</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>FunSuite</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>FunSuite</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <p>
 * See also: <a href="http://www.scalatest.org/getting_started_with_fun_suite" target="_blank">Getting started with <code>FunSuite</code>.</a>
 * </p>
 * 
 * <h2>Ignored tests</h2>
 *
 * <p>
 * To support the common use case of &#8220;temporarily&#8221; disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>FunSuite</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>test</code>. For example, to temporarily
 * disable the test named <code>Addition</code>, just change &#8220;<code>test</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.ignore
 * 
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 * 
 *   ignore("an empty Set should have size 0") {
 *     forAll(examples) { set =>
 *       set.size should be (0)
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     forAll(examples) { set =>
 *       evaluating { set.head } should produce [NoSuchElementException]
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
 * scala> new SetSpec execute
 * </pre>
 *
 * <p>
 * It will run only the second test and report that the first test was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:</span>
 * <span class="stYellow">- an empty Set should have size 0 !!! IGNORED !!!</span>
 * <span class="stGreen">- invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 *
 * <h2>Informers</h2>
 *
 * <p>
 * One of the parameters to the <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>FunSuite</code>'s methods will be sufficient, but
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
 * package org.scalatest.examples.propspec.info
 * 
 * import org.scalatest._
 * import prop._
 * import collection.mutable
 * 
 * class SetSuite extends PropSpec with TableDrivenPropertyChecks with GivenWhenThen {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       mutable.BitSet.empty,
 *       mutable.HashSet.empty[Int],
 *       mutable.LinkedHashSet.empty[Int]
 *     )
 * 
 *   property("an element can be added to an empty mutable Set") {
 * 
 *     forAll(examples) { set =&gt;
 * 
 *       info("----------------")
 * 
 *       given("an empty mutable " + set.getClass.getSimpleName)
 *       assert(set.isEmpty)
 * 
 *       when("an element is added")
 *       set += 99
 * 
 *       then("the Set should have size 1")
 *       assert(set.size === 1)
 * 
 *       and("the Set should contain the added element")
 *       assert(set.contains(99))
 *     }
 *   }
 * }
 * </pre>
 *
 *
 * If you run this <code>FunSuite</code> from the interpreter, you will see the following output:
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * <span class="stGreen">SetSuite:
 * - an element can be added to an empty mutable Set
 *   + ---------------- 
 *   + Given an empty mutable BitSet 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element 
 *   + ---------------- 
 *   + Given an empty mutable HashSet 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element 
 *   + ---------------- 
 *   + Given an empty mutable LinkedHashSet 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element</span>
 * </pre>
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
 * the actual test, and possibly the functionality, has not yet been implemented.
 * </p>
 *
 * <p>
 * Although pending tests may be used more often in specification-style suites, such as
 * <code>org.scalatest.FunSpec</code>, you can also use it in <code>FunSuite</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 * 
 *   property("an empty Set should have size 0") (pending)
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     forAll(examples) { set =&gt;
 *       evaluating { set.head } should produce [NoSuchElementException]
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>SetSuite</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> new SetSuite execute
 * </pre>
 *
 * <p>
 * It will run both tests, but report that first test is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">SetSuite:</span>
 * <span class="stYellow">- An empty Set should have size 0 (pending)</span>
 * <span class="stGreen">- Invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 * 
 * <a name="taggingTests"></a><h2>Tagging tests</h2>
 *
 * <p>
 * A <code>FunSuite</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>FunSuite</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>FunSuite</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to methods
 * that register tests, <code>test</code> and <code>ignore</code>. Class <code>Tag</code> takes one parameter, a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>FunSuite</code>s that match. To do so, simply 
 * pass the fully qualified names of the Java interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.tags.SlowTest</code> and
 * <code>com.mycompany.tags.DbTest</code>, then you could
 * create matching groups for <code>FunSuite</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.tagging
 *
 * import org.scalatest.Tag
 *
 * object SlowTest extends Tag("com.mycompany.tags.SlowTest")
 * object DbTest extends Tag("com.mycompany.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>FunSuite</code> tests into groups like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import prop._
 * import scala.collection.immutable._
 * 
 * class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {
 * 
 *   val examples =
 *     Table(
 *       "set",
 *       BitSet.empty,
 *       HashSet.empty[Int],
 *       TreeSet.empty[Int]
 *     )
 * 
 *   property("an empty Set should have size 0", SlowTest) {
 *     forAll(examples) { set =&gt;
 *       set.size should be (0)
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException",
 *       SlowTest, DbTest) {
 * 
 *     forAll(examples) { set =&gt;
 *       evaluating { set.head } should produce [NoSuchElementException]
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests, "Addition" and "Subtraction," with the <code>com.mycompany.tags.SlowTest</code> tag, 
 * and test "Subtraction" with the <code>com.mycompany.tags.DbTest</code> tag.
 * </p>
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
 * <h4>Using <code>PropSpec</code> to write a test matrix</h4>
 *
 * <p>
 * If you need to create the same mutable fixture objects in multiple tests, and don't need to clean them up after using them, the simplest approach is to write one or
 * more <em>get-fixture</em> methods. A get-fixture method returns a new instance of a needed fixture object (or an holder object containing
 * multiple fixture objects) each time it is called. You can call a get-fixture method at the beginning of each
 * test that needs the fixture, storing the returned object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.propspec.oneargtest
 * 
 * import org.scalatest._
 * import org.scalatest.prop._
 * import scala.collection.immutable._
 * 
 * trait SetExamples extends Tables {
 *   def examples =
 *     Table(
 *       "set",
 *       bitSet,
 *       hashSet,
 *       treeSet
 *     )
 * 
 *   def bitSet: BitSet
 *   def hashSet: HashSet[Int]
 *   def treeSet: TreeSet[Int]
 * }
 * 
 * class EmptySetExamples extends SetExamples {
 *   def bitSet = BitSet.empty
 *   def hashSet = HashSet.empty[Int]
 *   def treeSet = TreeSet.empty[Int]
 * }
 * 
 * class SetSpec extends PropSpec with TableDrivenPropertyChecks with ShouldMatchers {
 * 
 *   property("an empty Set should have size 0") {
 *     new EmptySetExamples {
 *       forAll(examples) { set =>
 *         set.size should be (0)
 *       }
 *     }
 *   }
 * 
 *   property("invoking head on an empty set should produce NoSuchElementException") {
 *     new EmptySetExamples {
 *       forAll(examples) { set =>
 *         evaluating { set.head } should produce [NoSuchElementException]
 *       }
 *     }
 *   }
 * }
 * </pre>
 * 
 * <h2>YE OLDE STUFF</h2>
 * <p>
 * This trait facilitates a style of testing in which each test is composed
 * of one property check. Tests are registered via a "<code>property</code>" method, and given a name and a body.
 * (A <code>PropSpec</code> behaves just like a <code>FunSuite</code>, except <code>test</code> is replaced with
 * <code>property</code>.) You can do anything in the body of the test, but the intention is that you'd check
 * one property in each test. To write properties in the ScalaCheck style, mix <code>Checkers</code> into
 * your <code>PropSpec</code>. To write them in the ScalaTest style, mix in <code>PropertyChecks</code>.
 * </p>
 *
 * <p>
 * For example, given this <code>Fraction</code> class:
 * </p>
 *
 * <pre class="stHighlight">
 * class Fraction(n: Int, d: Int) {
 *   require(d != 0)
 *   require(d != Integer.MIN_VALUE)
 *   require(n != Integer.MIN_VALUE)
 *
 *   val numer = if (d < 0) -1 * n else n
 *   val denom = d.abs
 *
 *   override def toString = numer + " / " + denom
 * }
 * </pre>
 *
 * <p>
 * You could write a <code>PropSpec</code> in the ScalaTest property style that specifies the <code>Fraction</code> behavior like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.PropSpec
 * import org.scalatest.prop.PropertyChecks
 * import org.scalatest.matchers.ShouldMatchers
 *
 * class FractionSpec extends PropSpec with PropertyChecks with ShouldMatchers {
 *
 *   property("Fraction constructor normalizes numerator and denominator") {
 *
 *     forAll { (n: Int, d: Int) =>
 *       whenever (d != 0 && d != Integer.MIN_VALUE
 *           && n != Integer.MIN_VALUE) {
 *
 *         val f = new Fraction(n, d)
 *
 *         if (n < 0 && d < 0 || n > 0 && d > 0)
 *           f.numer should be > 0
 *         else if (n != 0)
 *           f.numer should be < 0
 *         else
 *           f.numer should be === 0
 *
 *         f.denom should be > 0
 *       }
 *     }
 *   }
 *
 *   property("Fraction constructor throws IAE on bad data.") {
 *
 *     val invalidCombos =
 *       Table(
 *         ("n",               "d"),
 *         (Integer.MIN_VALUE, Integer.MIN_VALUE),
 *         (1,                 Integer.MIN_VALUE),
 *         (Integer.MIN_VALUE, 1),
 *         (Integer.MIN_VALUE, 0),
 *         (1,                 0)
 *       )
 *
 *     forAll (invalidCombos) { (n: Int, d: Int) =>
 *       evaluating {
 *         new Fraction(n, d)
 *       } should produce [IllegalArgumentException]
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * &#8220;<code>property</code>&#8221; is a method, defined in <code>PropSpec</code>, which will be invoked
 * by the primary constructor of <code>MathSpec</code>. You specify the name of the property as
 * a string between the parentheses, and the test code containing the property check between curly braces.
 * The test code is a function passed as a by-name parameter to <code>property</code>, which registers
 * it for later execution.
 * </p>
 *
 * <p>
 * A <code>PropSpec</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Properties can only be registered with the <code>property</code> method while the <code>PropSpec</code> is
 * in its registration phase. Any attempt to register a property after the <code>PropSpec</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>PropSpec</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>PropSpec</code> is to register properties during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <p>
 * <em>Note: Trait <code>PropSpec</code> is in part inspired by class <code>org.scalacheck.Properties</code>, designed by
 * Rickard Nilsson for the <a href="http://code.google.com/p/scalacheck/">ScalaCheck test framework</a>.</em>
 * </p>
 *
 * <a name="ignoredTests"></a><h2>Ignored tests</h2></a>
 *
 * <p>
 * To support the common use case of &#8220;temporarily&#8221; disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>PropSpec</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>property</code>. For example, to temporarily
 * disable the test named <code>addition</code>, just change &#8220;<code>property</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.PropSpec
 * import org.scalatest.prop.PropertyChecks
 * import org.scalatest.matchers.ShouldMatchers
 *
 * class MathSpec extends PropSpec with PropertyChecks with ShouldMatchers {
 *
 *   ignore("addition", SlowTest) {
 *     forAll { (i: Int) => i + i should equal (2 * i) }
 *   }
 *
 *   property("subtraction", SlowTest, DbTest) {
 *     forAll { (i: Int) => i - i should equal (0) }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>MathSpec</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new MathSpec).execute()
 * </pre>
 *
 * <p>
 * It will run only <code>subtraction</code> and report that <code>addition</code> was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">MathSpec:</span>
 * <span class="stYellow">- addition !!! IGNORED !!!</span>
 * <span class="stGreen">- subtraction</span>
 * </pre>
 *
 * <h2>Informers</h2>
 *
 * <p>
 * One of the parameters to the <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>PropSpec</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <code>Informer</code> that will forward information to the current <code>Reporter</code>
 * is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via one of its <code>apply</code> methods.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.PropSpec
 * import org.scalatest.prop.PropertyChecks
 * import org.scalatest.matchers.ShouldMatchers
 *
 * class MathSpec extends PropSpec with PropertyChecks with ShouldMatchers {
 *
 *   property("addition", SlowTest) {
 *     forAll { (i: Int) => i + i should equal (2 * i) }
 *     info("Addition seems to work")
 *   }
 *
 *   property("subtraction", SlowTest, DbTest) {
 *     forAll { (i: Int) => i - i should equal (0) }
 *   }
 * }
 * </pre>
 *
 * If you run this <code>PropSpec</code> from the interpreter, you will see the following message
 * included in the printed report:
 *
 * <pre class="stREPL">
 * <span class="stGreen">MathSpec:
 * - addition
 *   + Addition seems to work</span> 
 * </pre>
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
 * the actual test, and possibly the functionality, has not yet been implemented.
 * </p>
 *
 * <p>
 * Although pending tests may be used more often in specification-style suites, such as
 * <code>org.scalatest.FunSpec</code>, you can also use it in <code>PropSpec</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.PropSpec
 * import org.scalatest.prop.PropertyChecks
 * import org.scalatest.matchers.ShouldMatchers
 *
 * class MathSpec extends PropSpec with PropertyChecks with ShouldMatchers {
 *
 *   ignore("addition") {
 *     forAll { (i: Int) => i + i should equal (2 * i) }
 *   }
 *
 *   property("subtraction") (pending)
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>MathSpec</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new MathSpec).execute()
 * </pre>
 *
 * <p>
 * It will run both tests, but report that <code>subtraction</code> is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">MathSpec:
 * - addition</span>
 * <span class="stYellow">- subtraction (pending)</span>
 * </pre>
 *
 * <h2>Tagging tests</h2>
 *
 * <p>
 * A <code>PropSpec</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>PropSpec</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>PropSpec</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to methods
 * that register tests, <code>test</code> and <code>ignore</code>. Class <code>Tag</code> takes one parameter, a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>PropSpec</code>s that match. To do so, simply 
 * pass the fully qualified names of the Java interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.tags.SlowTest</code> and
 * <code>com.mycompany.tags.DbTest</code>, then you could
 * create matching groups for <code>PropSpec</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.Tag
 *
 * object SlowTest extends Tag("com.mycompany.tags.SlowTest")
 * object DbTest extends Tag("com.mycompany.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could tag a <code>PropSpec</code>'s tests like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.PropSpec
 * import org.scalatest.prop.PropertyChecks
 * import org.scalatest.matchers.ShouldMatchers
 *
 * class MathSpec extends PropSpec with PropertyChecks with ShouldMatchers {
 *
 *   property("addition", SlowTest) {
 *     forAll { (i: Int) => i + i should equal (2 * i) }
 *   }
 *
 *   property("subtraction", SlowTest, DbTest) {
 *     forAll { (i: Int) => i - i should equal (0) }
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests, "addition" and "subtraction," with the <code>com.mycompany.tags.SlowTest</code> tag, 
 * and test "subtraction" with the <code>com.mycompany.tags.DbTest</code> tag.
 * </p>
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
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, <em>etc.</em>) used by tests to do their work. You can use fixtures in
 * <code>PropSpec</code>s with the same approaches suggested for <code>FunSuite</code> in
 * its documentation. For more information, see the <a href="FunSuite.html#SharedFixtures">Shared fixtures</a> section of <code>FunSuite</code>'s
 * documentation (and substitute <code>property</code> for <code>test</code>).
 * </p>
 *
 * <a name="SharedTests"></a><h2>Shared tests</h2>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. In other words, you may want to write tests that are "shared"
 * by different fixture objects.
 * You accomplish this in a <code>PropSpec</code> in the same way you would do it in a <code>FunSuite</code>, exception instead of <code>test</code>
 * you say <code>property</code>, and instead of <code>testsFor</code> you say <code>propertiesFor</code>. 
 * For more information, see the <a href="FunSuite.html#SharedTests">Shared tests</a> section of <code>FunSuite</code>'s
 * documentation.
 * </p>
 *
 * @author Bill Venners
 */
@Style("org.scalatest.finders.PropSpecFinder")
trait PropSpec extends Suite { thisSuite =>

  private final val engine = new Engine("concurrentPropSpecMod", "PropSpec")
  import engine._

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>PropSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  /**
   * Returns a <code>Documenter</code> that during test execution will forward strings passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>PropSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def markup: Documenter = atomicDocumenter.get

  /**
   * Register a property-based test with the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later execution via an invocation of one of the <code>run</code>
   * methods. The passed test name must not have been registered previously on
   * this <code>PropSpec</code> instance.
   *
   * @param testName the name of the property
   * @param testTags the optional list of tags for this property
   * @param testFun the property function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   * @throws NullPointerException if <code>testName</code> or any passed test tag is <code>null</code>
   */
  protected def property(testName: String, testTags: Tag*)(testFun: => Unit) {
    registerTest(testName, testFun _, "propertyCannotAppearInsideAnotherProperty", "PropSpec.scala", "property", 4, -2, None, None, testTags: _*)
  }

  /**
   * Register a property-based test to ignore, which has the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>run</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>test</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be run, but a
   * report will be sent that indicates the test was ignored. The passed test name must not have been registered previously on
   * this <code>PropSpec</code> instance.
   *
   * @param testName the name of the test
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   */
  protected def ignore(testName: String, testTags: Tag*)(testFun: => Unit) {
    registerIgnoredTest(testName, testFun _, "ignoreCannotAppearInsideAProperty", "PropSpec.scala", "ignore", 4, -2, testTags: _*)
  }

  /**
  * An immutable <code>Set</code> of test names. If this <code>PropSpec</code> contains no tests, this method returns an empty <code>Set</code>.
  *
  * <p>
  * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's iterator will
  * return those names in the order in which the tests were registered.
  * </p>
  */
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by <code>testName</code>.
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   *
   * @throws IllegalArgumentException if <code>testName</code> is defined but a test with that name does not exist on this <code>PropSpec</code>
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args) {

    def invokeWithFixture(theTest: TestLeaf) {
      val theConfigMap = args.configMap
      withFixture(
        new NoArgTest {
          def name = testName
          def apply() { theTest.testFun() }
          def configMap = theConfigMap
        }
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>PropSpec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>PropSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to 
   * methods <code>test</code> and <code>ignore</code>. 
   * </p>
   */
  override def tags: Map[String, Set[String]] = atomic.get.tagsMap

  /**
   * Run zero to many of this <code>PropSpec</code>'s tests.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTests(testName: Option[String], args: Args) {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args) {
    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Registers shared tests.
   *
   * <p>
   * This method enables the following syntax for shared tests in a <code>PropSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * propertiesFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared tests. For examples of shared tests, see the
   * <a href="#SharedTests">Shared tests section</a> in the main documentation for this trait.
   * </p>
   */
  protected def propertiesFor(unit: Unit) {}
  
  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.PropSpec"
}
