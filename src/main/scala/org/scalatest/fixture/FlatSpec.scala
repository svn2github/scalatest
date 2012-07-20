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
package org.scalatest.fixture

import org.scalatest._
import FixtureNodeFamily._
import verb.{ResultOfTaggedAsInvocation, ResultOfStringPassedToVerb, BehaveWord, ShouldVerb, MustVerb, CanVerb}
import scala.collection.immutable.ListSet
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepth
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import org.scalatest.Suite.anErrorThatShouldCauseAnAbort
import org.scalatest.Suite.autoTagClassAnnotations

/**
 * A sister trait to <code>org.scalatest.FlatSpec</code> that can pass a fixture object into its tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>fixture.FlatSpec</code> in situations for which <a href="../FlatSpec.html"><code>FlatSpec</code></a>
 * would be a good choice, such as XXXXXXX, when all or most tests need the same fixture objects
 * that must be cleaned up afterwords. <em>Note: <code>fixture.FlatSpec</code> is intended for use in special situations, with trait <code>FlatSpec</code> used for general needs. For
 * more insight into where <code>fixture.FlatSpec</code> fits in the big picture, see the <a href="../FlatSpec.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../FlatSpec.html#sharedFixtures">Shared fixtures</a> section in the documentation for trait <code>FlatSpec</code>.</em>
 * </td></tr></table>
 * 
 * <p>
 * Trait <code>fixture.FlatSpec</code> behaves similarly to trait <code>org.scalatest.FlatSpec</code>, except that tests may have a
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
 * Subclasses of this trait must, therefore, do three things differently from a plain old <code>org.scalatest.FlatSpec</code>:
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
 * package org.scalatest.examples.flatspec.oneargtest
 * 
 * import org.scalatest.fixture
 * import java.io._
 * 
 * class ExampleSpec extends fixture.FlatSpec {
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
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   "Testing" should "be easy" in { f =&gt;
 *     f.writer.write("easy!")
 *     f.writer.flush()
 *     assert(f.file.length === 18)
 *   }
 * 
 *   it should "be fun" in { f =&gt;
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
 * use with that database, you will likely have many test classes that need a database fixture. You can create a "database fixture" trait that creates a
 * database with a unique name, passes the connector into the test, then removes the database once the test completes. This is shown in the following example:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.flatspec.sharing
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
 *     finally {
 *       removeDb(dbName) // clean up the fixture
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends fixture.FlatSpec with DbFixture {
 * 
 *   override def populateDb(db: Db) { // setup the fixture
 *     db.append("ScalaTest is ")
 *   }
 * 
 *   "Testing" should "be easy" in { db =&gt;
 *       db.append("easy!")
 *       assert(db.toString === "ScalaTest is easy!")
 *   }
 * 
 *   it should "be fun" in { db =&gt;
 *       db.append("fun!")
 *       assert(db.toString === "ScalaTest is fun!")
 *   }
 * 
 *   // This test doesn't need a Db
 *   "Test code" should "be clear" in { () =&gt;
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
 * If a test doesn't need the fixture, you can indicate that by providing a no-arg instead of a one-arg function, as is done in the
 * third test in the previous example, &ldquo;<code>Test code should be clear</code>&rdquo;. In other words, instead of starting your function literal
 * with something like &ldquo;<code>db =&gt;</code>&rdquo;, you'd start it with &ldquo;<code>() =&gt;</code>&rdquo;. For such tests, <code>runTest</code>
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
 * <h2>FOR REFERENCE, THE OLD STUFF</h2>
 *
 * <p>
 * The purpose of <code>fixture.FlatSpec</code> and its subtraits is to facilitate writing tests in
 * a functional style. Some users may prefer writing tests in a functional style in general, but one
 * particular use case is parallel test execution (See <a href="../ParallelTestExecution.html">ParallelTestExecution</a>). To run
 * tests in parallel, your test class must
 * be thread safe, and a good way to make it thread safe is to make it functional. A good way to
 * write tests that need common fixtures in a functional style is to pass the fixture objects into the tests,
 * the style enabled by the <code>fixture.Suite</code> family of traits.
 * </p>
 *
 * <p>
 * Trait <code>org.scalatest.fixture.FlatSpec</code> behaves similarly to trait <code>org.scalatest.FlatSpec</code>, except that tests may have a
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
 * Subclasses of this trait must, therefore, do three things differently from a plain old <code>org.scalatest.FlatSpec</code>:
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
 * class StackSpec extends fixture.FlatSpec {
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
 *   "A Stack" should "pop a value" in { stack =>
 *     val top = stack.pop()
 *     assert(top === 2)
 *     assert(stack.size === 1)
 *   }
 *
 *   it should "push a value" in { stack =>
 *     stack.push(9)
 *     assert(stack.size === 3)
 *     assert(stack.head === 9)
 *   }
 *
 *   // 4. You can also write tests that don't take a fixture parameter.
 *   it should "complain if popped when empty" in () {
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
 * class ExampleSpec extends fixture.FlatSpec {
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
 *   "Testing" should "be easy" in { f =>
 *     f.builder.append("easy!")
 *     assert(f.builder.toString === "ScalaTest is easy!")
 *     assert(f.buffer.isEmpty)
 *     f.buffer += "sweet"
 *   }
 *
 *   "Testing" should "be fun" in { f =>
 *     f.builder.append("fun!")
 *     assert(f.builder.toString === "ScalaTest is fun!")
 *     assert(f.buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <h2>Configuring fixtures and tests</h2>
 *
 * <p>
 * Sometimes you may want to write tests that are configurable. For example, you may want to write
 * a suite of tests that each take an open temp file as a fixture, but whose file name is specified
 * externally so that the file name can be can be changed from run to run. To accomplish this
 * the <code>OneArgTest</code> trait has a <code>configMap</code>
 * method, which will return a <code>Map[String, Any]</code> from which configuration information may be obtained.
 * The <code>runTest</code> method of this trait will pass a <code>OneArgTest</code> to <code>withFixture</code>
 * whose <code>configMap</code> method returns the <code>configMap</code> passed to <code>runTest</code>.
 * Here's an example in which the name of a temp file is taken from the passed <code>configMap</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class ExampleSpec extends fixture.FlatSpec {
 *
 *   type FixtureParam = FileReader
 *   def withFixture(test: OneArgTest) {
 *
 *     require(
 *       test.configMap.contains("TempFileName"),
 *       "This suite requires a TempFileName to be passed in the configMap"
 *     )
 *
 *     // Grab the file name from the configMap
 *     val FileName = test.configMap("TempFileName").asInstanceOf[String]
 *
 *     // Set up the temp file needed by the test
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *
 *     // Create the reader needed by the test
 *     val reader = new FileReader(FileName)
 *  
 *     try {
 *       // Run the test using the temp file
 *       test(reader)
 *     }
 *     finally {
 *       // Close and delete the temp file
 *       reader.close()
 *       val file = new File(FileName)
 *       file.delete()
 *     }
 *   }
 * 
 *   "A file" can "be read" in { reader =>
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   "The first char of a file" can "be read" in { reader =>
 *     assert(reader.read() === 'H')
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you want to pass into each test the entire <code>configMap</code> that was passed to <code>runTest</code>, you 
 * can mix in trait <code>ConfigMapFixture</code>. See the <a href="ConfigMapFixture.html">documentation
 * for <code>ConfigMapFixture</code></a> for the details, but here's a quick
 * example of how it looks:
 * </p>
 *
 * <pre class="stHighlight">
 *  import org.scalatest.fixture
 *  import org.scalatest.fixture.ConfigMapFixture
 *
 *  class ExampleSpec extends fixture.FlatSpec with ConfigMapFixture {
 *
 *    "The config map" should "contain hello" in { configMap =>
 *      // Use the configMap passed to runTest in the test
 *      assert(configMap.contains("hello"))
 *    }
 *
 *    it should "contain world" in { configMap =>
 *      assert(configMap.contains("world"))
 *    }
 *  }
 * </pre>
 *
 * <h2>Providing multiple fixtures</h2>
 *
 * <p>
 * If different tests in the same <code>fixture.FlatSpec</code> need different shared fixtures, you can use the <em>loan pattern</em> to supply to
 * each test just the fixture or fixtures it needs. First select the most commonly used fixture objects and pass them in via the
 * <code>FixtureParam</code>. Then for each remaining fixture needed by multiple tests, create a <em>with&lt;fixture name&gt;</em>
 * method that takes a function you will use to pass the fixture to the test. Lasty, use the appropriate
 * <em>with&lt;fixture name&gt;</em> method or methods in each test.
 * </p>
 *
 * <p>
 * In the following example, the <code>FixtureParam</code> is set to <code>Map[String, Any]</code> by mixing in <code>ConfigMapFixture</code>.
 * The <code>withFixture</code> method in trait <code>ConfigMapFixture</code> will pass the config map to any test that needs it.
 * In addition, some tests in the following example need a <code>Stack[Int]</code> and others a <code>Stack[String]</code>.
 * The <code>withIntStack</code> method takes
 * care of supplying the <code>Stack[Int]</code> to those tests that need it, and the <code>withStringStack</code> method takes care
 * of supplying the <code>Stack[String]</code> fixture. Here's how it looks:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import org.scalatest.fixture.ConfigMapFixture
 * import collection.mutable.Stack
 * 
 * class StackSpec extends fixture.FlatSpec with ConfigMapFixture {
 * 
 *   def withIntStack(test: Stack[Int] => Any) {
 *     val stack = new Stack[Int]
 *     stack.push(1)
 *     stack.push(2)
 *     test(stack) // "loan" the Stack[Int] fixture to the test
 *   }
 * 
 *   def withStringStack(test: Stack[String] => Any) {
 *     val stack = new Stack[String]
 *     stack.push("one")
 *     stack.push("two")
 *     test(stack) // "loan" the Stack[String] fixture to the test
 *   }
 * 
 *   "A Stack" must "pop an Int value" in { () => // This test doesn't need the configMap fixture, ...
 *     withIntStack { stack =>
 *       val top = stack.pop() // But it needs the Stack[Int] fixture.
 *       assert(top === 2)
 *       assert(stack.size === 1)
 *     }
 *   }
 * 
 *   it must "push an Int value" in { configMap =>
 *     withIntStack { stack =>
 *       val iToPush = // This test uses the configMap fixture...
 *         configMap("IntToPush").toString.toInt
 *       stack.push(iToPush) // And also uses the Stack[Int] fixture.
 *       assert(stack.size === 3)
 *       assert(stack.head === iToPush)
 *     }
 *   }
 * 
 *   it must "pop a String value" in { () => // This test doesn't need the configMap fixture, ...
 *     withStringStack { stack =>
 *       val top = stack.pop() // But it needs the Stack[String] fixture.
 *       assert(top === "two")
 *       assert(stack.size === 1)
 *     }
 *   }
 * 
 *   it must "push a String value" in { configMap =>
 *     withStringStack { stack =>
 *       val sToPush = // This test uses the configMap fixture...
 *         configMap("StringToPush").toString
 *       stack.push(sToPush) // And also uses the Stack[Int] fixture.
 *       assert(stack.size === 3)
 *       assert(stack.head === sToPush)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run the previous class in the Scala interpreter, you'll see:
 * </p>
 *
 * <pre class="stREPL">
 * scala> import org.scalatest._
 * import org.scalatest._
 *
 * scala> run(new StackSpec, configMap = Map("IntToPush" -> 9, "StringToPush" -> "nine"))
 * <span class="stGreen">StackSpec:
 * A Stack
 * - must pop an Int value
 * - must push an Int value
 * - must pop a String value
 * - must push a String value</span>
 * </pre>
 *
 * @author Bill Venners
 */
trait FlatSpec extends Suite with ShouldVerb with MustVerb with CanVerb { thisSuite =>

  private final val engine = new FixtureEngine[FixtureParam]("concurrentFixtureFlatSpecMod", "FixtureFlatSpec")
  import engine._
  
  private[scalatest] val sourceFileName = "FlatSpec.scala"

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>fixture.FlatSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  /**
   * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
   * An invocation of this method is called an &#8220;example.&#8221;
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FlatSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param methodName caller's method name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToRun(specText: String, testTags: List[Tag], methodName: String, testFun: FixtureParam => Any) {

    // TODO: This is what was being used before but it is wrong
    registerTest(specText, testFun, "itCannotAppearInsideAnotherIt", sourceFileName, methodName, 4, -3, None, None, testTags: _*)
  }

  /**
   * Class that supports the registration of a &#8220;subject&#8221; being specified and tested via the
   * instance referenced from <code>fixture.FlatSpec</code>'s <code>behavior</code> field.
   *
   * <p>
   * This field enables syntax such as the following subject registration:
   * </p>
   *
   * <pre class="stHighlight">
   * behavior of "A Stack"
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>behavior</code> field, see the <a href="../FlatSpec.html">main documentation</a>
   * for trait <code>fixture.FlatSpec</code>.
   * </p>
   */
  protected final class BehaviorWord {

    /**
     * Supports the registration of a &#8220;subject&#8221; being specified and tested via the
     * instance referenced from <code>fixture.FlatSpec</code>'s <code>behavior</code> field.
     *
     * <p>
     * This method enables syntax such as the following subject registration:
     * </p>
     *
     * <pre class="stHighlight">
     * behavior of "A Stack"
     *          ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of this method, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def of(description: String) {
      registerFlatBranch(description, "describeCannotAppearInsideAnIt", sourceFileName, "of", 3)
    }
  }

  /**
   * Supports the registration of a &#8220;subject&#8221; being specified and tested.
   *
   * <p>
   * This field enables syntax such as the following subject registration:
   * </p>
   *
   * <pre class="stHighlight">
   * behavior of "A Stack"
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>behavior</code> field, see the <a href="../FlatSpec.html">main documentation</a>
   * for trait <code>FlatSpec</code>.
   * </p>
   */
  protected val behavior = new BehaviorWord

  // TODO: Do a walk through. Are all these being used. I guess I'll find out when
  // I document them.
  /**
   * Class that supports the registration of tagged tests via the <code>ItWord</code> instance
   * referenced from <code>fixture.FlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                                      ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
   *                                                                      ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
   *                                                                      ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field to register tagged tests, see
   * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
   * </p>
   */
  protected final class ItVerbStringTaggedAs(verb: String, name: String, tags: List[Tag]) {

    /**
     * Supports the registration of tagged, no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) in { () => ... }
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToRun(verb + " " + name, tags, "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of tagged, one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) in { fixture => ... }
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToRun(verb + " " + name, tags, "in", testFun)
    }

    /**
     * Supports the registration of pending, tagged tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToRun(verb + " " + name, tags, "is", unusedFixtureParam => testFun)
    }

    /**
     * Supports the registration of ignored, tagged, no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { () => ... }
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: () => Any) {
      registerTestToIgnore(verb + " " + name, tags, "ignore", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of ignored, tagged, one-arg tests (tests that take a <code>FixtureParam</code> object
     * as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { fixture => ... }
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + name, tags, "ignore", testFun)
    }
  }

  /**
   * Class that supports test registration via the instance referenced from <code>fixture.FlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" in { ... }
   *                                                   ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" ignore { ... }
   *                                                   ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" is (pending)
   *                                                   ^
   * </pre>
   *
   * <p>
   * And finally, it also enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                   ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the <a href="FlatSpec.html">main documentation</a>
   * for trait <code>FlatSpec</code>.
   * </p>
   */
  protected final class ItVerbString(verb: String, name: String) {

    /**
     * Supports the registration of no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" in { () => ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of no-arg test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToRun(verb + " " + name, List(), "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" in { fixture => ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of one-arg test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>fixture.FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToRun(verb + " " + name, List(), "in", testFun)
    }

    /**
     * Supports the registration of pending tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" is (pending)
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToRun(verb + " " + name, List(), "is", unusedFixtureParam => testFun)
    }

    /**
     * Supports the registration of ignored no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" ignore { () => ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: () => Any) {
      registerTestToIgnore(verb + " " + name, List(), "ignore", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of ignored one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" ignore { fixture => ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + name, List(), "ignore", testFun)
    }

    /**
     * Supports the registration of tagged tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new ItVerbStringTaggedAs(verb, name, tagList)
    }
  }

  /**
   * Class that supports test (and shared test) registration via the instance referenced from <code>fixture.FlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation 
   * for trait <a href="../FlatSpec.html"><code>FlatSpec</code></a>.
   * </p>
   */
  protected final class ItWord {

    /**
     * Supports the registration of tests with <code>should</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it should "pop values in last-in-first-out order" in { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def should(string: String) = new ItVerbString("should", string)

    /**
     * Supports the registration of tests with <code>must</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" in { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def must(string: String) = new ItVerbString("must", string)

    /**
     * Supports the registration of tests with <code>can</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it can "pop values in last-in-first-out order" in { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def can(string: String) = new ItVerbString("can", string)

    /**
     * Supports the registration of shared tests with <code>should</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it should behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>must</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def must(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>can</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it can behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def can(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>fixture.FlatSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation 
   * for trait <a href="../FlatSpec.html"><code>FlatSpec</code></a>.
   * </p>
   */
  protected val it = new ItWord
  
  /**
   * Class that supports the registration of tagged tests via the <code>TheyWord</code> instance
   * referenced from <code>fixture.FlatSpec</code>'s <code>they</code> field.
   *
   * <p>
   * This class enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                                        ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
   *                                                                        ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
   *                                                                        ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field to register tagged tests, see
   * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
   * </p>
   */
  protected final class TheyVerbStringTaggedAs(verb: String, name: String, tags: List[Tag]) {

    /**
     * Supports the registration of tagged, no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) in { () => ... }
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToRun(verb + " " + name, tags, "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of tagged, one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) in { fixture => ... }
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToRun(verb + " " + name, tags, "in", testFun)
    }

    /**
     * Supports the registration of pending, tagged tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToRun(verb + " " + name, tags, "is", unusedFixtureParam => testFun)
    }

    /**
     * Supports the registration of ignored, tagged, no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { () => ... }
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: () => Any) {
      registerTestToIgnore(verb + " " + name, tags, "ignore", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of ignored, tagged, one-arg tests (tests that take a <code>FixtureParam</code> object
     * as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { fixture => ... }
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + name, tags, "ignore", testFun)
    }
  }

  /**
   * Class that supports test registration via the instance referenced from <code>fixture.FlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" in { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" ignore { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" is (pending)
   *                                                     ^
   * </pre>
   *
   * <p>
   * And finally, it also enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the <a href="FlatSpec.html">main documentation</a>
   * for trait <code>FlatSpec</code>.
   * </p>
   */
  protected final class TheyVerbString(verb: String, name: String) {

    /**
     * Supports the registration of no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" in { () => ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of no-arg test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToRun(verb + " " + name, List(), "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" in { fixture => ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of one-arg test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>fixture.FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToRun(verb + " " + name, List(), "in", testFun)
    }

    /**
     * Supports the registration of pending tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" is (pending)
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToRun(verb + " " + name, List(), "is", unusedFixtureParam => testFun)
    }

    /**
     * Supports the registration of ignored no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" ignore { () => ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: () => Any) {
      registerTestToIgnore(verb + " " + name, List(), "ignore", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of ignored one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" ignore { fixture => ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + name, List(), "ignore", testFun)
    }

    /**
     * Supports the registration of tagged tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new ItVerbStringTaggedAs(verb, name, tagList)
    }
  }

  /**
   * Class that supports test (and shared test) registration via the instance referenced from <code>fixture.FlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation 
   * for trait <a href="../FlatSpec.html"><code>FlatSpec</code></a>.
   * </p>
   */
  protected final class TheyWord {

    /**
     * Supports the registration of tests with <code>should</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they should "pop values in last-in-first-out order" in { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def should(string: String) = new ItVerbString("should", string)

    /**
     * Supports the registration of tests with <code>must</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" in { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def must(string: String) = new ItVerbString("must", string)

    /**
     * Supports the registration of tests with <code>can</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they can "pop values in last-in-first-out order" in { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="../FlatSpec.html">main documentation</a>
     * for trait <code>FlatSpec</code>.
     * </p>
     */
    def can(string: String) = new ItVerbString("can", string)

    /**
     * Supports the registration of shared tests with <code>should</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they should behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>must</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def must(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>can</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they can behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def can(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>fixture.FlatSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation 
   * for trait <a href="../FlatSpec.html"><code>FlatSpec</code></a>.
   * </p>
   */
  protected val they = new TheyWord

  /**
   * Class that supports registration of ignored, tagged tests via the <code>IgnoreWord</code> instance referenced
   * from <code>fixture.FlatSpec</code>'s <code>ignore</code> field.
   *
   * <p>
   * This class enables syntax such as the following registration of an ignored, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                                          ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of an ignored, tagged, pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
   *                                                                          ^
   * </pre>
   *
   * <p>
   * Note: the <code>is</code> method is provided for completeness and design symmetry, given there's no way
   * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
   * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
   * </p>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see
   * the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
   * in the main documentation for trait <code>FlatSpec</code>. For examples of tagged test registration, see
   * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
   * </p>
   */
  protected final class IgnoreVerbStringTaggedAs(verb: String, name: String, tags: List[Tag]) {

    /**
     * Supports the registration of ignored, tagged, no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) in { () => ... }
     *                                                                        ^
     * </pre>
     *
     * <p>
     * For examples of the registration of ignored tests, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>. For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToIgnore(verb + " " + name, tags, "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of ignored, tagged, one-arg tests (tests that take a <code>FixtureParam</code> object as a parameter)
     * in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) in { fixture => ... }
     *                                                                        ^
     * </pre>
     *
     * <p>
     * For examples of the registration of ignored tests, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>. For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + name, tags, "in", testFun)
    }

    /**
     * Supports the registration of ignored, tagged, pending tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
     *                                                                        ^
     * </pre>
     *
     * <p>
     * Note: this <code>is</code> method is provided for completeness and design symmetry, given there's no way
     * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
     * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
     * </p>
     *
     * <p>
     * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  For examples of the registration of ignored tests,
     * see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>. For examples of tagged test registration, see
     * the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToIgnore(verb + " " + name, tags, "is", unusedFixtureParam => testFun)
    }
  }

  /**
   * Class that supports registration of ignored tests via the <code>IgnoreWord</code> instance referenced
   * from <code>fixture.FlatSpec</code>'s <code>ignore</code> field.
   *
   * <p>
   * This class enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" in { ... }
   *                                                       ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of an ignored, pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" is (pending)
   *                                                       ^
   * </pre>
   *
   * Note: the <code>is</code> method is provided for completeness and design symmetry, given there's no way
   * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
   * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
   * </p>
   *
   * <p>
   * And finally, it also enables syntax such as the following ignored, tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                       ^
   * </pre>
   *
   * <p>
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
   * in the main documentation for trait <code>FlatSpec</code>.
   * </p>
   */
  protected final class IgnoreVerbString(verb: String, name: String) {

    /**
     * Supports the registration of ignored, no-arg tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" in { () => ... }
     *                                                     ^
     * </pre>
     *
     * <p>
     * For examples of the registration of ignored tests, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToIgnore(verb + " " + name, List(), "in", new NoArgTestWrapper(testFun))
    }
     
    /**
     * Supports the registration of ignored, one-arg tests (tests that take a <code>FixtureParam</code> object
     * as a parameter) in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" in { fixture => ... }
     *                                                     ^
     * </pre>
     *
     * <p>
     * For examples of the registration of ignored tests, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + name, List(), "in", testFun)
    }

    /**
     * Supports the registration of ignored, pending tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" is (pending)
     *                                                     ^
     * </pre>
     *
     * <p>
     * Note: this <code>is</code> method is provided for completeness and design symmetry, given there's no way
     * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
     * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
     * </p>
     *
     * <p>
     * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  For examples of the registration of ignored tests,
     * see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def is(testFun: => PendingNothing) {
      registerTestToIgnore(verb + " " + name, List(), "is", unusedFixtureParam => testFun)
    }

    /**
     * Supports the registration of ignored, tagged tests in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                     ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation
     * for trait <code>FlatSpec</code>.  For examples of the registration of ignored tests,
     * see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new IgnoreVerbStringTaggedAs(verb, name, tagList)
    }
  }

  /**
   * Class that supports registration of ignored tests via the instance referenced from <code>fixture.FlatSpec</code>'s <code>ignore</code> field.
   *
   * <p>
   * This class enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected final class IgnoreWord {

    /**
     * Supports the registration of ignored tests with <code>should</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore should "pop values in last-in-first-out order" in { ... }
     *        ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of the <code>ignore</code> field, see <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def should(string: String) = new IgnoreVerbString("should", string)

    /**
     * Supports the registration of ignored tests with <code>must</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" in { ... }
     *        ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of the <code>ignore</code> field, see <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def must(string: String) = new IgnoreVerbString("must", string)

    /**
     * Supports the registration of ignored tests with <code>can</code> in a <code>fixture.FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore can "pop values in last-in-first-out order" in { ... }
     *        ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of the <code>ignore</code> field, see <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def can(string: String) = new IgnoreVerbString("can", string)
  }

  /**
   * Supports registration of ignored tests in <code>fixture.FlatSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see the
   * <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a> in the main documentation for trait <code>FlatSpec</code>.
   * </p>
   */
  protected val ignore = new IgnoreWord

  /**
   * Class that supports test registration in shorthand form.
   *
   * <p>
   * For example, this class enables syntax such as the following test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" in { ... }
   *                                          ^
   * </pre>
   *
   * <p>
   * This class also enables syntax such as the following ignored test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" ignore { ... }
   *                                          ^
   * </pre>
   *
   * <p>
   * This class is used via an implicit conversion (named <code>convertToInAndIgnoreMethods</code>)
   * from <code>ResultOfStringPassedToVerb</code>. The <code>ResultOfStringPassedToVerb</code> class
   * does not declare any methods named <code>in</code>, because the
   * type passed to <code>in</code> differs in a <code>FlatSpec</code> and a <code>org.scalatest.fixture.FlatSpec</code>.
   * A <code>org.scalatest.fixture.FlatSpec</code> needs two <code>in</code> methods, one that takes a no-arg
   * test function and another that takes a one-arg test function (a test that takes a
   * <code>FixtureParam</code> as its parameter). By constrast, a <code>FlatSpec</code> needs
   * only one <code>in</code> method that takes a by-name parameter. As a result,
   * <code>FlatSpec</code> and <code>org.scalatest.fixture.FlatSpec</code> each provide an implicit conversion
   * from <code>ResultOfStringPassedToVerb</code> to a type that provides the appropriate
   * <code>in</code> methods.
   * </p>
   *
   * @author Bill Venners
   */
  protected final class InAndIgnoreMethods(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {

    import resultOfStringPassedToVerb.verb
    import resultOfStringPassedToVerb.rest

    /**
     * Supports the registration of no-arg tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" in { () => ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>fixture.FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToRun(verb + " " + rest, List(), "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of ignored, no-arg tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" ignore { () => ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: () => Any) {
      registerTestToIgnore(verb + " " + rest, List(), "ignore", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of one-arg tests (tests that take a <code>FixtureParam</code> parameter) in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" in { fixture => ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="FlatSpec.html">main documentation</a>
     * for trait <code>fixture.FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToRun(verb + " " + rest, List(), "in", testFun)
    }

    /**
     * Supports the registration of ignored, one-arg tests (tests that take a <code>FixtureParam</code> parameter) in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" ignore { fixture => ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + rest, List(), "ignore", testFun)
    }
  }

  /**
   * Implicitly converts an object of type <code>ResultOfStringPassedToVerb</code> to an
   * <code>InAndIgnoreMethods</code>, to enable <code>in</code> and <code>ignore</code>
   * methods to be invokable on that object.
   */
  protected implicit def convertToInAndIgnoreMethods(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) =
    new InAndIgnoreMethods(resultOfStringPassedToVerb)

  /**
   * Class that supports tagged test registration in shorthand form.
   *
   * <p>
   * For example, this class enables syntax such as the following tagged test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" taggedAs() in { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * This class also enables syntax such as the following tagged, ignored test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" taggedAs(SlowTest) ignore { ... }
   *                                                             ^
   * </pre>
   *
   * <p>
   * This class is used via an implicit conversion (named <code>convertToInAndIgnoreMethodsAfterTaggedAs</code>)
   * from <code>ResultOfTaggedAsInvocation</code>. The <code>ResultOfTaggedAsInvocation</code> class
   * does not declare any methods named <code>in</code>, because the
   * type passed to <code>in</code> differs in a <code>FlatSpec</code> and a <code>fixture.FlatSpec</code>.
   * A <code>fixture.FlatSpec</code> needs two <code>in</code> methods, one that takes a no-arg
   * test function and another that takes a one-arg test function (a test that takes a
   * <code>FixtureParam</code> as its parameter). By constrast, a <code>FlatSpec</code> needs
   * only one <code>in</code> method that takes a by-name parameter. As a result,
   * <code>FlatSpec</code> and <code>fixture.FlatSpec</code> each provide an implicit conversion
   * from <code>ResultOfTaggedAsInvocation</code> to a type that provides the appropriate
   * <code>in</code> methods.
   * </p>
   *
   * @author Bill Venners
   */
  protected final class InAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation: ResultOfTaggedAsInvocation) {

    import resultOfTaggedAsInvocation.verb
    import resultOfTaggedAsInvocation.rest
    import resultOfTaggedAsInvocation.{tags => tagsList}

    /**
     * Supports the registration of tagged, no-arg tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) in { () => ... }
     *                                                                           ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: () => Any) {
      registerTestToRun(verb + " " + rest, tagsList, "in", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of tagged, ignored, no-arg tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { () => ... }
     *                                                                           ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: () => Any) {
      registerTestToIgnore(verb + " " + rest, tagsList, "ignore", new NoArgTestWrapper(testFun))
    }

    /**
     * Supports the registration of tagged, one-arg tests (tests that take a <code>FixtureParam</code> parameter) in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) in { fixture => ... }
     *                                                                           ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def in(testFun: FixtureParam => Any) {
      registerTestToRun(verb + " " + rest, tagsList, "in", testFun)
    }

    /**
     * Supports the registration of tagged, ignored, one-arg tests (tests that take a <code>FixtureParam</code> parameter) in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { fixture => ... }
     *                                                                           ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="../FlatSpec.html#IgnoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a>
     * in the main documentation for trait <code>FlatSpec</code>.
     * </p>
     */
    def ignore(testFun: FixtureParam => Any) {
      registerTestToIgnore(verb + " " + rest, tagsList, "ignore", testFun)
    }
  }

  /**
   * Implicitly converts an object of type <code>ResultOfTaggedAsInvocation</code> to an
   * <code>InAndIgnoreMethodsAfterTaggedAs</code>, to enable <code>in</code> and <code>ignore</code>
   * methods to be invokable on that object.
   */
  protected implicit def convertToInAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation: ResultOfTaggedAsInvocation) =
    new InAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation)

  /**
   * Supports the shorthand form of test registration.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" in { ... }
   *                        ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject description (the first parameter to the function) and returns a <code>ResultOfStringPassedToVerb</code>
   * initialized with the verb and rest parameters (the second and third parameters to
   * the function, respectively).
   * </p>
   */
  protected implicit val shorthandTestRegistrationFunction: (String, String, String) => ResultOfStringPassedToVerb = {
    (subject, verb, rest) => {
      behavior.of(subject)
      new ResultOfStringPassedToVerb(verb, rest) {
        def is(testFun: => PendingNothing) {
          registerTestToRun(verb + " " + rest, List(), "is", unusedFixtureParam => testFun)
        }
        def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
          val tagList = firstTestTag :: otherTestTags.toList
          new ResultOfTaggedAsInvocation(verb, rest, tagList) {
            // "A Stack" must "test this" taggedAs(mytags.SlowAsMolasses) is (pending)
            //                                                            ^
            def is(testFun: => PendingNothing) {
              registerTestToRun(verb + " " + rest, tags, "is", new NoArgTestWrapper(testFun _))
            }
          }
        }
      }
    }
  }

  // TODO: Get rid of unusedfixture, and use NoArgTestFunction instead

  /**
   * Supports the shorthand form of shared test registration.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (with one item)" should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
   *                           ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject description (the  parameter to the function) and returns a <code>BehaveWord</code>.
   * </p>
   */
  protected implicit val shorthandSharedTestRegistrationFunction: (String) => BehaveWord = {
    (left) => {
      behavior.of(left)
      new BehaveWord
    }
  }

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FlatSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param methodName caller's method name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToIgnore(specText: String, testTags: List[Tag], methodName: String, testFun: FixtureParam => Any) {
    registerIgnoredTest(specText, testFun, "ignoreCannotAppearInsideAnIt", sourceFileName, methodName, 4, -3, testTags: _*)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FlatSpec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>fixture.FlatSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
   * methods <code>test</code> and <code>ignore</code>.
   * </p>
   * 
   * <p>
   * In addition, this trait's implementation will also auto-tag tests with class level annotations.  
   * For example, if you annotate @Ignore at the class level, all test methods in the class will be auto-annotated with @Ignore.
   * </p>
   */
  override def tags: Map[String, Set[String]] = autoTagClassAnnotations(atomic.get.tagsMap, this)

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param configMap a <code>Map</code> of properties that can be used by this <code>FlatSpec</code>'s executing tests.
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args) {

    def invokeWithFixture(theTest: TestLeaf) {
      theTest.testFun match {
        case wrapper: NoArgTestWrapper[_] =>
          withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
        case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
      }
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
   * <p>
   * Run zero to many of this <code>FlatSpec</code>'s tests.
   * </p>
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is <code>Some</code>, this trait's implementation of this method
   * invokes <code>runTest</code> on this object, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> value of the <code>testName</code> <code>Option</code> passed
   *   to this method</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * This method takes a <code>Set</code> of tag names that should be included (<code>tagsToInclude</code>), and a <code>Set</code>
   * that should be excluded (<code>tagsToExclude</code>), when deciding which of this <code>Suite</code>'s tests to execute.
   * If <code>tagsToInclude</code> is empty, all tests will be executed
   * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is non-empty, only tests
   * belonging to tags mentioned in <code>tagsToInclude</code>, and not mentioned in <code>tagsToExclude</code>
   * will be executed. However, if <code>testName</code> is <code>Some</code>, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
   * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
   * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. For more information on trait tags, see the main documentation for this trait.
   * </p>
   *
   * <p>
   * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially execute.
   * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
   * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be executed.)
   * For each test in the <code>testName</code> <code>Set</code>, in the order
   * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
   * of this method checks whether the test should be run based on the <code>tagsToInclude</code> and <code>tagsToExclude</code> <code>Set</code>s.
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
   * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>FlatSpec</code>.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, <code>tagsToInclude</code>,
   *     <code>tagsToExclude</code>, or <code>configMap</code> is <code>null</code>.
   */
  protected override def runTests(testName: Option[String], args: Args) {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>fixture.FlatSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space.
   * </p>
   */
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  override def run(testName: Option[String], args: Args) {
   runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Supports shared test registration in <code>fixture.FlatSpec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonFullStack(stackWithOneItem)
   *           ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of <code>behave</code>, see the <a href="../FlatSpec.html#SharedTests">Shared tests section</a>
   * in the main documentation for trait <code>FlatSpec</code>.
   * </p>
   */
  protected val behave = new BehaveWord
  
  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.fixture.FlatSpec"
}
