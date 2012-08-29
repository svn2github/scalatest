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

import scala.collection.immutable.ListSet
import org.scalatest.Suite.{IgnoreAnnotation, autoTagClassAnnotations}
import org.scalatest._
import Spec._
import Suite._
import org.scalatest.events.{TopOfClass, TopOfMethod}
import scala.reflect.NameTransformer._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}

@Finders(Array("org.scalatest.finders.SpecFinder"))
trait Spec extends Suite  { thisSuite => 
  
  private final val engine = new FixtureEngine[FixtureParam]("concurrentSpecMod", "Spec")
  import engine._
  // Sychronized on thisSuite, only accessed from ensureScopesAndTestsRegistered
  private var scopesRegistered = false
  
  private def ensureScopesAndTestsRegistered() {
    
    thisSuite.synchronized {
      if (!scopesRegistered) {
        scopesRegistered = true
        def getMethod(o: AnyRef, testName: String) = { 
          val methodName = encode(simpleNameForTest(testName))
          if (testMethodTakesAFixture(testName)) {
            val candidateMethods = o.getClass.getMethods.filter(_.getName == methodName)
            candidateMethods.find{ candidateMethod => 
                val paramTypes = candidateMethod.getParameterTypes
                paramTypes.length == 1
            } match {
              case Some(method) => method
              case None => throw new IllegalArgumentException(Resources("testNotFound", testName))
            }
          }
          else
            o.getClass.getMethod(methodName, new Array[Class[_]](0): _*)
        }
        
        def getMethodTags(o: AnyRef, methodName: String) =
          for {
            a <- getMethod(o, methodName).getDeclaredAnnotations
            annotationClass = a.annotationType
            if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
          } yield annotationClass.getName
          
        def isScopeMethod(o: AnyRef, m: Method): Boolean = {
          val className = o.getClass.getName
          val scopeClassName = 
            if (className.endsWith("$"))
              className + m.getName + "$"
            else
              className + "$" + m.getName + "$"
          scopeClassName == m.getReturnType.getName
        }
        
        def getScopeDesc(m: Method): String = {
          val objName = m.getReturnType.getName
          val objClassName = decode(objName.substring(0, objName.length - 1))
          objClassName.substring(objClassName.lastIndexOf("$") + 1)
        }
        
        val testTags = tags
        object MethodNameEncodedOrdering extends Ordering[Method] {
          def compare(x: Method, y: Method): Int = {
            decode(x.getName) compareTo decode(y.getName)
          }
        }
        
        def register(o: AnyRef) {
          val testMethods = o.getClass.getMethods.filter(isTestMethod(_)).sorted(MethodNameEncodedOrdering)
          
          testMethods.foreach { m =>
            val scope = isScopeMethod(o, m)
            if (scope) {
              val scopeDesc = getScopeDesc(m)
              def scopeFun = {
                val scopeObj = m.invoke(o)
                register(scopeObj)
              }
              val scopeLocation = TopOfClass(o.getClass.getName)
              registerNestedBranch(scopeDesc, None, scopeFun, "registrationAlreadyClosed", sourceFileName, "discoveryAndRegisterTests", 2, 0, Some(scopeLocation))
            }
            else {
              val methodName = m.getName
              val testName = 
                if (m.getParameterTypes.length == 0)
                  decode(methodName)
                else
                  decode(methodName) + FixtureInParens
              val methodTags = getMethodTags(o, testName)
              val testFun: FixtureParam => Unit = (fixture: FixtureParam) => { 
                val anyRefFixture: AnyRef = fixture.asInstanceOf[AnyRef] // TODO zap this cast
                val argsArray: Array[Object] = 
                  if (m.getParameterTypes.length == 0)
                    Array.empty
                  else
                    Array(anyRefFixture)  
                try m.invoke(o, argsArray: _*)
                catch {
                  case ite: InvocationTargetException => 
                    throw ite.getTargetException
                }
              }
          
              val testLocation = TopOfMethod(o.getClass.getName, m.toGenericString)
              val isIgnore = testTags.get(methodName) match {
                case Some(tagSet) => tagSet.contains(IgnoreAnnotation) || methodTags.contains(IgnoreAnnotation)
                case None => methodTags.contains(IgnoreAnnotation)
              }
              if (isIgnore)
                registerIgnoredTest(testName, testFun, "registrationAlreadyClosed", sourceFileName, "discoveryAndRegisterTests", 3, 0, Some(testLocation), methodTags.map(new Tag(_)): _*)
              else
                registerTest(testName, testFun, "registrationAlreadyClosed", sourceFileName, "discoveryAndRegisterTests", 2, 0, None, Some(testLocation), None, methodTags.map(new Tag(_)): _*)
            }
          }
        }
     
        register(thisSuite)
      }
    }
  }

  // TODO: Probably make this private final val sourceFileName in a singleton object so it gets compiled in rather than carried around in each instance
  private[scalatest] val sourceFileName = "Spec.scala"

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>Spec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get
  
  /**
   * Returns a <code>Documenter</code> that during test execution will forward strings passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>Spec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def markup: Documenter = atomicDocumenter.get
  
  /**
   * An immutable <code>Set</code> of test names. If this <code>Spec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>Spec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.Spec
   *
   * class StackSpec extends Spec {
   *   object &#96;A Stack&#96; {
   *     object &#96;(when not empty)&#96; {
   *       def &#96;must allow me to pop&#96; {}
   *     }
   *     object &#96;(when not full)&#96; {
   *       def &#96;must allow me to push&#96; {}
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>Spec</code> will yield a set that contains the following
   * two test name strings:
   * </p>
   *
   * <pre class="stExamples">
   * "A Stack (when not empty) must allow me to pop"
   * "A Stack (when not full) must allow me to push"
   * </pre>
   */
  override def testNames: Set[String] = {
    ensureScopesAndTestsRegistered()
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }
  
  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args) {

    ensureScopesAndTestsRegistered()

    def invokeWithFixture(theTest: TestLeaf) {
      val theConfigMap = args.configMap
      withFixture(
        new OneArgTest {
          def name = testName
          def apply(fixture: FixtureParam) { theTest.testFun(fixture) }
          def configMap = theConfigMap
        }
        //new TestFunAndConfigMap(testName, theTest.testFun, theConfigMap)
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }
  
  final override def expectedTestCount(filter: Filter): Int = {
    ensureScopesAndTestsRegistered()
    super.expectedTestCount(filter)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>Spec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>Spec</code> contains no tags, this method returns an empty <code>Map</code>.
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
  override def tags: Map[String, Set[String]] = {
    ensureScopesAndTestsRegistered()
    autoTagClassAnnotations(atomic.get.tagsMap, this)
  }
  
  /**
   * Run zero to many of this <code>Spec</code>'s tests.
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
    ensureScopesAndTestsRegistered()
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args) {
    ensureScopesAndTestsRegistered()
    runImpl(thisSuite, testName, args, super.run)
  }
  
  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.fixture.Spec"

}

private[scalatest] object Spec {
  
  def isTestMethod(m: Method): Boolean = {

    val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

    val paramTypes = m.getParameterTypes
    val hasNoParamOrFixtureParam = paramTypes.isEmpty || paramTypes.length == 1

    // name must have at least one encoded space: "$u0220"
    val includesEncodedSpace = m.getName.indexOf("$u0020") >= 0

    // def maybe(b: Boolean) = if (b) "" else "!"
    // println("m.getName: " + m.getName + ": " + maybe(isInstanceMethod) + "isInstanceMethod, " + maybe(hasNoParams) + "hasNoParams, " + maybe(includesEncodedSpace) + "includesEncodedSpace")
    isInstanceMethod && hasNoParamOrFixtureParam && includesEncodedSpace
  }
}