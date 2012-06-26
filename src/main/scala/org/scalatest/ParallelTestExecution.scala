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

import events.Event
import org.scalatest.time.Span
import tools.{DistributedTestRunnerSuite, TestSortingReporter, Runner}

/**
 * Trait that causes that the tests of any suite it is mixed into to be run in parallel if
 * a <code>Distributor</code> is passed to <code>runTests</code>.
 *
 * <p>
 * ScalaTest's normal approach for running suites of tests in parallel is to run different suites in parallel,
 * but the tests of any one suite sequentially. This approach should provide sufficient distribution of the work load
 * in most cases, but some suites may encapsulate multiple long-running tests. Such suites may dominate the execution
 * time of the run. If so, mixing in this trait into just those suites will allow their long-running tests to run in parallel with each
 * other, thereby helping to reduce the total time required to run an entire run.
 * </p>
 *
 * <p>
 * Because this trait extends <code>OneInstancePerTest</code>,
 * each test will be run its own instance of the suite's class. This trait overrides the 
 * <code>runTests</code> method. If no <code>Distributor</code> is passed to <code>runTests</code>, 
 * this trait's implementation simply invokes its supertrait <code>OneInstancePerTest</code>'s implementation
 * of <code>runTests</code>, which will run each test in its own instance sequentially. If a <code>Distributor</code>
 * is passed, however, this traits' implementation of <code>runTests</code> will, for each test, wrap a new instance of the
 * suite in a special <em>wrapper suite</em> that will invoke just that one test, and passes the wrapper suites to the <code>Distributor</code>.
 * The thread or entity that takes a wrapper suite from the <code>Distributor</code> will invoke <code>run</code>
 * on the wrapper suite, which will run just one test. In this way, different tests of a suite that mixes in
 * <code>ParallelTestExecution</code> will run in parallel.
 * </p>
 *
 * @author Bill Venners
 */
trait ParallelTestExecution extends OneInstancePerTest { this: Suite =>

  /**
   * Modifies the behavior of <code>super.runTests</code> to facilitate parallel test execution.
   *
   * <p>
   * TODO: Discuss...
   * </p>
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>RunArgs</code> for this run
   */
  protected abstract override def runTests(testName: Option[String], args: RunArgs) {
    val newArgs =
      if (args.runTestInNewInstance)
        args // This is the test-specific instance
      else {
        args.distributor match {  // This is the initial instance
          case Some(distributor) =>
            val testSortingReporter = new TestSortingReporter(args.reporter, sortingTimeout)
            args.copy(reporter = testSortingReporter, distributedTestSorter = Some(testSortingReporter))
          case None =>
            args
        }
      }

    // Always call super.runTests, which is OneInstancePerTest's runTests. But if RTINI is NOT
    // set, that means we are in the initial instance.In that case, we wrap the reporter in
    // a new TestSortingReporter, and wrap the distributor in a new DistributorWrapper that
    // knows is passed the TestSortingReporter. We then call super.runTests, which is OIPT's runTests.
    super.runTests(testName, newArgs)
  }

  /**
   * Modifies the behavior of <code>super.runTest</code> to facilitate parallel test execution.
   *
   * <p>
   * TODO: Discuss...  Note this is final because don't want things like before and after to
   * be executed by the wrong thread, and therefore, at the wrong time. With OIPT, if OIPT is
   * super to BAA, then still things will happen in the expected order, because all is sequential.
   * </p>
   *
   * @param testName the name of one test to execute.
   * @param args the <code>RunArgs</code> for this run
   */
  final protected abstract override def runTest(testName: String, args: RunArgs) {

    if (args.runTestInNewInstance) {
      // In initial instance, so wrap the test in a DistributedTestRunnerSuite and pass it to the Distributor.
      val oneInstance = newInstance
      args.distributor match {
        case None =>
          oneInstance.run(Some(testName), args)
        case Some(distribute) =>
          // Tell the TSR that the test is being distributed
          for (sorter <- args.distributedTestSorter)
            sorter.distributingTest(testName)

          class TestSpecificReporter(testSortingReporter: TestSortingReporter, testName: String) extends Reporter {
            def apply(event: Event) {
              testSortingReporter.apply(testName, event)
            }
          }
/*
          val testSpecificReporter =
            for (sorter <- args.distributedTestSorter)
            yield new TestSpecificReporter(sorter, testName)
*/

          // It will be oneInstance, testName, args.copy(reporter = ...)
          distribute(new DistributedTestRunnerSuite(oneInstance, testName, args), args.copy(tracker = args.tracker.nextTracker))
      }
    }
    else // In test-specific (distributed) instance, so just run the test. (RTINI was
         // removed by OIPT's implementation of runTests.)
         // New Approach: before calling super.runTest, wrap once again in the
         // wrapReporter? And after runTest returns, call testCompleted() on
         // the TSR.
      super.runTest(testName, args)
  }

  /**
   * Construct a new instance of this <code>Suite</code>.
   *
   * <p>
   * This trait's implementation of <code>runTests</code> invokes this method to create
   * a new instance of this <code>Suite</code> for each test. This trait's implementation
   * of this method uses reflection to call <code>this.getClass.newInstance</code>. This
   * approach will succeed only if this <code>Suite</code>'s class has a public, no-arg
   * constructor. In most cases this is likely to be true, because to be instantiated
   * by ScalaTest's <code>Runner</code> a <code>Suite</code> needs a public, no-arg
   * constructor. However, this will not be true of any <code>Suite</code> defined as
   * an inner class of another class or trait, because every constructor of an inner
   * class type takes a reference to the enclosing instance. In such cases, and in
   * cases where a <code>Suite</code> class is explicitly defined without a public,
   * no-arg constructor, you will need to override this method to construct a new
   * instance of the <code>Suite</code> in some other way.
   * </p>
   *
   * <p>
   * Here's an example of how you could override <code>newInstance</code> to construct
   * a new instance of an inner class:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.Suite
   *
   * class Outer {
   *   class InnerSuite extends Suite with ParallelTestExecution {
   *     def testOne() {}
   *     def testTwo() {}
   *     override def newInstance = new InnerSuite
   *   }
   * }
   * </pre>
   */
  override def newInstance: Suite with ParallelTestExecution = {
    val instance = getClass.newInstance.asInstanceOf[Suite with ParallelTestExecution]
    instance
  }

  /**
   * A maximum amount of time to wait for out-of-order events generated by running the tests
   * of this <code>Suite</code> in parallel while sorting the events back into a more
   * user-friendly, sequential order.
   *
   * <p>
   * The default implementation of this method returns the value specified via <code>-T</code> to
   * <a href="tools/Runner$.html"></code>Runner</code></a>, or 15 seconds, if no <code>-T</code> was given.
   * </p>
   *
   * @return a maximum amount of time to wait for events while resorting them into sequential order
   */
  protected def sortingTimeout: Span = Runner.testSortingReporterTimeout
}
