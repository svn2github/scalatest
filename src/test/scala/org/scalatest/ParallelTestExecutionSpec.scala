package org.scalatest

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.events.Event
import org.scalatest.events.ScopeOpened
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.ScopeClosed
import collection.mutable.ListBuffer

class ParallelTestExecutionSpec extends FunSpec with ShouldMatchers {
  /*
  Need 3 tests at least
  1. should have the events reported in correct order when tests are executed in parallel
     For that one, pass in a Distributor that runs with just one thread and orders things
     in a predefined, out of order order.

  2. DistributedSuiteSorter should wait for completedTests instead of moving on when it
     gets a SuiteCompleted.

  3. Both of these should time out. So we need a test for each that shows the timeout
     happened. I.e., it will move on when waiting for something.
   */
  private def checkScopeOpened(event: Event, message: String) {
    event match {
      case scopeOpened: ScopeOpened => assert(scopeOpened.message === message)
      case _ => fail("Expected ScopedOpened, but got " + event.getClass.getName)
    }
  }
  
  private def checkScopeClosed(event: Event, message: String) {
    event match {
      case scopeClosed: ScopeClosed => assert(scopeClosed.message === message)
      case _ => fail("Expected ScopedOpened, but got " + event.getClass.getName)
    }
  }
  
  private def checkTestStarting(event: Event, testName: String) {
    event match {
      case testStarting: TestStarting => assert(testStarting.testName === testName)
      case _ => fail("Expected TestStarting, but got " + event.getClass.getName)
    }
  }
  
  private def checkTestSucceeded(event: Event, testName: String) {
    event match {
      case testSucceeded: TestSucceeded => assert(testSucceeded.testName === testName)
      case _ => fail("Expected TestStarting, but got " + event.getClass.getName)
    }
  }
  
  describe("ParallelTestExecution") {

    class ControlledOrderDistributor extends Distributor {
      val buf = ListBuffer.empty[(Suite, RunArgs)]
      def apply(suite: Suite, args: RunArgs) {
        buf += ((suite, args))
      }
      def executeInOrder() {
        for ((suite, args) <- buf) {
          suite.run(None, args)
        }
      }
      def executeInReverseOrder() {
        for ((suite, args) <- buf.reverse) {
          suite.run(None, args)
        }
      }

      def apply(suite: Suite, tracker: Tracker) {
        throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
      }
    }

    it("should have the events reported in correct order when tests are executed in parallel") {

      def withDistributor(fun: ControlledOrderDistributor => Unit) {

        val recordingReporter = new EventRecordingReporter
        val outOfOrderDistributor = new ControlledOrderDistributor
        (new ExampleParallelSpec).run(None, RunArgs(recordingReporter, distributor = Some(outOfOrderDistributor)))
        fun(outOfOrderDistributor)

        val eventRecorded = recordingReporter.eventsReceived

        checkScopeOpened(eventRecorded(0), "Subject 1")
        checkTestStarting(eventRecorded(1), "Subject 1 should have behavior 1a")
        checkTestSucceeded(eventRecorded(2), "Subject 1 should have behavior 1a")
        checkTestStarting(eventRecorded(3), "Subject 1 should have behavior 1b")
        checkTestSucceeded(eventRecorded(4), "Subject 1 should have behavior 1b")
        checkTestStarting(eventRecorded(5), "Subject 1 should have behavior 1c")
        checkTestSucceeded(eventRecorded(6), "Subject 1 should have behavior 1c")
        checkScopeClosed(eventRecorded(7), "Subject 1")

        checkScopeOpened(eventRecorded(8), "Subject 2")
        checkTestStarting(eventRecorded(9), "Subject 2 should have behavior 2a")
        checkTestSucceeded(eventRecorded(10), "Subject 2 should have behavior 2a")
        checkTestStarting(eventRecorded(11), "Subject 2 should have behavior 2b")
        checkTestSucceeded(eventRecorded(12), "Subject 2 should have behavior 2b")
        checkTestStarting(eventRecorded(13), "Subject 2 should have behavior 2c")
        checkTestSucceeded(eventRecorded(14), "Subject 2 should have behavior 2c")
        checkScopeClosed(eventRecorded(15), "Subject 2")
      }
      withDistributor(_.executeInOrder())
      withDistributor(_.executeInReverseOrder())
    }
  }
}