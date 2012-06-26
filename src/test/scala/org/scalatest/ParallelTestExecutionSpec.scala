package org.scalatest

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.events.Event
import org.scalatest.events.ScopeOpened
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.ScopeClosed

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
    
    it("should have the events reported in correct order when tests are executed in parallel") {
      val recordingReporter = new EventRecordingReporter()
      new ExampleParallelSpec().run(None, RunArgs(recordingReporter, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      
      val eventRecorded = recordingReporter.eventsReceived
      
      checkScopeOpened(eventRecorded(0), "Thing 1")
      checkTestStarting(eventRecorded(1), "Thing 1 should do thing 1a")
      checkTestSucceeded(eventRecorded(2), "Thing 1 should do thing 1a")
      checkTestStarting(eventRecorded(3), "Thing 1 should do thing 1b")
      checkTestSucceeded(eventRecorded(4), "Thing 1 should do thing 1b")
      checkTestStarting(eventRecorded(5), "Thing 1 should do thing 1c")
      checkTestSucceeded(eventRecorded(6), "Thing 1 should do thing 1c")
      checkScopeClosed(eventRecorded(7), "Thing 1")
      
      checkScopeOpened(eventRecorded(8), "Thing 2")
      checkTestStarting(eventRecorded(9), "Thing 2 should do thing 2a")
      checkTestSucceeded(eventRecorded(10), "Thing 2 should do thing 2a")
      checkTestStarting(eventRecorded(11), "Thing 2 should do thing 2b")
      checkTestSucceeded(eventRecorded(12), "Thing 2 should do thing 2b")
      checkTestStarting(eventRecorded(13), "Thing 2 should do thing 2c")
      checkTestSucceeded(eventRecorded(14), "Thing 2 should do thing 2c")
      checkScopeClosed(eventRecorded(15), "Thing 2")
    }
  }
}