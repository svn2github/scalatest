package org.scalatest

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.events.Event
import org.scalatest.events.ScopeOpened
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.ScopeClosed
import collection.mutable.ListBuffer
import org.scalatest.events.InfoProvided
import org.scalatest.tools.ConcurrentDistributor
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import org.scalatest.tools.SuiteRunner

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
  
  private def checkInfoProvided(event: Event, message: String) {
    event match {
      case infoProvided: InfoProvided => assert(infoProvided.message === message)
      case _ => fail("Expected InfoProvided, but got " + event.getClass.getName)
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
    
    class ControlledOrderConcurrentDistributor(poolSize: Int) extends Distributor {
      val buf = ListBuffer.empty[SuiteRunner]
      val execSvc: ExecutorService = Executors.newFixedThreadPool(2)
      def apply(suite: Suite, args: RunArgs) {
        buf += new SuiteRunner(suite, args)
      }
      def executeInOrder() {
        for (suiteRunner <- buf) {
          execSvc.submit(suiteRunner)
        }
      }
      def executeInReverseOrder() {
        for (suiteRunner <- buf.reverse) {
          execSvc.submit(suiteRunner)
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
    
    it("should have InfoProvided fired from before and after block in correct order when tests are executed in parallel") {
      
      def withDistributor(fun: ControlledOrderDistributor => Unit) {

        val recordingReporter = new EventRecordingReporter
        val outOfOrderDistributor = new ControlledOrderDistributor
        (new ExampleBeforeAfterParallelSpec).run(None, RunArgs(recordingReporter, distributor = Some(outOfOrderDistributor)))
        fun(outOfOrderDistributor)

        val eventRecorded = recordingReporter.eventsReceived
        assert(eventRecorded.size === 28)

        checkScopeOpened(eventRecorded(0), "Thing 1")
        checkInfoProvided(eventRecorded(1), "In Before")
        checkTestStarting(eventRecorded(2), "Thing 1 do thing 1a")
        checkTestSucceeded(eventRecorded(3), "Thing 1 do thing 1a")
        checkInfoProvided(eventRecorded(4), "In After")
        checkInfoProvided(eventRecorded(5), "In Before")
        checkTestStarting(eventRecorded(6), "Thing 1 do thing 1b")
        checkTestSucceeded(eventRecorded(7), "Thing 1 do thing 1b")
        checkInfoProvided(eventRecorded(8), "In After")
        checkInfoProvided(eventRecorded(9), "In Before")
        checkTestStarting(eventRecorded(10), "Thing 1 do thing 1c")
        checkTestSucceeded(eventRecorded(11), "Thing 1 do thing 1c")
        checkInfoProvided(eventRecorded(12), "In After")
        checkScopeClosed(eventRecorded(13), "Thing 1")
        
        checkScopeOpened(eventRecorded(14), "Thing 2")
        checkInfoProvided(eventRecorded(15), "In Before")
        checkTestStarting(eventRecorded(16), "Thing 2 do thing 2a")
        checkTestSucceeded(eventRecorded(17), "Thing 2 do thing 2a")
        checkInfoProvided(eventRecorded(18), "In After")
        checkInfoProvided(eventRecorded(19), "In Before")
        checkTestStarting(eventRecorded(20), "Thing 2 do thing 2b")
        checkTestSucceeded(eventRecorded(21), "Thing 2 do thing 2b")
        checkInfoProvided(eventRecorded(22), "In After")
        checkInfoProvided(eventRecorded(23), "In Before")
        checkTestStarting(eventRecorded(24), "Thing 2 do thing 2c")
        checkTestSucceeded(eventRecorded(25), "Thing 2 do thing 2c")
        checkInfoProvided(eventRecorded(26), "In After")
        checkScopeClosed(eventRecorded(27), "Thing 2")
      }
      withDistributor(_.executeInOrder())
      withDistributor(_.executeInReverseOrder())
    }
    
    it("should have the blocking event fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
      def withDistributor(fun: ControlledOrderConcurrentDistributor => Unit) {
        val recordingReporter = new EventRecordingReporter
        val args = RunArgs(recordingReporter)
        val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
        (new ExampleTimeoutParallelSpec).run(None, RunArgs(recordingReporter, distributor = Some(outOfOrderConcurrentDistributor)))
        fun(outOfOrderConcurrentDistributor)
        Thread.sleep(3000)  // Get enough time for the timeout to reach, and the missing event to fire.

        val eventRecorded = recordingReporter.eventsReceived
        assert(eventRecorded.size === 16)

        checkScopeOpened(eventRecorded(0), "Thing 1")
        checkTestStarting(eventRecorded(1), "Thing 1 do thing 1a")
        checkTestSucceeded(eventRecorded(2), "Thing 1 do thing 1a")
        checkTestStarting(eventRecorded(3), "Thing 1 do thing 1b")        
        checkTestStarting(eventRecorded(4), "Thing 1 do thing 1c")
        checkTestSucceeded(eventRecorded(5), "Thing 1 do thing 1c")
        checkScopeClosed(eventRecorded(6), "Thing 1")
        
        checkScopeOpened(eventRecorded(7), "Thing 2")
        checkTestStarting(eventRecorded(8), "Thing 2 do thing 2a")
        checkTestSucceeded(eventRecorded(9), "Thing 2 do thing 2a")
        checkTestStarting(eventRecorded(10), "Thing 2 do thing 2b")
        checkTestSucceeded(eventRecorded(11), "Thing 2 do thing 2b")
        checkTestStarting(eventRecorded(12), "Thing 2 do thing 2c")
        checkTestSucceeded(eventRecorded(13), "Thing 2 do thing 2c")
        checkScopeClosed(eventRecorded(14), "Thing 2")
        
        // Now the missing one.
        checkTestSucceeded(eventRecorded(15), "Thing 1 do thing 1b")
      }

      withDistributor(_.executeInOrder())
      withDistributor(_.executeInReverseOrder())
    }
  }
}