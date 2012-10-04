package org.scalatest

import org.scalatest.prop.Tables
import scala.collection.mutable.ListBuffer
import org.scalatest.events.Event
import org.scalatest.prop.TableDrivenPropertyChecks
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Future
import org.scalatest.tools.SuiteRunner
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import org.scalatest.tools.SuiteSortingReporter
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteCompleted
import org.scalatest.time.Millis
import java.io.PrintStream
import java.io.ByteArrayOutputStream

class ParallelTestExecutionProp extends FunSuite 
  with TableDrivenPropertyChecks with SharedHelpers  
  with ParallelTestExecutionOrderExamples 
  with ParallelTestExecutionInfoExamples 
  with ParallelTestExecutionTestTimeoutExamples
  with ParallelTestExecutionParallelSuiteExamples 
  with ParallelTestExecutionSuiteTimeoutExamples {
  
  class ControlledOrderDistributor extends Distributor {
    val buf = ListBuffer.empty[(Suite, Args, ScalaTestStatefulStatus)]
    def apply(suite: Suite, args: Args): Status = {
      val status = new ScalaTestStatefulStatus
      buf += ((suite, args, status))
      status
    }
    def executeInOrder() {
      for ((suite, args, status) <- buf) {
        val runStatus = suite.run(None, args)
        if (!runStatus.succeeds())
          status.setFailed()
        status.setCompleted()
      }
    }
    def executeInReverseOrder() {
      for ((suite, args, status) <- buf.reverse) {
        val runStatus = suite.run(None, args)
        if (!runStatus.succeeds())
          status.setFailed()
        status.setCompleted()
      }
    }

    def apply(suite: Suite, tracker: Tracker) {
      throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
    }
  }
  
  class ControlledOrderConcurrentDistributor(poolSize: Int) extends Distributor {
      private val futureQueue = new LinkedBlockingQueue[Future[T] forSome { type T }]
      
      val buf = ListBuffer.empty[SuiteRunner]
      val execSvc: ExecutorService = Executors.newFixedThreadPool(2)
      def apply(suite: Suite, args: Args): Status = {
        val status = new ScalaTestStatefulStatus
        buf += new SuiteRunner(suite, args, status)
        status
      }
      def executeInOrder() {
        for (suiteRunner <- buf) {
          val future: Future[_] = execSvc.submit(suiteRunner)
          futureQueue.put(future)
        }
        while (futureQueue.peek != null) 
          futureQueue.poll().get()
      }
      def executeInReverseOrder() {
        for (suiteRunner <- buf.reverse) {
          val future: Future[_] = execSvc.submit(suiteRunner)
          futureQueue.put(future)
        }
        while (futureQueue.peek != null)
          futureQueue.poll().get()
      }

      def apply(suite: Suite, tracker: Tracker) {
        throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
      }
    }
  
  def withDistributor(suite: Suite, fun: ControlledOrderDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val outOfOrderDistributor = new ControlledOrderDistributor
    suite.run(None, Args(recordingReporter, distributor = Some(outOfOrderDistributor)))
    fun(outOfOrderDistributor)

    recordingReporter.eventsReceived
  }
  
  def withConcurrentDistributor(suite: Suite, fun: ControlledOrderConcurrentDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val args = Args(recordingReporter)
    val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
    suite.run(None, Args(recordingReporter, distributor = Some(outOfOrderConcurrentDistributor)))
    fun(outOfOrderConcurrentDistributor)

    recordingReporter.eventsReceived
  }
  
  def withConcurrentDistributor(suite1: Suite, suite2: Suite, timeout: Span, fun: ControlledOrderConcurrentDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
    val suiteSortingReporter = new SuiteSortingReporter(recordingReporter, timeout, new PrintStream(new ByteArrayOutputStream))
    
    val tracker = new Tracker()
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
        
    suite1.run(None, Args(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suite2.run(None, Args(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
        
    fun(outOfOrderConcurrentDistributor)
        
    recordingReporter.eventsReceived
  }
  
  test("ParallelTestExecution should have the events reported in correct order when tests are executed in parallel") {
    forAll(orderExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder)
      example.assertOrderTest(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder)
      example.assertOrderTest(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have InfoProvided fired from before and after block in correct order when tests are executed in parallel") {
    forAll(infoExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder)
      example.assertBeforeAfterInfo(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder)
      example.assertBeforeAfterInfo(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the blocking test's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
    forAll(testTimeoutExamples) { example => 
      val inOrderEvents = withConcurrentDistributor(example, _.executeInOrder)
      example.assertTestTimeoutTest(inOrderEvents)
      val reverseOrderEvents = withConcurrentDistributor(example, _.executeInReverseOrder)
      example.assertTestTimeoutTest(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the events reported in correct order when multiple suite's tests are executed in parallel") {
    forAll(parallelExamples) { example => 
      val inOrderEvents = withConcurrentDistributor(example.suite1, example.suite2, Span(5, Seconds), _.executeInOrder)
      example.assertParallelSuites(inOrderEvents)
      val reverseOrderEvents = withConcurrentDistributor(example.suite1, example.suite2, Span(5, Seconds), _.executeInReverseOrder)
      example.assertParallelSuites(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the blocking suite's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
    forAll(suiteTimeoutExamples) { example =>
      val events = withConcurrentDistributor(example.suite1, example.suite2, Span(100, Millis), _.executeInOrder)
      example.assertSuiteTimeoutTest(events)
    }
  }
}