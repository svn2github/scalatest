package org.scalatest
import org.scalatest.tools.SuiteRunner
import java.util.concurrent.CountDownLatch

trait Status {
   // Blocks until completed, then returns true if no tests failed and no suites aborted
  def succeeds(): Boolean // Not sure I need this? But it makes it easier to return a status.

  // returns immediately with true or false, a way to poll the Status
  def isCompleted: Boolean

  // Blocks until completed, then returns
  def waitUntilCompleted()  // For some reason I feel like this should have parens
}

final class SucceededStatus extends Status {
  def succeeds() = true
  def isCompleted = true
  def waitUntilCompleted() {} // returns immediately
}

final class FailedStatus extends Status {
  def succeeds() = false
  def isCompleted = true
  def waitUntilCompleted() {} // returns immediately
}

private[scalatest] final class ScalaTestStatefulStatus extends Status {
  private val latch = new CountDownLatch(1)
  @volatile private var succeed = true
  
  def succeeds() = {
    waitUntilCompleted()
    succeed
  }
  
  def isCompleted = latch.getCount() == 0L
  
  def waitUntilCompleted() {
    latch.await()
  }
  
  def fails() {
    succeed = false
  }
  
  def completes() {
    latch.countDown()
  }
}

final class StatefulStatus extends Status {
  private val latch = new CountDownLatch(1)
  @volatile private var succeed = true
  
  def succeeds() = {
    waitUntilCompleted()
    succeed
  }
  
  def isCompleted = latch.getCount() == 0L
  
  def waitUntilCompleted() {
    latch.await()
  }
  
  def fails() {
    succeed = false
  }
  
  def completes() {
    latch.countDown()
  }
}

final class CompositeStatus(statusSeq: IndexedSeq[Status]) extends Status {
  def succeeds() = statusSeq.forall(_.succeeds())
  def isCompleted = statusSeq.forall(_.isCompleted)
  def waitUntilCompleted() {
    statusSeq.foreach(_.waitUntilCompleted())
  }
}