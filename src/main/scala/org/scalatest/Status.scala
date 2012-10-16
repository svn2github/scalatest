package org.scalatest
import org.scalatest.tools.SuiteRunner
import java.util.concurrent.CountDownLatch

/**
 * Trait that represents the run status of a test or a suite.  It can be used to check if a test or a suite run is completed, and if it succeeded or failed.
 * 
 * @author cheeseng
 */
trait Status {
  
  /**
   * Check if the run status succeeds, this call blocks until the test or suite run completes and returns <code>true</code> if no tests failed and no suites aborted, 
   * <code>false</code> otherwise.
   * 
   * @return <code>true</code> if no tests failed and no suites aborted, <code>false</code> otherwise
   */
  def succeeds(): Boolean

  /**
   * Non-blocking call to check if the test or suite run is completed, returns <code>true</code> if the test or suite run is already completed, 
   * <code>false</code> otherwise.  You can use this to poll the run status.
   * 
   * @return <code>true</code> if the test or suite run is already completed, <code>false</code> otherwise.
   */
  def isCompleted: Boolean

  /**
   * A blocking call that return only after the underlying test or suite is completed.
   */
  def waitUntilCompleted()
}

/**
 * Singleton status that represents a completed run with no tests failed and no suites aborted.
 */
object SucceededStatus extends Status {
  /**
   * Always return <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def succeeds() = true
  
  /**
   * Always return <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def isCompleted = true
  
  /**
   * Returns immediately, as the run is already completed.
   */
  def waitUntilCompleted() {}
}

/**
 * Singleton status that represents a completed run with at least one failed test or aborted suite.
 */
object FailedStatus extends Status {
  /**
   * Always return <code>false</code>.
   * 
   * @return <code>true</code>
   */
  def succeeds() = false
  
  /**
   * Always return <code>true</code>.
   * 
   * @return <code>true</code>
   */
  def isCompleted = true
  
  /**
   * Returns immediately, as the run is already completed.
   */
  def waitUntilCompleted() {}
}

// Used internally in ScalaTest
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
  
  def setFailed() {
    succeed = false
  }
  
  def setCompleted() {
    latch.countDown()
  }
}

/**
 * Status implementation that could change its state over time.
 */
final class StatefulStatus extends Status {
  private val latch = new CountDownLatch(1)
  @volatile private var succeed = true
  
  /**
   * Check if the run status succeeds, this call blocks until the test or suite run completes and returns <code>true</code> if no tests failed and no suites aborted, 
   * <code>false</code> otherwise.
   * 
   * @return <code>true</code> if no tests failed and no suites aborted, <code>false</code> otherwise
   */
  def succeeds() = {
    waitUntilCompleted()
    succeed
  }
  
  /**
   * Non-blocking call to check if the test or suite run is completed, returns <code>true</code> if the test or suite run is already completed, 
   * <code>false</code> otherwise.  You can use this to poll the run status.
   * 
   * @return <code>true</code> if the test or suite run is already completed, <code>false</code> otherwise.
   */
  def isCompleted = latch.getCount() == 0L
  
  /**
   * A blocking call that return only after the underlying test or suite is completed.
   */
  def waitUntilCompleted() {
    latch.await()
  }
  
  /**
   * Set the status to failed.
   */
  def setFailed() {
    succeed = false
  }
  
  /**
   * Set the status to completed.
   */
  def setCompleted() {
    latch.countDown()
  }
}

/**
 * Composite status that determine its status based on composition of other status, passed at construction time.
 */
final class CompositeStatus(statusSeq: Seq[Status]) extends Status {
  private val statuses = statusSeq.toIndexedSeq
  
  /**
   * Check if the run status succeeds, this call blocks until all composition status succeeds or at least one failed.
   * 
   * @return <code>true</code> if all composition status succeeds, <code>false</code> otherwise
   */
  def succeeds() = statuses.forall(_.succeeds())
  
  /**
   * Non-blocking call to check if the test or suite run is completed, returns <code>true</code> if all composition status are completed, 
   * <code>false</code> otherwise.  You can use this to poll the run status.
   * 
   * @return <code>true</code> if all composition status are completed, <code>false</code> otherwise.
   */
  def isCompleted = statuses.forall(_.isCompleted)
  
  /**
   * A blocking call that return only after all composition status are completed.
   */
  def waitUntilCompleted() {
    statuses.foreach(_.waitUntilCompleted())
  }
}