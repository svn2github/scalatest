package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import org.scalatest.time.Span
import java.util.Timer
import java.util.TimerTask
import java.util.UUID

private[scalatest] class TestSortingReporter(dispatch: Reporter, sortingTimeout: Span) extends ResourcefulReporter with DistributedTestSorter {

  // Each test gets one slot
  case class Slot(uuid: UUID, eventList: ListBuffer[Event], ready: Boolean)
  
  private val waitingBuffer = new ListBuffer[Slot]()
  private val slotMap = new collection.mutable.HashMap[String, Slot]()  // testName -> Slot
  @volatile private var completedTestCount = 0 // Called within synchronized. Don't need volatile and it wouldn't work anyway.

  // Passed slot will always be the head of waitingBuffer
  class TimeoutTask(val slot: Slot) extends TimerTask {
    override def run() {
      timeout()
    }
  }
  
  private val timer = new Timer()
  private var timeoutTask: Option[TimeoutTask] = None

  /**
   * Called to indicate a test is being distributed. The tests will be reported
   * in the order this is invoked.
   *
   * @param testName the name of the test being distributed
   */
  def distributingTest(testName: String) {
    synchronized {
      val slot = Slot(UUID.randomUUID, new ListBuffer[Event](), false)
      slotMap.put(testName, slot)
      waitingBuffer += slot
    }
  }

  def apply(testName: String, event: Event) {
    synchronized {
      event match {
        case _: InfoProvided | _: MarkupProvided =>  // This can happen if there's an info in before or after. Inside the test these will be recorded. Oh, it will also happen if multi-threaded info's going out from the test.
          val slot = slotMap(testName)  // How do you know that a slot exists?
          slot.eventList += event // Need to handle the error case better. Deal with no slot found.
          fireReadyEvents()
        case _ =>
          apply(event)
      }
    }
  }

  def completedTest(testName: String) {
    synchronized {
      val slot = slotMap(testName)  // How do you know a slot exists?
      val newSlot = slot.copy(ready = true)
      val slotIdx = waitingBuffer.indexOf(slot)
      if (slotIdx >= 0)
        waitingBuffer.update(slotIdx, newSlot)
      slotMap.put(testName, newSlot)
      completedTestCount += 1
      fireReadyEvents()
    }
  }

  override def apply(event: Event) {
    synchronized {
      event match {
        case testStarting: TestStarting => 
          slotMap.get(testStarting.testName) match {
            case Some(slot) =>
              val slotIdx = waitingBuffer.indexOf(slot)
              if (slotIdx >= 0) 
                slot.eventList += testStarting // Normally, insert the TestStarting in the eventList for that test
              else // slot not in waiting buffer. what case is this? Probably an err condition
                dispatch(testStarting) // calling dispatch inside a synchronized, so could be slow
            case None => 
              dispatch(testStarting) // a test that wasn't announced
          }
        case testIgnored: TestIgnored => // Is there a test for an ignored test?
          slotMap.get(testIgnored.testName) match {
            case Some(slot) => 
              val slotIdx = waitingBuffer.indexOf(slot)
              if (slotIdx >= 0) 
                slot.eventList += testIgnored // Similar to above, normrally, insert the TestIgnored in the event list
              else
                dispatch(testIgnored)
            case None => 
              dispatch(testIgnored)
          }

        // "test completed events"
        case testSucceeded: TestSucceeded => 
          handleTestCompleted(testSucceeded, testSucceeded.testName)
        case testFailed: TestFailed => 
          handleTestCompleted(testFailed, testFailed.testName)
        case testPending: TestPending => 
          handleTestCompleted(testPending, testPending.testName)
        case testCanceled: TestCanceled => 
          handleTestCompleted(testCanceled, testCanceled.testName)

        // "suite events"
        case scopeOpened: ScopeOpened =>
          handleSuiteEvent(scopeOpened)
        case scopeClosed: ScopeClosed =>
          handleSuiteEvent(scopeClosed)
        case infoProvided: InfoProvided =>
          handleSuiteEvent(infoProvided)
        case markupProvided: MarkupProvided =>
          handleSuiteEvent(markupProvided)

        // unexpected things like RunStarting, etc.
        case _ => 
          dispatch(event)
      }
      fireReadyEvents() // Always call fire ready events
    }
  }

  // This is either ScopeOpened, ScopeClosed, InfoProvided, or MarkupProvided
  // Called within synchronized
  private def handleSuiteEvent(event: Event) {
    val listBuffer = new ListBuffer[Event]()
    listBuffer += event
    val slot = Slot(UUID.randomUUID, listBuffer, true)  // Already ready already!
    waitingBuffer += slot
    // This is outside a test. That's why he calls it a SuiteEvent.
    // If inside a test, it comes through the other apply.
  }


  // Also called within synchronized
  private def handleTestCompleted(event: Event, testName: String) {
    slotMap.get(testName) match {
      case Some(slot) =>
        val slotIdx = waitingBuffer.indexOf(slot)
        if (slotIdx >= 0) 
          slot.eventList += event // Normal path is just add it. Note not ready until completedTest call.
        else // could happen when timeout, just fire the test completed event.
          dispatch(event) // Yup. Might be not there anymore.
      case None => 
        dispatch(event)
    }
  }

  // I see that slots are appended to the waitingBuffer, so first come first served here.
  // By the way, this must be called within synchronized only
  private def fireReadyEvents() { // Isn't there a better method than takeWhile?
    val (ready, pending) = {
      val ready = waitingBuffer.takeWhile(slot => slot.ready)
      (ready, waitingBuffer.drop(ready.size))
    }

    // Again, dispatching inside a synchronized.
    ready.foreach { slot => slot.eventList.foreach(dispatch(_)) }
    waitingBuffer.clear() // Seems better to drop ready.size?
    waitingBuffer ++= pending
    if (waitingBuffer.size > 0) 
      scheduleTimeoutTask()
    else {
      timeoutTask match { // Waiting buffer is zero, so no timeout needed
        case Some(task) => 
          task.cancel()
          timeoutTask = None
        case None =>
      }
    }
  }

  // Also happening inside synchronized block
  private def scheduleTimeoutTask() {
    val head = waitingBuffer.head  // Assumes waitingBuffer is non-empty. Put a require there to make that obvious.
    timeoutTask match {
        case Some(task) => 
          if (head.uuid != task.slot.uuid) {
            task.cancel()
            timeoutTask = Some(new TimeoutTask(head)) // Replace the old with the new
            timer.schedule(timeoutTask.get, sortingTimeout.millisPart)
          }
        case None => 
          timeoutTask = Some(new TimeoutTask(head)) // Just create a new one
          timer.schedule(timeoutTask.get, sortingTimeout.millisPart)
      }
  }
  
  private def timeout() {
    synchronized {
      if (waitingBuffer.size > 0) {
        val head = waitingBuffer.head
        if (timeoutTask.get.slot.uuid == head.uuid) { // Probably a double check, or just in case there's race condition
          val newSlot = head.copy(ready = true) // Essentially, if time out, just say that one is ready. This test's events go out, and
          waitingBuffer.update(0, newSlot)
        }
        fireReadyEvents()
      }
    }
  }
  
  override def dispose() = {
    fireReadyEvents()
    propagateDispose(dispatch)
  }
}