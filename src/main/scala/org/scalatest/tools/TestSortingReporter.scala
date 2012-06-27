package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import org.scalatest.time.Span
import java.util.Timer
import java.util.TimerTask
import java.util.UUID

private[scalatest] class TestSortingReporter(dispatch: Reporter, timeout: Span) extends ResourcefulReporter with DistributedTestSorter {

  case class Slot(uuid: UUID, eventList: ListBuffer[Event], ready: Boolean)
  
  private val waitingBuffer = new ListBuffer[Slot]()
  private val slotMap = new collection.mutable.HashMap[String, Slot]()  // testName -> Slot
  @volatile private var completedTestCount = 0
  
  class TimeoutTask(val slot: Slot) extends TimerTask {
    override def run() {
      timeout()
    }
  }
  
  private val timer = new Timer()
  private var timeoutTask: Option[TimeoutTask] = None

  /**
   * Called to indicate a test is being distributed. The test will be reported
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
        case _: InfoProvided | _: MarkupProvided => 
          val slot = slotMap(testName)
          slot.eventList += event
          fireReadyEvents()
        case _ =>
          apply(event)
      }
    }
  }

  def completedTest(testName: String) {
    synchronized {
      val slot = slotMap(testName)
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
                slot.eventList += testStarting
              else
                dispatch(testStarting)
            case None => 
              dispatch(testStarting)
          }
        case testIgnored: TestIgnored => 
          slotMap.get(testIgnored.testName) match {
            case Some(slot) => 
              val slotIdx = waitingBuffer.indexOf(slot)
              if (slotIdx >= 0) 
                slot.eventList += testIgnored
              else
                dispatch(testIgnored)
            case None => 
              dispatch(testIgnored)
          }
        case testSucceeded: TestSucceeded => 
          handleTestCompleted(testSucceeded, testSucceeded.testName)
        case testFailed: TestFailed => 
          handleTestCompleted(testFailed, testFailed.testName)
        case testPending: TestPending => 
          handleTestCompleted(testPending, testPending.testName)
        case testCanceled: TestCanceled => 
          handleTestCompleted(testCanceled, testCanceled.testName)
        case scopeOpened: ScopeOpened =>
          handleSuiteEvent(scopeOpened)
        case scopeClosed: ScopeClosed =>
          handleSuiteEvent(scopeClosed)
        case infoProvided: InfoProvided =>
          handleSuiteEvent(infoProvided)
        case markupProvided: MarkupProvided =>
          handleSuiteEvent(markupProvided)
        case _ => 
          dispatch(event)
      }
      fireReadyEvents()
    }
  }
  
  private def handleSuiteEvent(event: Event) {
    val listBuffer = new ListBuffer[Event]()
    listBuffer += event
    val slot = Slot(UUID.randomUUID, listBuffer, true)
    waitingBuffer += slot
  }
  
  private def handleTestCompleted(event: Event, testName: String) {
    slotMap.get(testName) match {
      case Some(slot) =>
        val slotIdx = waitingBuffer.indexOf(slot)
        if (slotIdx >= 0) 
          slot.eventList += event
        else // could happen when timeout, just fire the test completed event.
          dispatch(event)
      case None => 
        dispatch(event)
    }
  }
  
  private def fireReadyEvents() {
    val (ready, pending) = {
      val ready = waitingBuffer.takeWhile(slot => slot.ready)
      (ready, waitingBuffer.drop(ready.size))
    }

    ready.foreach { slot => slot.eventList.foreach(dispatch(_)) }
    waitingBuffer.clear()
    waitingBuffer ++= pending
    if (waitingBuffer.size > 0) 
      scheduleTimeoutTask()
    else {
      timeoutTask match {
        case Some(task) => 
          task.cancel()
          timeoutTask = None
        case None =>
      }
    }
  }
  
  private def scheduleTimeoutTask() {
    val head = waitingBuffer.head
    timeoutTask match {
        case Some(task) => 
          if (head.uuid != task.slot.uuid) {
            task.cancel()
            timeoutTask = Some(new TimeoutTask(head))
            timer.schedule(timeoutTask.get, timeout.millisPart)
          }
        case None => 
          timeoutTask = Some(new TimeoutTask(head))
          timer.schedule(timeoutTask.get, timeout.millisPart)
      }
  }
  
  private def timeout() {
    synchronized {
      if (waitingBuffer.size > 0) {
        val head = waitingBuffer.head
        if (timeoutTask.get.slot.uuid == head.uuid) {
          val newSlot = head.copy(ready = true)
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