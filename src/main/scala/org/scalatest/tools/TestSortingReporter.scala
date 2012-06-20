package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import org.scalatest.time.Span
import java.util.Timer
import java.util.TimerTask
import java.util.UUID

private[scalatest] class TestSortingReporter(dispatch: Reporter, timeout: Span) extends ResourcefulReporter {

  case class Slot(uuid: UUID, startEvent: Option[Event], completedEvent: Option[Event], ready: Boolean) extends Ordered[Slot] {
    def compare(that: Slot): Int = //ordinal.compare(that.ordinal)
      if (startEvent.isDefined && that.startEvent.isDefined)
        startEvent.get compare that.startEvent.get
      else
        0
  }
  
  private val waitingBuffer = new ListBuffer[Slot]()
  private val slotMap = new collection.mutable.HashMap[String, Slot]()  // testName -> Slot
  
  class TimeoutTask(val slot: Slot) extends TimerTask {
    override def run() {
      timeout()
    }
  }
  
  private val timer = new Timer()
  private var timeoutTask: Option[TimeoutTask] = None
  
  def waitForTestCompleted(testName: String) {
    synchronized {
      val slot = Slot(UUID.randomUUID, None, None, false)
      waitingBuffer += slot
      slotMap.put(testName, slot)
    }
  }
  
  override def apply(event: Event) {
    synchronized {
      event match {
        case testStarting: TestStarting => 
          slotMap.get(testStarting.testName) match {
            case Some(slot) => 
              val slotIdx = waitingBuffer.indexOf(slot)
              if (slotIdx >= 0) {
                val newSlot = slot.copy(startEvent = Some(testStarting))
                waitingBuffer.update(slotIdx, newSlot)
                slotMap.put(testStarting.testName, newSlot)
              }
              else
                dispatch(testStarting)
            case None => 
              dispatch(testStarting)
          }
        case testIgnored: TestIgnored => 
          slotMap.get(testIgnored.testName) match {
            case Some(slot) => 
              val slotIdx = waitingBuffer.indexOf(slot)
              if (slotIdx >= 0) {
                val newSlot = slot.copy(startEvent = Some(testIgnored), ready = true)
                waitingBuffer.update(slotIdx, newSlot)
                slotMap.put(testIgnored.testName, newSlot)
              }
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
    val slot = Slot(UUID.randomUUID, Some(event), None, true)
    waitingBuffer += slot
  }
  
  private def handleTestCompleted(event: Event, testName: String) {
    slotMap.get(testName) match {
      case Some(slot) =>
        val slotIdx = waitingBuffer.indexOf(slot)
        if (slotIdx >= 0) {
          val newSlot = slot.copy(completedEvent = Some(event), ready = true)
          waitingBuffer.update(slotIdx, newSlot)
          slotMap.put(testName, newSlot)
        }
        else // could happen when timeout, just fire the test completed event.
          dispatch(event)
      case None => 
        dispatch(event)
    }
  }
  
  private def fireReadyEvents() {
    // Check if there's InfoProvided or MarkupProvided in the buffer.
    val infoMarkupOpt = waitingBuffer.find { slot =>
      if (slot.startEvent.isDefined) 
        slot.startEvent.get.isInstanceOf[InfoProvided] || slot.startEvent.get.isInstanceOf[MarkupProvided]
      else
        false
    }
    
    // We need to sort the buffer based on ordinal if there's InfoProvided or MarkupProvided in the buffer, 
    // this is because the InfoProvided can be fired from runTest, but before TestStarting (e.g. info in before 
    // of BeforeAndAfter trait)
    infoMarkupOpt match {
      case Some(slot) => 
        val sortedBuffer = waitingBuffer.sortWith((a, b) => a < b)
        waitingBuffer.clear()
        waitingBuffer ++= sortedBuffer
      case None =>
    }
    
    val (ready, pending) = waitingBuffer.span(slot => slot.ready)
    ready.foreach { slot => 
      dispatch(slot.startEvent.get)
      slot.completedEvent match {
        case Some(completedEvent) => 
          dispatch(completedEvent)
        case None =>
      }
    }
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