package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import org.scalatest.time.Span

private[scalatest] class SuiteSortingReporter(dispatch: Reporter) extends ResourcefulReporter with DistributedSuiteSorter {

  case class Slot(suiteId: String, doneEvent: Option[Event], includesDistributedTests: Boolean, testsCompleted: Boolean)

  @volatile private var slotListBuf = new ListBuffer[Slot]()
  private val slotMap = collection.mutable.HashMap[String, Slot]()
  // suiteEventMap is suite Id -> events for that suite (should be a Vector)
  private val suiteEventMap = collection.mutable.HashMap[String, Vector[Event]]()

  override def apply(event: Event) {
    try {
      synchronized {
        event match {
          case suiteStarting: SuiteStarting =>
            val slot = Slot(suiteStarting.suiteId, None, false, false)
            slotListBuf += slot // Why put this in the ListBuffer then throw an exception?
            slotMap.get(suiteStarting.suiteId) match {
              case Some(slot) =>
                throw new RuntimeException("2 SuiteStarting (" + slot.suiteId + ", " + suiteStarting.suiteId + ") having same suiteId '" + suiteStarting.suiteId + "'.")
              case None =>
                slotMap.put(suiteStarting.suiteId, slot)
            }
            handleTestEvents(suiteStarting.suiteId, suiteStarting)

          case suiteCompleted: SuiteCompleted =>
            handleSuiteEvents(suiteCompleted.suiteId, suiteCompleted)
          case suiteAborted: SuiteAborted =>
            handleSuiteEvents(suiteAborted.suiteId, suiteAborted)
          case testStarting: TestStarting =>
            handleTestEvents(testStarting.suiteId, testStarting)
          case testIgnored: TestIgnored =>
            handleTestEvents(testIgnored.suiteId, testIgnored)
          case testSucceeded: TestSucceeded =>
            handleTestEvents(testSucceeded.suiteId, testSucceeded)
          case testFailed: TestFailed =>
            handleTestEvents(testFailed.suiteId, testFailed)
          case testPending: TestPending =>
            handleTestEvents(testPending.suiteId, testPending)
          case testCanceled: TestCanceled =>
            handleTestEvents(testCanceled.suiteId, testCanceled)
          case infoProvided: InfoProvided =>
            infoProvided.nameInfo match {
              case Some(nameInfo) =>
                handleTestEvents(nameInfo.suiteID, infoProvided)
              case None => // Under what condition will reach here?
                dispatch(infoProvided)
            }
          case markupProvided: MarkupProvided =>
            markupProvided.nameInfo match {
              case Some(nameInfo) =>
                handleTestEvents(nameInfo.suiteID, markupProvided)
              case None => // Under what condition will reach here?
                dispatch(markupProvided)
            }
          case scopeOpened: ScopeOpened =>
            handleTestEvents(scopeOpened.nameInfo.suiteID, scopeOpened)
          case scopeClosed: ScopeClosed =>
            handleTestEvents(scopeClosed.nameInfo.suiteID, scopeClosed)
          case _ =>
            dispatch(event)  // Just dispatch it if got unexpected event.
        }
        fireReadyEvents()
      }
    }
    catch {
      case e: Exception =>
        val stringToPrint = Resources("reporterThrew", event)
        System.err.println(stringToPrint)
        e.printStackTrace(System.err)
    }
  }

  // Handles just SuiteCompleted and SuiteAborted
  private def handleSuiteEvents(suiteId: String, event: Event) {
    val slot = slotMap(suiteId)
    //slot.doneEvent = Some(event)
    val newSlot = slot.copy(doneEvent = Some(event))  // Assuming here that a done event hasn't already arrived
    slotMap.put(suiteId, newSlot)                     // Probably should fail on the second one
    val slotIdx = slotListBuf.indexOf(slot)
    if (slotIdx >= 0)                                 // In what case would it not be there?
      slotListBuf.update(slotIdx, newSlot)  // Why not fire ready events here? Oh, at end of apply
  }
  // Handles SuiteStarting, TestStarting, TestIgnored, TestSucceeded, TestFailed, TestPending,
  // TestCanceled, InfoProvided, MarkupProvided, ScopeOpened, ScopeClosed.
  private def handleTestEvents(suiteId: String, event: Event) {
    suiteEventMap.get(suiteId) match { // Can probably use the transform or some such method
      case Some(eventList) =>
        suiteEventMap.put(suiteId, eventList :+ event)
      case None =>                                     // oldest events at front of vector
        suiteEventMap.put(suiteId, Vector(event))
    }
    fireReadyEvents() // Then if at end of apply, why have it here too?
  }

  // Only called within synchronized
  private def fireReadyEvents() {
    if (slotListBuf.size > 0) {
      val head = slotListBuf.head
      fireSuiteEvents(head.suiteId)
      if (isDone(head)) {
        dispatch(head.doneEvent.get)  // Assuming it is existing again.
        slotListBuf = fireReadySuiteEvents(slotListBuf.tail)
      }
    }
  }

  // suiteId must exist in the suiteEventMap
  private def fireSuiteEvents(suiteId: String) {
    suiteEventMap.get(suiteId) match {
      case Some(eventList) =>
        eventList.foreach(dispatch(_)) // Fire all of them and empty it out. The done event is stored elsewhere
        suiteEventMap.put(suiteId, Vector.empty[Event]) // Just fire in order they appear. (Could sort them here.)
      case None =>
      // Unable to get event vector from map, shouldn't happen
    }
  }

  private def isDone(slot: Slot) = {
    if (slot.includesDistributedTests)
        slot.doneEvent.isDefined && slot.testsCompleted
    else
        slot.doneEvent.isDefined
/*
    slot.testSortingReporter match {    // This should just be a flag
      case Some(testSortingReporter) => // Only look at testCompleted flag if there's a TSR
        slot.doneEvent.isDefined && slot.testsCompleted
      case None =>
        slot.doneEvent.isDefined
    }
*/
  }

  private def fireReadySuiteEvents(remainingSlotList: ListBuffer[Slot]): ListBuffer[Slot] = {
    val (done, undone) = remainingSlotList.span(isDone(_)) // Grab all the done slots
    done.foreach {
      slot =>
        fireSuiteEvents(slot.suiteId)
        dispatch(slot.doneEvent.get)
    }
    undone
  }
  
  def completedTests(suiteId: String) {
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(testsCompleted = true)
    slotMap.put(suiteId, newSlot)
    val slotIdx = slotListBuf.indexOf(slot)
    if (slotIdx >= 0)
      slotListBuf.update(slotIdx, newSlot)
    fireReadyEvents()
  }

  // Will need a timeout. Hmm. Because can change it. Hmm. This is an issue. I wanted
  // suite's timeout to be 20% longer than the -T one. If an overridden sortingTimeout timeout is shorter, then
  // that's no prob. But if it is longer, then the suiteTimeout will timeout first. I think that's fine. I'll
  // just document that behavior.
  def distributingTests(suiteId: String) {
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(includesDistributedTests = true)
    slotMap.put(suiteId, newSlot)
    val slotIdx = slotListBuf.indexOf(slot)
    if (slotIdx >= 0)
      slotListBuf.update(slotIdx, newSlot)
  }
/*
  def distributingTests(suiteId: String, timeout: Span, testCount: Int) = {
    val testSortingReporter = new TestSortingReporter(suiteId, this, timeout, testCount, Some(this))
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(testSortingReporter = Some(testSortingReporter))
    slotMap.put(suiteId, newSlot)
    val slotIdx = slotList.indexOf(slot)
    if (slotIdx >= 0)
      slotList.update(slotIdx, newSlot)
    testSortingReporter
  }
*/
  
  // def getDistributedTestSorter(suiteId: String) = slotMap(suiteId).testSortingReporter.get

  override def dispose() = {
    try {
      fireReadyEvents()
    }
    catch {
      case e: Exception =>
        val stringToPrint = Resources("reporterDisposeThrew")
        System.err.println(stringToPrint)
        e.printStackTrace(System.err)
    }
  }
}
