package org.scalatest.tools

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.events.Ordinal
import org.scalatest.events.ScopeOpened
import org.scalatest.events.NameInfo
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.ScopeClosed
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.Tracker

class TestSortingReporterSpec extends FunSpec with ShouldMatchers {

  describe("TestSortingReporter") {
    
    val tracker = new Tracker()
    val scope1Opened = ScopeOpened(tracker.nextOrdinal, "Scope 1", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None, None))
    val scope2Opened = ScopeOpened(tracker.nextOrdinal, "Scope 2", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None, None))
    val s1s2t1Starting = TestStarting(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 1 Scope 2 Test 1", "Test 1", None)
    val s1s2t1Succeeded = TestSucceeded(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 1 Scope 2 Test 1", "Test 1", None, Vector.empty)
    val s1s2t2Starting = TestStarting(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 1 Scope 2 Test 2", "Test 2", None)
    val s1s2t2Succeeded = TestSucceeded(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 1 Scope 2 Test 2", "Test 2", None, Vector.empty)
    val s1s2t3Starting = TestStarting(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 1 Scope 2 Test 3", "Test 3", None)
    val s1s2t3Succeeded = TestSucceeded(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 1 Scope 2 Test 3", "Test 3", None, Vector.empty)
    val scope2Closed = ScopeClosed(tracker.nextOrdinal, "Scope 2", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None, None))
    val scope1Closed = ScopeClosed(tracker.nextOrdinal, "Scope 1", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None, None))
    
    val scope3Opened = ScopeOpened(tracker.nextOrdinal, "Scope 3", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None, None))
    val s3t1Starting = TestStarting(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 3 Test 1", "Test 1", None)
    val s3t1Succeeded = TestSucceeded(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 3 Test 1", "Test 1", None, Vector.empty)
    val s3t2Starting = TestStarting(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 3 Test 2", "Test 2", None)
    val s3t2Succeeded = TestSucceeded(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 3 Test 2", "Test 2", None, Vector.empty)
    val s3t3Starting = TestStarting(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 3 Test 3", "Test 3", None)
    val s3t3Succeeded = TestSucceeded(tracker.nextOrdinal, "aSuite", "aSuite", Some("a.b.aSuite"), None, "Scope 3 Test 3", "Test 3", None, Vector.empty)
    val scope3Closed = ScopeClosed(tracker.nextOrdinal, "Scope 3", NameInfo("aSuite", "aSuite", Some("a.b.aSuite"), None, None))
    
    it("should fire event passed to it in the order they arrive if waitForTestCompleted is not called.") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter(recordingReporter, Span(15, Seconds))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch(s1s2t1Starting)
      dispatch(s1s2t1Succeeded)
      dispatch(s1s2t2Starting)
      dispatch(s1s2t2Succeeded)
      dispatch(s1s2t3Starting)
      dispatch(s1s2t3Succeeded)
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      dispatch(scope3Opened)
      dispatch(s3t1Starting)
      dispatch(s3t1Succeeded)
      dispatch(s3t2Starting)
      dispatch(s3t2Succeeded)
      dispatch(s3t3Starting)
      dispatch(s3t3Succeeded)
      dispatch(scope3Closed)
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t1Starting)
      recordedEvents(3) should be (s1s2t1Succeeded)
      recordedEvents(4) should be (s1s2t2Starting)
      recordedEvents(5) should be (s1s2t2Succeeded)
      recordedEvents(6) should be (s1s2t3Starting)
      recordedEvents(7) should be (s1s2t3Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
      recordedEvents(10) should be (scope3Opened)
      recordedEvents(11) should be (s3t1Starting)
      recordedEvents(12) should be (s3t1Succeeded)
      recordedEvents(13) should be (s3t2Starting)
      recordedEvents(14) should be (s3t2Succeeded)
      recordedEvents(15) should be (s3t3Starting)
      recordedEvents(16) should be (s3t3Succeeded)
    }
    
    it("should wait and fire event based on the order of waitForTestCompleted is called.") {
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter(recordingReporter, Span(15, Seconds))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch(s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch(s1s2t2Starting)
      dispatch.distributingTest(s1s2t3Starting.testName)
      dispatch(s1s2t3Starting)
      dispatch(s1s2t3Succeeded)
      dispatch(s1s2t1Succeeded)
      dispatch(s1s2t2Succeeded)
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      dispatch(scope3Opened)
      dispatch.distributingTest(s3t1Starting.testName)
      dispatch(s3t1Starting)
      dispatch.distributingTest(s3t2Starting.testName)
      dispatch(s3t2Starting)
      dispatch.distributingTest(s3t3Starting.testName)
      dispatch(s3t3Starting)
      dispatch(s3t3Succeeded)
      dispatch(s3t1Succeeded)
      dispatch(s3t2Succeeded)
      dispatch(scope3Closed)
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t1Starting)
      recordedEvents(3) should be (s1s2t1Succeeded)
      recordedEvents(4) should be (s1s2t2Starting)
      recordedEvents(5) should be (s1s2t2Succeeded)
      recordedEvents(6) should be (s1s2t3Starting)
      recordedEvents(7) should be (s1s2t3Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
      recordedEvents(10) should be (scope3Opened)
      recordedEvents(11) should be (s3t1Starting)
      recordedEvents(12) should be (s3t1Succeeded)
      recordedEvents(13) should be (s3t2Starting)
      recordedEvents(14) should be (s3t2Succeeded)
      recordedEvents(15) should be (s3t3Starting)
      recordedEvents(16) should be (s3t3Succeeded)
    }
    
    it("should wait and fire blocking event when timeout, and just fire the missing event directly without waiting when received later.") {
    
      val recordingReporter = new EventRecordingReporter()
      val dispatch = new TestSortingReporter(recordingReporter, Span(3, Seconds))
      
      dispatch(scope1Opened)
      dispatch(scope2Opened)
      dispatch.distributingTest(s1s2t1Starting.testName)
      dispatch(s1s2t1Starting)
      dispatch.distributingTest(s1s2t2Starting.testName)
      dispatch(s1s2t2Starting)
      dispatch.distributingTest(s1s2t3Starting.testName)
      dispatch(s1s2t3Starting)
      dispatch(s1s2t3Succeeded)
      dispatch(s1s2t2Succeeded)
      
      Thread.sleep(4000)
      dispatch(s1s2t1Succeeded)
      
      dispatch(scope2Closed)
      dispatch(scope1Closed)
      
      
      
      val recordedEvents = recordingReporter.eventsReceived
      recordedEvents(0) should be (scope1Opened)
      recordedEvents(1) should be (scope2Opened)
      recordedEvents(2) should be (s1s2t1Starting)
      recordedEvents(3) should be (s1s2t2Starting)
      recordedEvents(4) should be (s1s2t2Succeeded)
      recordedEvents(5) should be (s1s2t3Starting)
      recordedEvents(6) should be (s1s2t3Succeeded)
      recordedEvents(7) should be (s1s2t1Succeeded)
      recordedEvents(8) should be (scope2Closed)
      recordedEvents(9) should be (scope1Closed)
    }
  }
  
}