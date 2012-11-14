package org.scalatest.tools

import scala.collection.mutable.ListBuffer
import org.scalatest.events.Summary

private[scalatest] class SuiteResultHolder {

  val suiteList = new ListBuffer[SuiteResult]()
  
  def +=(result: SuiteResult) {
    suiteList += result
  }
  
  def summary: Summary = {
    val (succeeded, failed, ignored, pending, canceled) = suiteList.foldLeft((0, 0, 0, 0, 0)) { case ((succeeded, failed, ignored, pending, canceled), r) =>
      (succeeded + r.testsSucceededCount, failed + r.testsFailedCount, ignored + r.testsIgnoredCount, 
       pending + r.testsPendingCount, canceled + r.testsCanceledCount)
    }
    Summary(succeeded, failed, ignored, pending, canceled, suiteList.length, suiteList.filter(!_.isCompleted).length)
  }
  
  def totalDuration: Long = suiteList.map(s => if (s.duration.isDefined) s.duration.get else 0).sum
}