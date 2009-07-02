/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import org.scalatest.events._

trait SharedHelpers extends Assertions {
  
  class TestDurationReporter extends Reporter {
    var testSucceededWasFiredAndHadADuration = false
    var testFailedWasFiredAndHadADuration = false
    override def apply(event: Event) {
      event match {
        case event: TestSucceeded => testSucceededWasFiredAndHadADuration = event.duration.isDefined
        case event: TestFailed => testFailedWasFiredAndHadADuration = event.duration.isDefined
        case _ =>
      }
    }
  }

  class SuiteDurationReporter extends Reporter {
    var suiteCompletedWasFiredAndHadADuration = false
    var suiteAbortedWasFiredAndHadADuration = false
    override def apply(event: Event) {
      event match {
        case event: SuiteCompleted => suiteCompletedWasFiredAndHadADuration = event.duration.isDefined
        case event: SuiteAborted => suiteAbortedWasFiredAndHadADuration = event.duration.isDefined
        case _ =>
      }
    }
  }

  class PendingReporter extends Reporter {
    var testPendingWasFired = false
    override def apply(event: Event) {
      event match {
        case _: TestPending => testPendingWasFired = true
        case _ =>
      }
    }
  }

  class EventRecordingReporter extends Reporter {
    private var eventList: List[Event] = List()
    def eventsReceived = eventList.reverse
    def apply(event: Event) {
      eventList ::= event
    }
  }

  def runCommonInformerTestCode(suite: Suite, testName: String, infoMsg: String): (Int, Int, Int) = {

    val myRep = new EventRecordingReporter
    suite.run(None, myRep, new Stopper {}, Set(), Set(), Map(), None, new Tracker)

    val indexedList = myRep.eventsReceived.zipWithIndex

    val testStartingOption = indexedList.find(_._1.isInstanceOf[TestStarting])
    val infoProvidedOption = indexedList.find(_._1.isInstanceOf[InfoProvided])
    val testSucceededOption = indexedList.find(_._1.isInstanceOf[TestSucceeded])

    assert(testStartingOption.isDefined)
    assert(infoProvidedOption.isDefined)
    assert(testSucceededOption.isDefined)

    val testStartingIndex = testStartingOption.get._2
    val infoProvidedIndex = infoProvidedOption.get._2
    val testSucceededIndex = testSucceededOption.get._2

    val testStarting = testStartingOption.get._1.asInstanceOf[TestStarting]
    val infoProvided = infoProvidedOption.get._1.asInstanceOf[InfoProvided]
    val testSucceeded = testSucceededOption.get._1.asInstanceOf[TestSucceeded]

    assert(testStarting.testName === testName)
    assert(infoProvided.message === infoMsg)
    assert(testSucceeded.testName === testName)

    (infoProvidedIndex, testStartingIndex, testSucceededIndex)
  }
}
