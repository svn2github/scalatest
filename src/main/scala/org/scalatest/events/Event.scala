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
package org.scalatest.events

import org.scalatest._
import java.util.Date

/**
 * A base class for the events that can be passed to the report function passed
 * to the <code>execute</code> method of a <code>Suite</code>.
 *
 * @author Bill Venners
 */
sealed abstract class Event extends Ordered[Event] {

  /**
   * An <code>Ordinal</code> that can be used to place this event in order in the context of
   * other events reported during the same run.
   */
  val ordinal: Ordinal

  /**
   * An optional formatter that provides extra information that can be used by reporters in determining
   * how to present this event to the user.
   */
  val formatter: Option[Formatter]

  /**
   * An optional location that provides information indicating where in the source code an event originated.
   * IDEs can use this information, for example, to allow the user to hop from an event report to the relevant
   * line of source code.
   */
  val location: Option[Location]

  /**
   * An optional object that can be used to pass custom information to the reporter about this event.
   */
  val payload: Option[Any]

  /**
   * A name for the <code>Thread</code> about whose activity this event was reported.
   */
  val threadName: String

  /**
   * A <code>Long</code> indicating the time this event was reported, expressed in terms of the
   * number of milliseconds since the standard base time known as "the epoch":
   * January 1, 1970, 00:00:00 GMT.
   */
  val timeStamp: Long

  /**
   * Comparing <code>this</code> event with the event passed as <code>that</code>. Returns
   * x, where x < 0 iff this < that, x == 0 iff this == that, x > 0 iff this > that.
   *
   * @param that the event to compare to this event
   * @param return an integer indicating whether this event is less than, equal to, or greater than
   * the passed event
   */
  def compare(that: Event): Int = ordinal.compare(that.ordinal)
}

/**
 * Event that indicates a suite (or other entity) is about to start running a test.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestStarting</code> to report
 * that a test method of a <code>Suite</code> is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestStarting(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that is starting, suitable for presenting to the user
 * @param suiteID a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
 * @param testName the name of the test that is starting
 * @param testText the text of the test that is starting (may be the test name, or a suffix of the test name)
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that is starting (if <code>None</code>
 *        is passed, the test cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestStarting (
  ordinal: Ordinal,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[Rerunner] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a suite (or other entity) has completed running a test that succeeded.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestSucceeded</code> to report
 * that a test method of a <code>Suite</code> returned normally
 * (without throwing an <code>Exception</code>).
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestSucceeded$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestSucceeded</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestSucceeded(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that has succeeded, suitable for presenting to the user
 * @param suiteID a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
 * @param testName the name of the test that has succeeded
 * @param testText the text of the test that has succeeded (may be the test name, or a suffix of the test name)
 * @param duration an optional amount of time, in milliseconds, that was required to run the test that has succeeded
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that has succeeded (if <code>None</code>
 *        is passed, the test cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestSucceeded</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestSucceeded (
  ordinal: Ordinal,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[Rerunner] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a suite (or other entity) has completed running a test that failed.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestFailed</code> to report
 * that a test method of a <code>Suite</code> completed abruptly with an <code>Exception</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestFailed$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestFailed</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestFailed(ordinal, userFriendlyName, message, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param suiteName a localized name identifying the suite containing the test that has failed, suitable for presenting to the user
 * @param suiteID a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
 * @param testName the name of the test that has failed
 * @param testText the text of the test that has failed (may be the test name, or a suffix of the test name)
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required to run the test that has failed
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that has failed (if <code>None</code>
 *        is passed, the test cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestFailed</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestFailed (
  ordinal: Ordinal,
  message: String,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  throwable: Option[Throwable] = None,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[Rerunner] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a suite (or other entity) has ignored a test.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestIgnored</code> to report
 * that a test method of a <code>Suite</code> was ignored because it was annotated with <code>@Ignore</code>. 
 * Ignored tests will not be run, but will usually be reported as reminder to fix the broken test.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestIgnored$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestIgnored</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestIgnored(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that was ignored, suitable for presenting to the user
 * @param suiteID a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was ignored
 * @param testName the name of the test that was ignored
 * @param testText the text of the test that was ignored (may be the test name, or a suffix of the test name)
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestIgnored</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestIgnored (
  ordinal: Ordinal,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {
    
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a test is pending, <em>i.e.</em>, it hasn't yet been implemented.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestPending$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestPending</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestPending(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that is pending, suitable for presenting to the user
 * @param suiteID a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is pending
 * @param testName the name of the test that is pending
 * @param testText the text of the test that is pending (may be the test name, or a suffix of the test name)
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestPending</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestPending (
  ordinal: Ordinal,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a test was canceled, <em>i.e.</em>, it couldn't run because some precondition was not met.
 *
 * <p>
 * To create instances of this class you may
 * use the factory methods provided in its <a href="TestCanceled$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestCanceled</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestPending(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that was canceled, suitable for presenting to the user
 * @param suiteID a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was canceled
 * @param testName the name of the test that was canceled
 * @param testText the text of the test that was canceled (may be the test name, or a suffix of the test name)
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestCanceled</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
// TODO: Probably add a rerunnable to TestCanceled
final case class TestCanceled (
  ordinal: Ordinal,
  message: String,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  throwable: Option[Throwable] = None,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a suite of tests is about to start executing.
 *
 * <p>
 * For example, trait <code>Suite</code> and object <code>Runner</code> use <code>SuiteStarting</code> to report
 * that the <code>execute</code> method of a <code>Suite</code> is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="SuiteStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>SuiteStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(SuiteStarting(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName)))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite that is starting, suitable for presenting to the user
 * @param suiteID a string ID for the suite that is starting, intended to be unique across all suites in a run XXX 
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name of the suite that is starting
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that is starting (if <code>None</code>
 *        is passed, the suite cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class SuiteStarting (
  ordinal: Ordinal,
  suiteName: String,
  suiteClassName: Option[String],
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[Rerunner] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a suite of tests has completed executing.
 *
 * <p>
 * For example, trait <code>Suite</code> and object <code>Runner</code> use <code>SuiteCompleted</code> to report
 * that the <code>execute</code> method of a <code>Suite</code>
 * has returned normally (without throwing a <code>RuntimeException</code>).
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="SuiteCompleted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>SuiteCompleted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(SuiteCompleted(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName)))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite that has completed, suitable for presenting to the user
 * @param suiteID a string ID for the suite that has completed, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
 * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has completed
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that has completed (if <code>None</code>
 *        is passed, the suite cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteCompleted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class SuiteCompleted (
  ordinal: Ordinal,
  suiteName: String,
  suiteClassName: Option[String],
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[Rerunner] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates the execution of a suite of tests has aborted, likely because of an error, prior
 * to completion.
 *
 * <p>
 * For example, trait <code>Suite</code> and object <code>Runner</code> use <code>SuiteAborted</code> to report
 * that the <code>execute</code> method of a <code>Suite</code>
 * has completed abruptly with a <code>RuntimeException</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="SuiteAborted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>SuiteAborted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(SuiteAborted(ordinal, userFriendlyName, message, suiteName, Some(thisSuite.getClass.getName)))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param name a localized name identifying the suite that has aborted, which should include the
 *        suite name, suitable for presenting to the user
 * @param message a localized message suitable for presenting to the user
 * @param suiteName a localized name identifying the suite that has aborted, suitable for presenting to the user
 * @param suiteID a string ID for the suite that has aborted, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has aborted
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that has aborted (if <code>None</code>
 *        is passed, the suite cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteAborted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class SuiteAborted (
  ordinal: Ordinal,
  message: String,
  suiteName: String,
  suiteClassName: Option[String],
  throwable: Option[Throwable] = None,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[Rerunner] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

// TODO: Put location as a val set to None
/**
 * Event that indicates a runner is about run a suite of tests.
 *
 * <p>
 * For example, object <code>Runner</code> reports <code>RunStarting</code> to indicate
 * that the first <code>execute</code> method of a run's initial <code>Suite</code>
 * is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunStarting(ordinal, testCount))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param testCount the number of tests expected during this run
 * @param configMap a <code>Map</code> of key-value pairs that can be used by custom <code>Reporter</code>s
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @throws IllegalArgumentException if <code>testCount</code> is less than zero.
 *
 * @author Bill Venners
 */
final case class RunStarting (
  ordinal: Ordinal,
  testCount: Int,
  configMap: Map[String, Any],
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {
    
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (testCount < 0)
    throw new IllegalArgumentException("testCount was less than zero: " + testCount)
  if (configMap == null)
    throw new NullPointerException("configMap was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a runner has completed running a suite of tests.
 *
 * <p>
 * <code>Suite</code>'s <code>execute</code> method takes a <code>Stopper</code>, whose <code>stopRequested</code>
 * method indicates a stop was requested. If <code>true</code> is returned by
 * <code>stopRequested</code> while a suite of tests is running, the
 * <code>execute</code> method should promptly
 * return even if that suite hasn't finished running all of its tests.
 * </p>
 *
 * <p>If a stop was requested via the <code>Stopper</code>.
 * <code>Runner</code> will report <code>RunStopped</code>
 * when the <code>execute</code> method of the run's starting <code>Suite</code> returns.
 * If a stop is not requested, <code>Runner</code> will report <code>RunCompleted</code>
 * when the last <code>execute</code> method of the run's starting <code>Suite</code>s returns.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunCompleted</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunCompleted</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunCompleted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunCompleted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunCompleted(ordinal))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunCompleted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class RunCompleted (
  ordinal: Ordinal,
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a runner has stopped running a suite of tests prior to completion, likely
 * because of a stop request.
 *
 * <p>
 * <code>Suite</code>'s <code>execute</code> method takes a <code>Stopper</code>, whose <code>stopRequested</code>
 * method indicates a stop was requested. If <code>true</code> is returned by
 * <code>stopRequested</code> while a suite of tests is running, the
 * <code>execute</code> method should promptly
 * return even if that suite hasn't finished running all of its tests.
 * </p>
 *
 * <p>If a stop was requested via the <code>Stopper</code>.
 * <code>Runner</code> will report <code>RunStopped</code>
 * when the <code>execute</code> method of the run's starting <code>Suite</code> returns.
 * If a stop is not requested, <code>Runner</code> will report <code>RunCompleted</code>
 * when the last <code>execute</code> method of the run's starting <code>Suite</code>s returns.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunStopped</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunStopped</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunStopped$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunStopped</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunStopped(ordinal))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has stopped
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStopped</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class RunStopped (
  ordinal: Ordinal,
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a runner encountered an error while attempting to run a suite of tests.
 *
 * <p>
 * For example, object <code>Runner</code> reports <code>RunAborted</code> if the
 * <code>execute</code> method of any of the run's starting <code>Suite</code>s completes
 * abruptly with a <code>Throwable</code>.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunAborted</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunAborted</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunAborted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunAborted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunAborted(ordinal, message, Some(exception)))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has aborted
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunAborted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class RunAborted (
  ordinal: Ordinal,
  message: String,
  throwable: Option[Throwable],
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event used to provide information that is not appropriate to report via any other <code>Event</code>.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="InfoProvided$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>InfoProvided</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(InfoProvided(ordinal, message, Some(NameInfo(suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * An <code>InfoProvided</code> event may be fired from anywhere. In this respect <code>InfoProvided</code> is different
 * from the other events, for which it is defined whether they are fired in the context of a suite or test.
 * If fired in the context of a test, the <code>InfoProvided</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>InfoProvided</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined. If fired within the context
 * of neither a suite nor a test, the <code>nameInfo</code> of the <code>InfoProvided</code> event (an <code>Option[NameInfo]</code>) should be <code>None</code>.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test 
 *        in the context of which the information was provided
 * @param aboutAPendingTest indicates whether the information being provided via this event is about a pending test
 * @param aboutACanceledTest indicates whether the information being provided via this event is about a canceled test
 * @param throwable an optional <code>Throwable</code>
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>InfoProvided</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class InfoProvided (
  ordinal: Ordinal,
  message: String,
  nameInfo: Option[NameInfo],
  aboutAPendingTest: Option[Boolean] = None,
  aboutACanceledTest: Option[Boolean] = None,
  throwable: Option[Throwable] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (aboutAPendingTest == null)
    throw new NullPointerException("aboutAPendingTest was null")
  if (aboutACanceledTest == null)
    throw new NullPointerException("aboutACanceledTest was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event used to provide markup text for document-style reports.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="MarkupProvided$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>MarkupProvided</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(MarkupProvided(ordinal, text, Some(NameInfo(suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>MarkupProvided</code> event may be fired from anywhere. In this respect <code>MarkupProvided</code> is different
 * from the other events, for which it is defined whether they are fired in the context of a suite or test.
 * If fired in the context of a test, the <code>MarkupProvided</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>MarkupProvided</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined. If fired within the context
 * of neither a suite nor a test, the <code>nameInfo</code> of the <code>MarkupProvided</code> event (an <code>Option[NameInfo]</code>) should be <code>None</code>.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param text a snippet of markup text (in Markdown format)
 * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test 
 *        in the context of which the information was provided
 * @param aboutAPendingTest indicates whether the information being provided via this event is about a pending test
 * @param aboutACanceledTest indicates whether the information being provided via this event is about a canceled test
 * @param throwable an optional <code>Throwable</code>
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>MarkupProvided</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class MarkupProvided (
  ordinal: Ordinal,
  text: String,
  nameInfo: Option[NameInfo],
  aboutAPendingTest: Option[Boolean] = None,
  aboutACanceledTest: Option[Boolean] = None,
  throwable: Option[Throwable] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (text == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (aboutAPendingTest == null)
    throw new NullPointerException("aboutAPendingTest was null")
  if (aboutACanceledTest == null)
    throw new NullPointerException("aboutACanceledTest was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a new scope has been opened.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="ScopeOpened$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>ScopeOpened</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(ScopeOpened(ordinal, message, Some(NameInfo(suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>ScopeOpened</code> event may be fired from within suites or tests. 
 * If fired in the context of a test, the <code>ScopeOpened</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>ScopeOpened</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo a <code>NameInfo</code> that provides names for the suite and optionally the test 
 *        in the context of which the scope was opened
 * @param aboutAPendingTest indicates whether the scope was opened in the context of a pending test
 * @param aboutACanceledTest indicates whether the scope was opened in the context of a canceled test
 * @param throwable an optional <code>Throwable</code>
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>ScopeOpened</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class ScopeOpened (
  ordinal: Ordinal,
  message: String,
  nameInfo: NameInfo,
  aboutAPendingTest: Option[Boolean] = None,
  aboutACanceledTest: Option[Boolean] = None,
  throwable: Option[Throwable] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (aboutAPendingTest == null)
    throw new NullPointerException("aboutAPendingTest was null")
  if (aboutACanceledTest == null)
    throw new NullPointerException("aboutACanceledTest was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Event that indicates a scope has been closed.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="ScopeClosed$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>ScopeClosed</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(ScopeClosed(ordinal, message, Some(NameInfo(suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>ScopeClosed</code> event may be fired from within suites or tests. 
 * If fired in the context of a test, the <code>ScopeClosed</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>ScopeClosed</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo a <code>NameInfo</code> that provides names for the suite and optionally the test 
 *        in the context of which the scope was closed
 * @param aboutAPendingTest indicates whether the scope was closed in the context of a pending test
 * @param aboutACanceledTest indicates whether the scope was closed in the context of a canceled test
 * @param throwable an optional <code>Throwable</code>
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>ScopeClosed</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class ScopeClosed (
  ordinal: Ordinal,
  message: String,
  nameInfo: NameInfo,
  aboutAPendingTest: Option[Boolean] = None,
  aboutACanceledTest: Option[Boolean] = None,
  throwable: Option[Throwable] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (aboutAPendingTest == null)
    throw new NullPointerException("aboutAPendingTest was null")
  if (aboutACanceledTest == null)
    throw new NullPointerException("aboutACanceledTest was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/*

/**
 * Event that indicates a runner is about run a suite of tests.
 *
 * <p>
 * For example, object <code>Runner</code> reports <code>RunStarting</code> to indicate
 * that the first <code>execute</code> method of a run's initial <code>Suite</code>
 * is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunStarting(ordinal, testCount))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param testCount the number of tests expected during this run
 * @param configMap a <code>Map</code> of key-value pairs that can be used by custom <code>Reporter</code>s
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @throws IllegalArgumentException if <code>testCount</code> is less than zero.
 *
 * @author Bill Venners
 */
final case class DiscoveryStarting (
  ordinal: Ordinal,
  testCount: Int,
  configMap: Map[String, Any],
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {
 
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (testCount < 0)
    throw new IllegalArgumentException("testCount was less than zero: " + testCount)
  if (configMap == null)
    throw new NullPointerException("configMap was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")

  /**
   * Location in a <code>DiscoveryStarting</code> is always set to <code>None</code>.
   */
  val location: Option[Location] = None

  /**
   * Formatter in a <code>DiscoveryStarting</code> is always set to <code>None</code>.
   */
  val formatter: Option[Formatter] = None
}

/**
 * Event that indicates a runner has completed running a suite of tests.
 *
 * <p>
 * <code>Suite</code>'s <code>execute</code> method takes a <code>Stopper</code>, whose <code>stopRequested</code>
 * method indicates a stop was requested. If <code>true</code> is returned by
 * <code>stopRequested</code> while a suite of tests is running, the
 * <code>execute</code> method should promptly
 * return even if that suite hasn't finished running all of its tests.
 * </p>
 *
 * <p>If a stop was requested via the <code>Stopper</code>.
 * <code>Runner</code> will report <code>RunStopped</code>
 * when the <code>execute</code> method of the run's starting <code>Suite</code> returns.
 * If a stop is not requested, <code>Runner</code> will report <code>RunCompleted</code>
 * when the last <code>execute</code> method of the run's starting <code>Suite</code>s returns.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunCompleted</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunCompleted</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunCompleted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunCompleted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunCompleted(ordinal))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunCompleted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class DiscoveryCompleted (
  ordinal: Ordinal,
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")

  /**
   * Location in a <code>DiscoveryCompleted</code> is always set to <code>None</code>.
   */
  val location: Option[Location] = None

  /**
   * Formatter in a <code>DiscoveryCompleted</code> is always set to <code>None</code>.
   */
  val formatter: Option[Formatter] = None
}

*/
