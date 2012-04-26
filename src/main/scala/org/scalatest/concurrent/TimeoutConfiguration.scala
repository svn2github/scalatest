/*
 * Copyright 2001-2011 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest._
import java.awt.AWTError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import java.lang.annotation.AnnotationFormatError
import org.scalatest.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.Suite.anErrorThatShouldCauseAnAbort
import scala.annotation.tailrec
import time.{Millis, Second, Span}

/**
 * Trait providing methods and classes used to configure timeouts and, where relevant, the interval
 * between retries.
 *
 * <p>
 * Timeouts are used by the <code>eventually</code> methods of trait
 * <a href="Eventually.html"><code>Eventually</code></a> and the <code>await</code> method of class
 * <a href="AsyncAssertions/Waiter.html"><code>Waiter</code></a>, a member of trait
 * <a href="AsyncAssertions.html"><code>AsyncAssertions</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
trait TimeoutConfiguration {

  /**
   * Configuration object for traits <code>Eventually</code> and <code>Futures</code>.
   *
   * <p>
   * The default values for the parameters are:
   * </p>
   *
   * <table style="border-collapse: collapse; border: 1px solid black">
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * timeout
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 1 second
   * </td>
   * </tr>
   * <tr>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * interval
   * </td>
   * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
   * 10 milliseconds
   * </td>
   * </tr>
   * </table>
   *
   * @param timeout the maximum amount of time to retry before giving up and throwing
   *   <code>TestFailedException</code>.
   * @param interval the amount of time to sleep between each attempt
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  final case class TimeoutConfig(timeout: Span = Span(1, Second), interval: Span = Span(10, Millis))

  /**
   * Abstract class defining a family of configuration parameters for traits <code>Eventually</code> and <code>Futures</code>.
   * 
   * <p>
   * The subclasses of this abstract class are used to pass configuration information to
   * the <code>eventually</code> methods of trait <code>Eventually</code> and the <code>whenReady</code> method of trait <code>Futures</code>.
   * </p>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  sealed abstract class TimeoutConfigParam

  /**
   * A <code>TimeoutConfigParam</code> that specifies the maximum amount of time to allow retries: either invocations of the
   * by-name parameter passed to <code>eventually</code> that give an unsuccessful result, or futures passed to <code>whenReady</code> that
   * are canceled, or expired, or not ready.
   *
   * @param value the maximum amount of time to retry before giving up and throwing
   *   <code>TestFailedException</code>.
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   *
   * @author Bill Venners
   */
  final case class Timeout(value: Span) extends TimeoutConfigParam

  /**
   * A <code>TimeoutConfigParam</code> that specifies the amount of time to sleep after
   * each retry: each unsuccessful invocation of the by-name parameter passed to <code>eventually</code> or
   * each query of a future passed to <code>whenReady</code>.
   *
   * @param value the amount of time to sleep between each attempt
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   *
   * @author Bill Venners
   */
  final case class Interval(value: Span) extends TimeoutConfigParam

  /**
   * Implicit <code>TimeoutConfig</code> value providing default configuration values.
   *
   * <p>
   * To change the default configuration, override or hide this <code>val</code> with another implicit
   * <code>TimeoutConfig</code> containing your desired default configuration values.
   * </p>
   */
  implicit val timeoutConfig = TimeoutConfig()

  /**
   * Returns a <code>Timeout</code> configuration parameter containing the passed value, which
   * specifies the maximum amount of time to retry: to allow invocations of the
   * by-name parameter passed to <code>eventually</code> to give an unsuccessful result, or to
   * allow a future passed to <code>whenReady</code> to be canceled, or expired, or not ready.
   */
  def timeout(value: Span) = Timeout(value)

  /**
   * Returns an <code>Interval</code> configuration parameter containing the passed value, which
   * specifies the amount of time to sleep after
   * each retry: each unsuccessful invocation of the by-name parameter passed to <code>eventually</code>
   * or each query of a non-ready, canceled, or expired future passed to <code>whenReady</code>.
   */
  def interval(value: Span) = Interval(value)    // TODO: Throw NPE
}
