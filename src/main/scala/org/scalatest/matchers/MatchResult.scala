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
package org.scalatest.matchers
 
import org.scalatest._

// TODO: Deprecate this one

/**
 * Companion object for the <code>MatchResult</code> case class.
 *
 * @author Bill Venners
 */
object MatchResult {

  /**
   * Factory method that constructs a new <code>MatchResult</code> with passed <code>matches</code>, <code>failureMessage</code>, 
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>, and
   * <code>midSentenceNegatedFailureMessage</code> fields.
   *
   * @param matches indicates whether or not the matcher matched
   * @param failureMessage a failure message to report if a match fails
   * @param negatedFailureMessage a message with a meaning opposite to that of the failure message
   * @param midSentenceFailureMessage a failure message to report if a match fails
   * @param midSentenceNegatedFailureMessage a message with a meaning opposite to that of the failure message
   */
  def apply(matches: Boolean, failureMessage: String, negatedFailureMessage: String, midSentenceFailureMessage: String,
      midSentenceNegatedFailureMessage: String): MatchResult =
    new MatchResult(matches, failureMessage, negatedFailureMessage, midSentenceFailureMessage, midSentenceNegatedFailureMessage)

  /**
   * Factory method that constructs a new <code>MatchResult</code> with passed <code>matches</code>, <code>failureMessage</code>, and
   * <code>negativeFailureMessage</code> fields. The <code>midSentenceFailureMessage</code> will return the same
   * string as <code>failureMessage</code>, and the <code>midSentenceNegatedFailureMessage</code> will return the
   * same string as <code>negatedFailureMessage</code>.
   *
   * @param matches indicates whether or not the matcher matched
   * @param failureMessage a failure message to report if a match fails
   * @param negatedFailureMessage a message with a meaning opposite to that of the failure message
   */
  def apply(matches: Boolean, failureMessage: String, negatedFailureMessage: String): MatchResult =
    new MatchResult(matches, failureMessage, negatedFailureMessage, failureMessage, negatedFailureMessage)

  def unapply(mr: MatchResult): Option[(Boolean, String, String, String, String)] = Some((mr.matches, mr.failureMessage, mr.negatedFailureMessage, mr.midSentenceFailureMessage, mr.midSentenceNegatedFailureMessage))
}

