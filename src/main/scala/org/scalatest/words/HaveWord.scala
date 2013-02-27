/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest.words

import org.scalatest.matchers._
import org.scalautils._
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.Resources
import scala.collection.GenTraversable
import scala.collection.GenSeq
import org.scalatest.Matchers.newTestFailedException
import org.scalatest.Helper.accessProperty

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class HaveWord {

  // TODO: How about returning a Matcher[Gazornimplatz] and then providing implicit conversion
  // methods from Matcher[Gazornimplatz] to Matcher[Seq], Matcher[String], Matcher[java.util.List], and
  // Matcher[the structural length methods]. This is similar to the technique I used with "contain (7)"
  // to get it to work with java.util.Collection.
  // I couldn't figure out how to combine view bounds with existential types. May or may not
  // be possible, but going dynamic for now at least.
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * book should have length (9)
   *                  ^
   * </pre>
   *
   * <p>
   * Currently (as of ScalaTest 0.9.5), this method will produce a <code>Matcher[AnyRef]</code>, and if the
   * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>length</code> property
   * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem. The one exception is that it will work on
   * <code>java.util.List</code>, even though that type has no <code>length</code> structure (its <code>size</code> property
   * will be used instead.) In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
   * </p>
   */
  def length(expectedLength: Long): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult =
        left match {
          case leftArray: Array[_] =>
            MatchResult(
              leftArray.length == expectedLength, 
              FailureMessages("didNotHaveExpectedLength", left, expectedLength),
              FailureMessages("hadExpectedLength", left, expectedLength)
            )
          case leftSeq: GenSeq[_] =>
            MatchResult(
              leftSeq.length == expectedLength, 
              FailureMessages("didNotHaveExpectedLength", left, expectedLength),
              FailureMessages("hadExpectedLength", left, expectedLength)
            )
          case leftString: String =>
            MatchResult(
              leftString.length == expectedLength, 
              FailureMessages("didNotHaveExpectedLength", left, expectedLength),
              FailureMessages("hadExpectedLength", left, expectedLength)
            )
          case leftJavaList: java.util.List[_] =>
            MatchResult(
              leftJavaList.size == expectedLength,
              FailureMessages("didNotHaveExpectedLength", left, expectedLength),
              FailureMessages("hadExpectedLength", left, expectedLength)
            )
          case _ =>

            accessProperty(left, 'length, false) match {

              case None =>

                throw newTestFailedException(Resources("noLengthStructure", expectedLength.toString))

              case Some(result) =>

                MatchResult(
                  result == expectedLength,
                  FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                  FailureMessages("hadExpectedLength", left, expectedLength)
                )
            }
        }
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * book should have size (9)
   *                  ^
   * </pre>
   *
   * <p>
   * Currently, this method will produce a <code>Matcher[AnyRef]</code>, and if the
   * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>size</code> property
   * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem.
   * In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
   * </p>
   */
  def size(expectedSize: Long): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult =
        left match {
          case leftArray: Array[_] =>
            MatchResult(
              leftArray.length == expectedSize, 
              FailureMessages("didNotHaveExpectedSize", left, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          case leftTrav: GenTraversable[_] =>
            MatchResult(
              leftTrav.size == expectedSize, 
              FailureMessages("didNotHaveExpectedSize", left, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          case leftJavaList: java.util.List[_] =>
            MatchResult(
              leftJavaList.size == expectedSize,
              FailureMessages("didNotHaveExpectedSize", left, expectedSize),
              FailureMessages("hadExpectedSize", left, expectedSize)
            )
          case _ =>

            accessProperty(left, 'size, false) match {

              case None =>

                throw newTestFailedException(Resources("noSizeStructure", expectedSize.toString))

              case Some(result) =>

                MatchResult(
                  result == expectedSize,
                  FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                  FailureMessages("hadExpectedSize", left, expectedSize)
                )
            }
        }
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * book should have (title ("A Tale of Two Cities"))
   *                  ^
   * </pre>
   */
  def apply[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =

    new Matcher[T] {

      def apply(left: T): MatchResult = {

        val results =
          for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
            propertyVerifier(left)

        val firstFailureOption = results.find(pv => !pv.matches)

        val justOneProperty = propertyMatchers.length == 0

        firstFailureOption match {

          case Some(firstFailure) =>

            val failedVerification = firstFailure
            val failureMessage =
              FailureMessages(
                "propertyDidNotHaveExpectedValue",
                UnquotedString(failedVerification.propertyName),
                failedVerification.expectedValue,
                failedVerification.actualValue,
                left
              )
            val midSentenceFailureMessage =
              FailureMessages(
                "midSentencePropertyDidNotHaveExpectedValue",
                UnquotedString(failedVerification.propertyName),
                failedVerification.expectedValue,
                failedVerification.actualValue,
                left
              )

            MatchResult(false, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)

          case None =>

            val failureMessage =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                FailureMessages(
                  "propertyHadExpectedValue",
                  UnquotedString(firstPropertyResult.propertyName),
                  firstPropertyResult.expectedValue,
                  left
                )
              }
              else FailureMessages("allPropertiesHadExpectedValues", left)

            val midSentenceFailureMessage =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                FailureMessages(
                  "midSentencePropertyHadExpectedValue",
                  UnquotedString(firstPropertyResult.propertyName),
                  firstPropertyResult.expectedValue,
                  left
                )
              }
              else FailureMessages("midSentenceAllPropertiesHadExpectedValues", left)

            MatchResult(true, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)
        }
      }
    }
}
