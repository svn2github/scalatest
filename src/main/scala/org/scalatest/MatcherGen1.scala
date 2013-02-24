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
package org.scalatest

import Matchers.andMatchersAndApply
import Matchers.orMatchersAndApply

abstract class MatcherGen1[-SUPERCLASS, TYPECLASS[_]] { thisMatcherGen1 =>

  def matcher[T <: SUPERCLASS : TYPECLASS]: Matcher[T]

  def apply[T <: SUPERCLASS](explicit: TYPECLASS[T]): Matcher[T] = matcher[T](explicit)

  // (equal (7) and ...)
  def and[U <: SUPERCLASS](rightMatcher: Matcher[U]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
/*
            val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
            if (!leftMatchResult.matches)
              MatchResult(
                false,
                leftMatchResult.failureMessage,
                leftMatchResult.negatedFailureMessage,
                leftMatchResult.midSentenceFailureMessage,
                leftMatchResult.midSentenceNegatedFailureMessage
              )
            else {
              MatchResult(
                rightMatchResult.matches,
                Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
                Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
                Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
                Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
              )
            }
*/
          }
        }
      }
    }

  // (equal (7) or ...)
  def or[U <: SUPERCLASS](rightMatcher: Matcher[U]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

// Need one for the same typeclass and one for a different typeclass, yes, and can overload because
// one returns a MatcherGen1 the other a MatcherGen2.
   // "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})
  def or[U <: SUPERCLASS](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            val rightMatcher = rightMatcherGen1.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
  def and[U <: SUPERCLASS](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            val rightMatcher = rightMatcherGen1.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }
}
