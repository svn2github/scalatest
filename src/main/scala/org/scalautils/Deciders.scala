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
package org.scalautils

/**
 * Provides <code>decidedBy</code> and <code>whenBothAre</code> syntax, which facilitates the
 * explicit specification of <code>Equality[T]</code> and/or <code>Normalization[T]</code> where
 * <code>Equality[T]</code> is taken implicitly.
 *
 * @author Bill Venners
 */
trait Deciders {

  /**
   * Wrapper class with <code>decidedBy</code> and <code>whenBothAre</code> methods that facilitate
   * explicit specification of equality and normalization where an <code>Equality[T]</code> type class is required.
   * as type <code>Any</code>.
   *
   * @param b the object to wrap
   *
   * @author Bill Venners
   */
  class DecidersWrapper[B](b: B) {

    /**
     * Returns an <code>EqualityCandidate</code> whose <code>isEqual</code> method will compare a passed
     * object to the <code>B</code> passed to the <code>DecidersWrapper</code> constructor using the
     * passed <code>Equality[A]</code>.
     *
     * @return an <code>EqualityCandidate[A, B]</code> that compares a given <code>A</code> to a
     *     known <code>B</code> using a known <code>Equality[A]</code>.
     */
    def decidedBy[A](equalityOfA: Equality[A]): EqualityCandidate[A, B] = new EqualityCandidate(equalityOfA, b)
    def whenBothAre(normalization: Normalization[B]): WhenBothAreResult[B] = new WhenBothAreResult(normalization, b)
  }

  /**
   * Implicit conversion that adds an <code>asAny</code> method to an object, which returns
   * the exact same object but as type <code>Any</code>.
   */
  implicit def convertToDecidersWrapper[B](b: B): DecidersWrapper[B] = new DecidersWrapper(b)
}

/**
 * Companion object to trait <code>AsAny</code> that facilitates the importing of <code>AsAny</code> members as 
 * an alternative to mixing it in. One use case is to import <code>AsAny</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; Set(1, "2") should contain (1)
 * <console>:14: error: overloaded method value should with alternatives:
 * [R](inv: org.scalautils.TripleEqualsInvocation[R])(implicit constraint: org.scalautils.EqualityConstraint[scala.collection.immutable.Set[Any],R])Unit <and>
 * (notWord: org.scalatest.Matchers.NotWord)org.scalatest.Matchers.ResultOfNotWordForTraversable[Any,scala.collection.immutable.Set] <and>
 * (beWord: org.scalatest.Matchers.BeWord)org.scalatest.Matchers.ResultOfBeWordForAnyRef[scala.collection.GenTraversable[Any]] <and>
 * (containMatcher: org.scalatest.ContainMatcher[Any])Unit <and>
 * (containWord: org.scalatest.Matchers.ContainWord)org.scalatest.Matchers.ResultOfContainWordForTraversable[Any] <and>
 * (haveWord: org.scalatest.Matchers.HaveWord)org.scalatest.Matchers.ResultOfHaveWordForTraversable[Any] <and>
 * (rightMatcherGen1: org.scalatest.Matchers.MatcherGen1[scala.collection.immutable.Set[Any],org.scalautils.Equality])(implicit equality: org.scalautils.Equality[scala.collection.immutable.Set[Any]])Unit <and>
 * (rightMatcherX6: org.scalatest.matchers.Matcher[scala.collection.GenTraversable[Any]])Unit
 *cannot be applied to (org.scalatest.matchers.Matcher[scala.collection.GenTraversable[Int]])
 *             Set(1, "2") should contain (1)
 *                         ^
 *
 * scala&gt; Set(1, "2") should contain (1.asAny)
 *
 * scala&gt;
 * </pre>
 */
object Deciders extends Deciders

