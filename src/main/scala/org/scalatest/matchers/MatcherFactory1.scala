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
package org.scalatest.matchers

import org.scalatest.enablers._
import org.scalatest.Matchers.andMatchersAndApply
import org.scalatest.Matchers.orMatchersAndApply
import org.scalatest.words.MatcherWords
import scala.collection.GenTraversable
import scala.util.matching.Regex
import org.scalautils.Equality
import org.scalautils.Interval
import org.scalautils.TripleEqualsInvocation
import org.scalatest.FailureMessages
import org.scalatest.words.FullyMatchWord
import org.scalatest.words.StartWithWord
import org.scalatest.words.EndWithWord
import org.scalatest.words.IncludeWord
import org.scalatest.words.HaveWord
import org.scalatest.words.BeWord
import org.scalatest.words.NotWord
import org.scalatest.words.ContainWord
import org.scalatest.words.ResultOfLengthWordApplication
import org.scalatest.words.ResultOfSizeWordApplication
import org.scalatest.words.ResultOfLessThanComparison
import org.scalatest.words.ResultOfGreaterThanComparison
import org.scalatest.words.ResultOfLessThanOrEqualToComparison
import org.scalatest.words.ResultOfGreaterThanOrEqualToComparison
import org.scalatest.words.ResultOfAWordToSymbolApplication
import org.scalatest.words.ResultOfAWordToBePropertyMatcherApplication
import org.scalatest.words.ResultOfAWordToAMatcherApplication
import org.scalatest.words.ResultOfAnWordToSymbolApplication
import org.scalatest.words.ResultOfAnWordToBePropertyMatcherApplication
import org.scalatest.words.ResultOfAnWordToAnMatcherApplication
import org.scalatest.words.ResultOfTheSameInstanceAsApplication
import org.scalatest.words.ResultOfRegexWordApplication
import org.scalatest.words.ResultOfKeyWordApplication
import org.scalatest.words.ResultOfValueWordApplication

/**
 * A matcher factory that can produce a matcher given one typeclass instance.
 *
 * <p>
 * In the type parameters for this class, "<code>SC</code>" means <em>superclass</em>; "<code>TC</code>"
 * (in <code>TC1</code>, <code>TC2</code>, <em>etc.</em>) means <em>typeclass</em>.
 * This class's <code>matcher</code> factory method will produce a <code>Matcher[T]</code>, where <code>T</code> is a subtype of (or the same type
 * as) <code>SC</code>, given a typeclass instance for each <code>TC<em>n</em></code>
 * implicit parameter (for example, a <code>TC1[T]</code>, <code>TC2[T]</code>, <em>etc.</em>).
 * </p>
 *
 * @author Bill Venners
 */
// Add a TYPECLASSN for each N
abstract class MatcherFactory1[-SC, TC1[_]] { thisMatcherFactory =>

  // Add a TYPECLASSN for each N
  /**
   * Factory method that will produce a <code>Matcher[T]</code>, where <code>T</code> is a subtype of (or the same type
   * as) <code>SC</code>, given a typeclass instance for each <code>TC<em>n</em></code>
   * implicit parameter (for example, a <code>TC1[T]</code>, <code>TC2[T]</code>, <em>etc.</em>).
   */
  def matcher[T <: SC : TC1]: Matcher[T]

  /**
   * Enables the <a href="../../org/scalautils/"><code>Explicitly</code></a> DSL to be used directly
   * on a <code>MatcherFactory1</code>, without invoking the <code>matcher</code> factory method.
   *
   * <p>
   * Here's an example of the kind of syntax this <code>apply</code> method can enable:
   * </p>
   *
   * <pre>
   * result should equal (1) (decided by defaultEquality)
   * </pre>
   */
  def apply[T <: SC](explicit: TC1[T]): Matcher[T] = matcher[T](explicit)

  // And and or taking a Matcher
  // Changes the 1's to N's here, and will need to add TYPECLASSN for each N in 3 places
  // (equal (7) and ...)
  /**
   * Ands this matcher factory with the passed matcher.
   */
  def and[U <: SC](rightMatcher: Matcher[U]): MatcherFactory1[U, TC1] =
    new MatcherFactory1[U, TC1] {
      def matcher[V <: U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // (equal (7) or ...)
  // Changes the 1's to N's here, and will need to add TYPECLASSN for each N in 3 places
  /**
   * Ors this matcher factory with the passed matcher.
   */
  def or[U <: SC](rightMatcher: Matcher[U]): MatcherFactory1[U, TC1] =
    new MatcherFactory1[U, TC1] {
      def matcher[V <: U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // And and or taking a MF1 that has the rightmost type class
// Need one for the same typeclass and one for a different typeclass, yes, and can overload because
// one returns a MatcherFactory1 the other a MatcherFactory2.
   // "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})
  // Changes the 1's to N's here, and will need to add TYPECLASSN for each N in 3 places
  // And what I'd do is use the rightmost TC. I may call these TC's. If it is the same, then I return the same one.
  // "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
  // Yes, same for and. Essentially, each N must have and one each and and or methods that takes a Matcher, one and and or
  // method that takes each other MatcherFactoryN, plus one extra one for MatcherFactory1 of the rightmost type.

  /**
   * Ands this matcher factory with the passed <code>MatcherFactory1</code> that has the same final typeclass as this one.
   */
  def and[U <: SC](rightMatcherFactory: MatcherFactory1[U, TC1]): MatcherFactory1[U, TC1] =
    new MatcherFactory1[U, TC1] {
      def matcher[V <: U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  /**
   * Ors this matcher factory with the passed <code>MatcherFactory1</code> that has the same final typeclass as this one.
   */
  def or[U <: SC](rightMatcherFactory: MatcherFactory1[U, TC1]): MatcherFactory1[U, TC1] =
    new MatcherFactory1[U, TC1] {
      def matcher[V <: U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // And and or taking a MF1 with a different type class
  // This one, though, I'd need to add 1 more. Amazing this overloads, but anyway. And I really need this for each N. The above
  // special case is just for MatcherFactory1. The other N's I'm not going to bother trying to do a quickie overload.
  /**
   * Ands this matcher factory with the passed matcher factory.
   */
  def and[U <: SC, TC2[_]](rightMatcherFactory: MatcherFactory1[U, TC2]): MatcherFactory2[U, TC1, TC2] =
    new MatcherFactory2[U, TC1, TC2] {
      def matcher[V <: U : TC1 : TC2]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  /**
   * Ors this matcher factory with the passed matcher factory.
   */
  def or[U <: SC, TC2[_]](rightMatcherFactory: MatcherFactory1[U, TC2]): MatcherFactory2[U, TC1, TC2] =
    new MatcherFactory2[U, TC1, TC2] {
      def matcher[V <: U : TC1 : TC2]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // If A is Arity of this one, then For each N > Arity < 22 - Arity, and and or taking a MFN with a different type class
  // Replicating the and/or DSL here:

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndHaveWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and have length (3 - 1))
     *                           ^
     * </pre>
     */
    def length(expectedLength: Long): MatcherFactory2[SC, TC1, Length] = and(MatcherWords.have.length(expectedLength))

    // These guys need to generate a MatcherFactory of N+1. And it needs N-1 TC's, with the last one being Length.

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and have size (3 - 1))
     *                           ^ 
     * </pre>
     */
    def size(expectedSize: Long): MatcherFactory2[SC, TC1, Size] = and(MatcherWords.have.size(expectedSize))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (haMatcherFactory and have size (3 - 1))
   *                   ^ 
   * </pre>
   */
  def and(haveWord: HaveWord): AndHaveWord = new AndHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndContainWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain (3 - 1))
     *                              ^
     * </pre>
     */
    def apply[U](expectedElement: U): MatcherFactory1[SC with GenTraversable[U], TC1] = thisMatcherFactory.and(MatcherWords.contain(expectedElement))

    // And some, the ones that would by themselves already generate a Matcher, just return a MatcherFactoryN where N is the same.

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain key ("one"))
     *                               ^
     * </pre>
     */
    def key[U](expectedElement: U): MatcherFactory1[SC with scala.collection.GenMap[U, Any], TC1] = thisMatcherFactory.and(MatcherWords.contain.key(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain value (1))
     *                              ^
     * </pre>
     */
    def value[U](expectedValue: U): MatcherFactory1[SC with scala.collection.GenMap[K, U] forSome { type K }, TC1] = thisMatcherFactory.and(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain theSameElementsAs List(1, 2, 3))
     *                              ^
     * </pre>
     */
    def theSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.theSameElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain theSameIteratedElementsAs List(1, 2, 3))
     *                              ^
     * </pre>
     */
    def theSameIteratedElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.theSameIteratedElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain allOf (1, 2, 3))
     *                              ^
     * </pre>
     */
    def allOf[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.allOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain inOrder (1, 2, 3))
     *                              ^
     * </pre>
     */
    def inOrder[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.inOrder(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain oneOf (1, 3, 3))
     *                              ^
     * </pre>
     */
    def oneOf[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.oneOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain only (3, 1))
     *                              ^
     * </pre>
     */
    def only[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.only(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (and contain inOrderOnly (1, 3))
     *              ^
     * </pre>
     */
    def inOrderOnly[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.inOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (and contain noneOf (7, 8, 9))
     *              ^
     * </pre>
     */
    def noneOf[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.and(MatcherWords.contain.noneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain a (validNumber))
     *                              ^
     * </pre>
     */
    def a[E](aMatcher: AMatcher[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      and(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and contain an (invalidNumber))
     *                              ^
     * </pre>
     */
    def an[E](anMatcher: AnMatcher[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      and(MatcherWords.contain.an(anMatcher))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and contain key ("one"))
   *                  ^ 
   * </pre>
   */
  def and(containWord: ContainWord): AndContainWord = new AndContainWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndBeWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be a ('file))
     *                         ^
     * </pre>
     */
    def a(symbol: Symbol): MatcherFactory1[SC with AnyRef, TC1] = and(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be a (file))
     *                         ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory1[SC with AnyRef with U, TC1] = and(MatcherWords.be.a(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be a (validNumber))
     *                         ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): MatcherFactory1[SC with U, TC1] = and(MatcherWords.be.a(aMatcher))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be an ('apple))
     *                         ^
     * </pre>
     */
    def an(symbol: Symbol): MatcherFactory1[SC with AnyRef, TC1] = and(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be an (apple))
     *                         ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory1[SC with AnyRef with U, TC1] = and(MatcherWords.be.an(bePropertyMatcher))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be an (integerNumber))
     *                         ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): MatcherFactory1[SC with U, TC1] = and(MatcherWords.be.an(anMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and be theSameInstanceAs (string))
     *                         ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): MatcherFactory1[SC with AnyRef, TC1] = and(MatcherWords.be.theSameInstanceAs(anyRef))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and be a ('file))
   *                  ^
   * </pre>
   */
  def and(beWord: BeWord): AndBeWord = new AndBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndFullyMatchWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and fullyMatch regex (decimal))
     *                                 ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = and(MatcherWords.fullyMatch.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and fullyMatch regex (decimalRegex))
     *                                 ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = and(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and fullyMatch regex (decimalRegex))
   *                  ^
   * </pre>
   */
  def and(fullyMatchWord: FullyMatchWord): AndFullyMatchWord = new AndFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndIncludeWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and include regex (decimal))
     *                              ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = and(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and include regex (decimalRegex))
     *                              ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = and(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and include regex ("wor.d"))
   *                  ^
   * </pre>
   */
  def and(includeWord: IncludeWord): AndIncludeWord = new AndIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndStartWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and startWith regex (decimal))
     *                                ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = and(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and startWith regex (decimalRegex))
     *                                ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = and(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and startWith regex ("1.7"))
   *                  ^
   * </pre>
   */
  def and(startWithWord: StartWithWord): AndStartWithWord = new AndStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndEndWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and endWith regex (decimal))
     *                              ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = and(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and endWith regex (decimalRegex))
     *                              ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = and(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and endWith regex (decimalRegex))
   *                  ^
   * </pre>
   */
  def and(endWithWord: EndWithWord): AndEndWithWord = new AndEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndNotWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not equal (3 - 1))
     *                          ^
     * </pre>
     */
    def equal(any: Any): MatcherFactory2[SC, TC1, Equality] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not equal (17.0 +- 0.2))
     *                          ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.and(MatcherWords.not.equal(interval))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not equal (null))
     *                          ^
     * </pre>
     */
    def equal(o: Null): MatcherFactory1[SC, TC1] = {
      thisMatcherFactory and {
        new Matcher[SC] {
          def apply(left: SC): MatchResult = {
            MatchResult(
              left != null,
              FailureMessages("equaledNull"),
              FailureMessages("didNotEqualNull", left),
              FailureMessages("midSentenceEqualedNull"),
              FailureMessages("didNotEqualNull", left)
            )
          }
        }
      }
    }

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be (3 - 1))
     *                          ^
     * </pre>
     */
    def be(any: Any): MatcherFactory1[SC, TC1] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not have length (3))
     *                          ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory2[SC, TC1, Length] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not have size (3))
     *                          ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory2[SC, TC1, Size] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not have (author ("Melville")))
     *                          ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be &lt; (6))
     *                          ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be (null))
     *                          ^
     * </pre>
     */
    def be(o: Null): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.and(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory (8) and not be &gt; (6))
     *                              ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be &lt;= (2))
     *                          ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be &gt;= (6))
     *                          ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be === (6))
     *                          ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): MatcherFactory1[SC, TC1] =
      thisMatcherFactory.and(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be ('empty))
     *                          ^
     * </pre>
     */
    def be(symbol: Symbol): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.and(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be (odd))
     *                          ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.and(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be (directory))
     *                          ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory1[SC with AnyRef with U, TC1] = thisMatcherFactory.and(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be a ('file))
     *                          ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be a (validMarks))
     *                          ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be a (directory))
     *                          ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be a primeNumber)
     *                          ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be an (directory))
     *                          ^
     * </pre>
     */
    def be[SC <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[SC]) = thisMatcherFactory.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be an (invalidMarks))
     *                          ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be theSameInstanceAs (otherString))
     *                          ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.and(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be (17.0 +- 0.2))
     *                          ^
     * </pre>
     */
    def be[U](interval: Interval[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.and(MatcherWords.not.be(interval))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not fullyMatch regex (decimal))
     *                          ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not include regex (decimal))
     *                          ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not include ("1.7"))
     *                          ^
     * </pre>
     */
    def include(expectedSubstring: String): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not startWith regex (decimal))
     *                          ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not startWith ("1.7"))
     *                          ^
     * </pre>
     */
    def startWith(expectedSubstring: String): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not endWith regex (decimal))
     *                          ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not endWith ("1.7"))
     *                          ^
     * </pre>
     */
    def endWith(expectedSubstring: String): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.and(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not contain (3))
     *                          ^
     * </pre>
     */
    def contain[U](expectedElement: U): MatcherFactory1[SC with GenTraversable[U], TC1] =
      thisMatcherFactory.and(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not contain key ("three"))
     *                          ^
     * </pre>
     */
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): MatcherFactory1[SC with scala.collection.GenMap[U, Any], TC1] =
      thisMatcherFactory.and(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not contain value (3))
     *                          ^
     * </pre>
     */
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): MatcherFactory1[SC with scala.collection.GenMap[K, U] forSome { type K }, TC1] =
      thisMatcherFactory.and(MatcherWords.not.contain(resultOfValueWordApplication))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not contain theSameElementsAs (List(8, 1, 2))) 
     *                          ^
     * </pre>
     */
    def contain[U](right: ContainMatcher[U]): MatcherFactory1[SC with GenTraversable[U], TC1] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not contain a primeNumber)
     *                          ^
     * </pre>
     */
    def contain[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): MatcherFactory1[SC with GenTraversable[U], TC1] = 
      thisMatcherFactory.and(MatcherWords.not.contain(resultOfAWordApplication))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not contain an invalidNumber)
     *                          ^
     * </pre>
     */
    def contain[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): MatcherFactory1[SC with GenTraversable[U], TC1] = 
      thisMatcherFactory.and(MatcherWords.not.contain(resultOfAnWordApplication))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory and not contain value (3))
   *                  ^
   * </pre>
   */
  def and(notWord: NotWord): AndNotWord = new AndNotWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrHaveWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or have length (3 - 1))
     *                          ^
     * </pre>
     */
    def length(expectedLength: Long): MatcherFactory2[SC, TC1, Length] = or(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or have size (3 - 1))
     *                          ^
     * </pre>
     */
    def size(expectedSize: Long): MatcherFactory2[SC, TC1, Size] = or(MatcherWords.have.size(expectedSize))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or have size (3 - 1))
   *                  ^
   * </pre>
   */
  def or(haveWord: HaveWord): OrHaveWord = new OrHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrContainWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain (3 - 1))
     *                             ^
     * </pre>
     */
    def apply[U](expectedElement: U): MatcherFactory1[SC with GenTraversable[U], TC1] = thisMatcherFactory.or(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain key ("one"))
     *                             ^
     * </pre>
     */
    def key[U](expectedKey: U): MatcherFactory1[SC with scala.collection.GenMap[U, Any], TC1] = thisMatcherFactory.or(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain value (1))
     *                             ^
     * </pre>
     */
    def value[U](expectedValue: U): MatcherFactory1[SC with scala.collection.GenMap[K, U] forSome { type K }, TC1] = thisMatcherFactory.or(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain theSameElementsAs List(1, 2, 3))
     *                             ^
     * </pre>
     */
    def theSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.theSameElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain theSameIteratedElementsAs List(1, 2, 3))
     *                             ^
     * </pre>
     */
    def theSameIteratedElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.theSameIteratedElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain allOf (1, 2, 3))
     *                             ^
     * </pre>
     */
    def allOf[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.allOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain inOrder (1, 2, 3))
     *                             ^
     * </pre>
     */
    def inOrder[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.inOrder(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain oneOf (1, 3, 3))
     *                             ^
     * </pre>
     */
    def oneOf[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.oneOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain only (3, 1))
     *                             ^
     * </pre>
     */
    def only[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.only(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain inOrderOnly (1, 3))
     *                             ^
     * </pre>
     */
    def inOrderOnly[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.inOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain noneOf (7, 8, 9))
     *                             ^
     * </pre>
     */
    def noneOf[E](right: E*)(implicit equality: Equality[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      thisMatcherFactory.or(MatcherWords.contain.noneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain a (validNumber))
     *                             ^
     * </pre>
     */
    def a[E](aMatcher: AMatcher[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      or(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or contain an (invalidNumber))
     *                             ^
     * </pre>
     */
    def an[E](anMatcher: AnMatcher[E]): MatcherFactory1[SC with GenTraversable[E], TC1] = 
      or(MatcherWords.contain.an(anMatcher))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or contain value (1))
   *                  ^
   * </pre>
   */
  def or(containWord: ContainWord): OrContainWord = new OrContainWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrBeWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be a ('directory))
     *                        ^
     * </pre>
     */
    def a(symbol: Symbol): MatcherFactory1[SC with AnyRef, TC1] = or(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be a (directory))
     *                        ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory1[SC with AnyRef with U, TC1] = or(MatcherWords.be.a(bePropertyMatcher))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be a (validNumber))
     *                        ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): MatcherFactory1[SC with U, TC1] = or(MatcherWords.be.a(aMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be an ('apple))
     *                        ^
     * </pre>
     */
    def an(symbol: Symbol): MatcherFactory1[SC with AnyRef, TC1] = or(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be an (apple))
     *                        ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory1[SC with AnyRef with U, TC1] = or(MatcherWords.be.an(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be an (integerNumber))
     *                        ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): MatcherFactory1[SC with U, TC1] = or(MatcherWords.be.an(anMatcher))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or be theSameInstanceAs (otherString))
     *                        ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): MatcherFactory1[SC with AnyRef, TC1] = or(MatcherWords.be.theSameInstanceAs(anyRef))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or be a ('directory))
   *                  ^
   * </pre>
   */
  def or(beWord: BeWord): OrBeWord = new OrBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrFullyMatchWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or fullyMatch regex (decimal))
     *                                ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = or(MatcherWords.fullyMatch.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or fullyMatch regex (decimal))
     *                                ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = or(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or fullyMatch regex (decimal))
   *                  ^
   * </pre>
   */
  def or(fullyMatchWord: FullyMatchWord): OrFullyMatchWord = new OrFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrIncludeWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or include regex (decimal))
     *                             ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = or(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or include regex (decimal))
     *                             ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = or(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or include regex ("1.7"))
   *                  ^
   * </pre>
   */
  def or(includeWord: IncludeWord): OrIncludeWord = new OrIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrStartWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or startWith regex (decimal))
     *                               ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = or(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or startWith regex (decimal))
     *                               ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = or(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or startWith regex ("1.7"))
   *                  ^
   * </pre>
   */
  def or(startWithWord: StartWithWord): OrStartWithWord = new OrStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrEndWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or endWith regex (decimal))
     *                             ^
     * </pre>
     */
    def regex(regexString: String): MatcherFactory1[SC with String, TC1] = or(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or endWith regex (decimal))
     *                             ^
     * </pre>
     */
    def regex(regex: Regex): MatcherFactory1[SC with String, TC1] = or(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or endWith regex ("7b"))
   *                  ^
   * </pre>
   */
  def or(endWithWord: EndWithWord): OrEndWithWord = new OrEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrNotWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not equal (2))
     *                         ^
     * </pre>
     */
    def equal(any: Any): MatcherFactory1[SC, TC1] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.legacyEqual(any)))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not equal (17.0 +- 0.2))
     *                         ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.equal(interval))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not equal (null))
     *                         ^
     * </pre>
     */
    def equal(o: Null): MatcherFactory1[SC, TC1] = {
      thisMatcherFactory or {
        new Matcher[SC] {
          def apply(left: SC): MatchResult = {
            MatchResult(
              left != null,
              FailureMessages("equaledNull"),
              FailureMessages("didNotEqualNull", left),
              FailureMessages("midSentenceEqualedNull"),
              FailureMessages("didNotEqualNull", left)
            )
          }
        }
      }
    }

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be (2))
     *                         ^
     * </pre>
     */
    def be(any: Any): MatcherFactory1[SC, TC1] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not have length (3))
     *                         ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory2[SC, TC1, Length] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not have size (3))
     *                         ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory2[SC, TC1, Size] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not have (author ("Melville")))
     *                         ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be (null))
     *                         ^
     * </pre>
     */
    def be(o: Null): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.or(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be &lt; (8))
     *                         ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be &gt; (6))
     *                         ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be &lt;= (2))
     *                         ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be &gt;= (6))
     *                         ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): MatcherFactory1[SC with U, TC1] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be === (8))
     *                         ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): MatcherFactory1[SC, TC1] =
      thisMatcherFactory.or(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be ('empty))
     *                         ^
     * </pre>
     */
    def be(symbol: Symbol): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.or(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be (odd))
     *                         ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be (file))
     *                         ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory1[SC with AnyRef with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be a ('file))
     *                         ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be a (validMarks))
     *                         ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be a (file))
     *                         ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be an ('apple))
     *                         ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be an (file))
     *                         ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory and not be an (invalidMarks))
     *                          ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be theSameInstanceAs (string))
     *                         ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): MatcherFactory1[SC with AnyRef, TC1] = thisMatcherFactory.or(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not be (17.0 +- 0.2))
     *                         ^
     * </pre>
     */
    def be[U](interval: Interval[U]): MatcherFactory1[SC with U, TC1] = thisMatcherFactory.or(MatcherWords.not.be(interval))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not fullyMatch regex (decimal))
     *                         ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not include regex (decimal))
     *                         ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not include ("1.7"))
     *                         ^
     * </pre>
     */
    def include(expectedSubstring: String): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not startWith regex (decimal))
     *                         ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not startWith ("1.7"))
     *                         ^
     * </pre>
     */
    def startWith(expectedSubstring: String): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not endWith regex (decimal))
     *                         ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not endWith ("1.7"))
     *                         ^
     * </pre>
     */
    def endWith(expectedSubstring: String): MatcherFactory1[SC with String, TC1] =
      thisMatcherFactory.or(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not contain (3))
     *                         ^
     * </pre>
     */
    def contain[U](expectedElement: U): MatcherFactory1[SC with GenTraversable[U], TC1] =
      thisMatcherFactory.or(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not contain key ("three"))
     *                         ^
     * </pre>
     */
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): MatcherFactory1[SC with scala.collection.GenMap[U, Any], TC1] =
      thisMatcherFactory.or(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not contain value (3))
     *                         ^
     * </pre>
     */
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): MatcherFactory1[SC with scala.collection.GenMap[K, U] forSome { type K }, TC1] =
      thisMatcherFactory.or(MatcherWords.not.contain(resultOfValueWordApplication))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not contain theSameElementsAs (List(8, 1, 2))) 
     *                         ^
     * </pre>
     */
    def contain[U](right: ContainMatcher[U]): MatcherFactory1[SC with GenTraversable[U], TC1] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not contain a primeNumber)
     *                         ^
     * </pre>
     */
    def contain[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): MatcherFactory1[SC with GenTraversable[U], TC1] = 
      thisMatcherFactory.or(MatcherWords.not.contain(resultOfAWordApplication))
      
    /**
     * This method enables the following syntax given a <code>MatcherFactory1</code>:
     *
     * <pre class="stHighlight">
     * (aMatcherFactory or not contain an invalidNumber)
     *                         ^
     * </pre>
     */
    def contain[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): MatcherFactory1[SC with GenTraversable[U], TC1] = 
      thisMatcherFactory.or(MatcherWords.not.contain(resultOfAnWordApplication))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory1</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or not contain value (3))
   *                  ^
   * </pre>
   */
  def or(notWord: NotWord): OrNotWord = new OrNotWord
}

