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
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
trait LogicalOperations[-T] { leftMatcher: Matcher[T] =>

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndHaveWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have length (2) and have length (3 - 1))
     *                                              ^
     * </pre>
     */
    def length(expectedLength: Long): Matcher[T with AnyRef] = and(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                            ^ 
     * </pre>
     */
    def size(expectedSize: Long): Matcher[T with AnyRef] = and(MatcherWords.have.size(expectedSize))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (have size (2) and have size (3 - 1))
   *                                   ^ 
   * </pre>
   */
  def and(haveWord: HaveWord): AndHaveWord = new AndHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (contain (2) and contain (3 - 1))
     *                                     ^
     * </pre>
     */
    def apply[U](expectedElement: U): Matcher[T with GenTraversable[U]] = leftMatcher.and(MatcherWords.contain(expectedElement))
    // def element[T](expectedElement: T) = leftMatcher.and(MatcherWords.contain.apply(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("two") and contain key ("one"))
     *                                                                     ^
     * </pre>
     */
    def key[U](expectedElement: U): Matcher[T with scala.collection.GenMap[U, Any]] = leftMatcher.and(MatcherWords.contain.key(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (2) and contain value (1))
     *                                                                   ^
     * </pre>
     */
    def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = leftMatcher.and(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain theSameElementsAs List(1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def theSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.theSameElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain theSameIteratedElementsAs List(1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def theSameIteratedElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.theSameIteratedElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain allOf (1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def allOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.allOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain inOrder (1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def inOrder[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.inOrder(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain oneOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def oneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.oneOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain only (3, 1))
     *                                                                           ^
     * </pre>
     */
    def only[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.only(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain inOrderOnly (1, 3))
     *                                                                           ^
     * </pre>
     */
    def inOrderOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.inOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain noneOf (7, 8, 9))
     *                                                                           ^
     * </pre>
     */
    def noneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.and(MatcherWords.contain.noneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) and contain a (validNumber))
     *                                                       ^
     * </pre>
     */
    def a[E](aMatcher: AMatcher[E]): Matcher[T with GenTraversable[E]] = 
      and(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain an (positiveNumber) and contain an (validNumber))
     *                                                        ^
     * </pre>
     */
    def an[E](anMatcher: AnMatcher[E]): Matcher[T with GenTraversable[E]] = 
      and(MatcherWords.contain.an(anMatcher))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("two") and contain key ("one"))
   *                                                         ^ 
   * </pre>
   */
  def and(containWord: ContainWord): AndContainWord = new AndContainWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndBeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a ('file) and be a ('file))
     *                                        ^
     * </pre>
     */
    def a(symbol: Symbol): Matcher[T with AnyRef] = and(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (be a (file) and be a (file))
     *                                        ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(MatcherWords.be.a(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be a (positiveNumber) and be a (validNumber))
     *                                             ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): Matcher[T with U] = and(MatcherWords.be.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isAppleMock should (be an ('apple) and be an ('apple))
     *                                           ^
     * </pre>
     */
    def an(symbol: Symbol): Matcher[T with AnyRef] = and(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isAppleMock should (be an (apple) and be an (apple))
     *                                           ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(MatcherWords.be.an(bePropertyMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be an (oddNumber) and be an (integerNumber))
     *                                         ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): Matcher[T with U] = and(MatcherWords.be.an(anMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
     *                                                  ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = and(MatcherWords.be.theSameInstanceAs(anyRef))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * isFileMock should (be a ('file) and be a ('file))
   *                                 ^
   * </pre>
   */
  def and(beWord: BeWord): AndBeWord = new AndBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndFullyMatchWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
     *                                                         ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
     *                                                              ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
   *                                               ^
   * </pre>
   */
  def and(fullyMatchWord: FullyMatchWord): AndFullyMatchWord = new AndFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndIncludeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include regex (decimal) and include regex (decimal))
     *                                                   ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
     *                                                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "hello, world" should (include regex ("hel*o") and include regex ("wor.d"))
   *                                           ^
   * </pre>
   */
  def and(includeWord: IncludeWord): AndIncludeWord = new AndIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndStartWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex (decimal) and startWith regex (decimal))
     *                                                       ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
     *                                                            ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.78" should (have length (4) and startWith regex ("1.7"))
   *                                ^
   * </pre>
   */
  def and(startWithWord: StartWithWord): AndStartWithWord = new AndStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndEndWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex (decimal) and endWith regex (decimal))
     *                                                   ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
     *                                                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
   *                                            ^
   * </pre>
   */
  def and(endWithWord: EndWithWord): AndEndWithWord = new AndEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndNotWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 1 should (not equal (2) and not equal (3 - 1))
     *                                 ^
     * </pre>
     */
    def equal(any: Any): MatcherGen1[T, Equality] =
      leftMatcher.and(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not equal (17.0 plusOrMinus 0.2) and not equal (17.0 plusOrMinus 0.2))
     *                                                         ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): Matcher[T with U] = leftMatcher.and(MatcherWords.not.equal(interval))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aNullRef should (not equal ("hi") and not equal (null))
     *                                   ^
     * </pre>
     */
    def equal(o: Null): Matcher[T] = {
      leftMatcher and {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
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
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 1 should (not be (2) and not be (3 - 1))
     *                              ^
     * </pre>
     */
    def be(any: Any): Matcher[T] =
      leftMatcher.and(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have length (3))
     *                                               ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[T with AnyRef] =
      leftMatcher.and(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have size (3))
     *                                               ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[T with AnyRef] =
      leftMatcher.and(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) and not have (author ("Melville")))
     *                                                     ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
      leftMatcher.and(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be &lt; (2) and not be &lt; (6))
     *                                ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
      leftMatcher.and(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should (contain key (7) and not be (null))
     *                                     ^
     * </pre>
     */
    def be(o: Null): Matcher[T with AnyRef] = leftMatcher.and(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 7 should (not be &gt; (8) and not be &gt; (6))
     *                                ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
      leftMatcher.and(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be &lt;= (1) and not be &lt;= (2))
     *                                 ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
      leftMatcher.and(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 7 should (not be &gt;= (8) and not be &gt;= (6))
     *                                 ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
      leftMatcher.and(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be === (2) and not be === (6))
     *                                  ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
      leftMatcher.and(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notEmptyMock should (not be ('empty) and not be ('empty))
     *                                              ^
     * </pre>
     */
    def be(symbol: Symbol): Matcher[T with AnyRef] = leftMatcher.and(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be (odd) and not be (odd))
     *                                ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = leftMatcher.and(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be (directory) and not be (directory))
     *                                              ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = leftMatcher.and(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('file) and not be a ('file))
     *                                                ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = leftMatcher.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be a (passedMarks) and not be a (validMarks))
     *                                               ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with U] = leftMatcher.and(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be a (directory) and not be a (directory))
     *                                             ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = leftMatcher.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be a negativeNumber and not be a primeNumber)
     *                                                ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = leftMatcher.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) and not be an (directory))
     *                                              ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]) = leftMatcher.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be an (oddMarks) and not be an (invalidMarks))
     *                                                ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with U] = leftMatcher.and(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (otherString))
     *                                                            ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = leftMatcher.and(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not be (17.0 plusOrMinus 0.2) and not be (17.0 plusOrMinus 0.2))
     *                                                          ^
     * </pre>
     */
    def be[U](interval: Interval[U]): Matcher[T with U] = leftMatcher.and(MatcherWords.not.be(interval))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimal))
     *                                                     ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include regex ("bob") and not include regex (decimal))
     *                                                     ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include ("bob") and not include ("1.7"))
     *                                            ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith regex ("bob") and not startWith regex (decimal))
     *                                                    ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith ("red") and not startWith ("1.7"))
     *                                              ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith regex ("bob") and not endWith regex (decimal))
     *                                                  ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith ("fre") and not endWith ("1.7"))
     *                                            ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[T with String] =
      leftMatcher.and(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (5) and not contain (3))
     *                                                     ^
     * </pre>
     */
    def contain[U](expectedElement: U): Matcher[T with GenTraversable[U]] =
      leftMatcher.and(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain key ("five") and not contain key ("three"))
     *                                                                      ^
     * </pre>
     */
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
      leftMatcher.and(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (5) and not contain value (3))
     *                                                                   ^
     * </pre>
     */
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
      leftMatcher.and(MatcherWords.not.contain(resultOfValueWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain theSameElementsAs (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def contain[U](right: ContainMatcher[U]): Matcher[T with GenTraversable[U]] =
      leftMatcher.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain a negativeNumber and not contain a primeNumber)
     *                                                     ^
     * </pre>
     */
    def contain[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      leftMatcher.and(MatcherWords.not.contain(resultOfAWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain an oddNumber and not contain an invalidNumber)
     *                                                 ^
     * </pre>
     */
    def contain[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      leftMatcher.and(MatcherWords.not.contain(resultOfAnWordApplication))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (5) and not contain value (3))
   *                                                           ^
   * </pre>
   */
  def and(notWord: NotWord): AndNotWord = new AndNotWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrHaveWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have length (2) and have length (3 - 1))
     *                                              ^
     * </pre>
     */
    def length(expectedLength: Long): Matcher[T with AnyRef] = or(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                       ^
     * </pre>
     */
    def size(expectedSize: Long): Matcher[T with AnyRef] = or(MatcherWords.have.size(expectedSize))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (have size (2) and have size (3 - 1))
   *                                   ^
   * </pre>
   */
  def or(haveWord: HaveWord): OrHaveWord = new OrHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (contain (2) or contain (3 - 1))
     *                                            ^
     * </pre>
     */
    def apply[U](expectedElement: U): Matcher[T with GenTraversable[U]] = leftMatcher.or(MatcherWords.contain(expectedElement))
    // def element[T](expectedElement: T) = leftMatcher.or(MatcherWords.contain.apply(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("cat") or contain key ("one"))
     *                                                                    ^
     * </pre>
     */
    def key[U](expectedKey: U): Matcher[T with scala.collection.GenMap[U, Any]] = leftMatcher.or(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (7) or contain value (1))
     *                                                                  ^
     * </pre>
     */
    def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = leftMatcher.or(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain theSameElementsAs List(1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def theSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.theSameElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain theSameIteratedElementsAs List(1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def theSameIteratedElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.theSameIteratedElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain allOf (1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def allOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.allOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain inOrder (1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def inOrder[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.inOrder(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain oneOf (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def oneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.oneOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain only (3, 1))
     *                                                                          ^
     * </pre>
     */
    def only[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.only(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain inOrderOnly (1, 3))
     *                                                                          ^
     * </pre>
     */
    def inOrderOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.inOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain noneOf (7, 8, 9))
     *                                                                          ^
     * </pre>
     */
    def noneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      leftMatcher.or(MatcherWords.contain.noneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) or contain a (validNumber))
     *                                                      ^
     * </pre>
     */
    def a[E](aMatcher: AMatcher[E]): Matcher[T with GenTraversable[E]] = 
      or(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain an (positiveNumber) or contain an (validNumber))
     *                                                       ^
     * </pre>
     */
    def an[E](anMatcher: AnMatcher[E]): Matcher[T with GenTraversable[E]] = 
      or(MatcherWords.contain.an(anMatcher))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (7) or contain value (1))
   *                                                       ^
   * </pre>
   */
  def or(containWord: ContainWord): OrContainWord = new OrContainWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrBeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a ('file) or be a ('directory))
     *                                       ^
     * </pre>
     */
    def a(symbol: Symbol): Matcher[T with AnyRef] = or(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a (file) or be a (directory))
     *                                      ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(MatcherWords.be.a(bePropertyMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be a (positiveNumber) or be a (validNumber))
     *                                            ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): Matcher[T with U] = or(MatcherWords.be.a(aMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * appleMock should (be an ('orange) or be an ('apple))
     *                                         ^
     * </pre>
     */
    def an(symbol: Symbol): Matcher[T with AnyRef] = or(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * appleMock should (be an (orange) or be an (apple))
     *                                        ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(MatcherWords.be.an(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be an (oddNumber) or be an (integerNumber))
     *                                        ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): Matcher[T with U] = or(MatcherWords.be.an(anMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (be theSameInstanceAs (string) or be theSameInstanceAs (otherString))
     *                                                 ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = or(MatcherWords.be.theSameInstanceAs(anyRef))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * isFileMock should (be a ('file) or be a ('directory))
   *                                 ^
   * </pre>
   */
  def or(beWord: BeWord): OrBeWord = new OrBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrFullyMatchWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
     *                                                        ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
     *                                                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
   *                                          ^
   * </pre>
   */
  def or(fullyMatchWord: FullyMatchWord): OrFullyMatchWord = new OrFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrIncludeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include regex ("hello") or include regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include regex ("hello") or include regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "a1.7b" should (include regex ("1.7") or include regex ("1.7"))
   *                                          ^
   * </pre>
   */
  def or(includeWord: IncludeWord): OrIncludeWord = new OrIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrStartWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
     *                                                      ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
     *                                                      ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (startWith regex ("hello") or startWith regex ("1.7"))
   *                                            ^
   * </pre>
   */
  def or(startWithWord: StartWithWord): OrStartWithWord = new OrStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrEndWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7b" should (endWith regex ("hello") or endWith regex ("7b"))
   *                                           ^
   * </pre>
   */
  def or(endWithWord: EndWithWord): OrEndWithWord = new OrEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrNotWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 1 should (not equal (1) or not equal (2))
     *                                ^
     * </pre>
     */
    def equal(any: Any): Matcher[T] =
      leftMatcher.or(MatcherWords.not.apply(MatcherWords.legacyEqual(any)))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not equal (17.0 plusOrMinus 0.2) or not equal (17.0 plusOrMinus 0.2))
     *                                                        ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.equal(interval))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aNullRef should (not equal (null) or not equal (null))
     *                                   ^
     * </pre>
     */
    def equal(o: Null): Matcher[T] = {
      leftMatcher or {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
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
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 1 should (not be (1) or not be (2))
     *                             ^
     * </pre>
     */
    def be(any: Any): Matcher[T] =
      leftMatcher.or(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have length (2) or not have length (3))
     *                                                ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[T with AnyRef] =
      leftMatcher.or(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (2) or not have size (3))
     *                                              ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[T with AnyRef] =
      leftMatcher.or(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) or not have (author ("Melville")))
     *                                                    ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
      leftMatcher.or(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should (contain key (7) or not be (null))
     *                                    ^
     * </pre>
     */
    def be(o: Null): Matcher[T with AnyRef] = leftMatcher.or(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be &lt; (7) or not be &lt; (8))
     *                               ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
      leftMatcher.or(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 7 should (not be &gt; (5) or not be &gt; (6))
     *                               ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
      leftMatcher.or(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be &lt;= (3) or not be &lt;= (2))
     *                                ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
      leftMatcher.or(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 8 should (not be &gt;= (7) or not be &gt;= (6))
     *                                ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
      leftMatcher.or(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be === (7) or not be === (8))
     *                                 ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
      leftMatcher.or(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notEmptyMock should (not be ('full) or not be ('empty))
     *                                            ^
     * </pre>
     */
    def be(symbol: Symbol): Matcher[T with AnyRef] = leftMatcher.or(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be (even) or not be (odd))
     *                                ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be (directory) or not be (file))
     *                                          ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = leftMatcher.or(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('directory) or not be a ('file))
     *                                                    ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = leftMatcher.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be a (passedMarks) or not be a (validMarks))
     *                                                 ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be a (directory) or not be a (file))
     *                                            ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notAppleMock should (not be an ('apple) or not be an ('apple))
     *                                                ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = leftMatcher.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) or not be an (file))
     *                                             ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be an (oddMarks) and not be an (invalidMarks))
     *                                                ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (not be theSameInstanceAs (otherString) or not be theSameInstanceAs (string))
     *                                                           ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = leftMatcher.or(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not be (17.0 plusOrMinus 0.2) or not be (17.0 plusOrMinus 0.2))
     *                                                         ^
     * </pre>
     */
    def be[U](interval: Interval[U]): Matcher[T with U] = leftMatcher.or(MatcherWords.not.be(interval))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimal))
     *                                                     ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include regex ("fred") or not include regex (decimal))
     *                                                  ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include ("bob") or not include ("1.7"))
     *                                           ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith regex ("bob") or not startWith regex (decimal))
     *                                                   ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith ("fred") or not startWith ("1.7"))
     *                                              ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith regex ("bob") or not endWith regex (decimal))
     *                                                 ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith ("fred") or not endWith ("1.7"))
     *                                            ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[T with String] =
      leftMatcher.or(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (1) or not contain (3))
     *                                            ^
     * </pre>
     */
    def contain[U](expectedElement: U): Matcher[T with GenTraversable[U]] =
      leftMatcher.or(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain key ("two") or not contain key ("three"))
     *                                                                    ^
     * </pre>
     */
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
      leftMatcher.or(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (2) or not contain value (3))
     *                                                                  ^
     * </pre>
     */
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
      leftMatcher.or(MatcherWords.not.contain(resultOfValueWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain theSameElementsAs (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def contain[U](right: ContainMatcher[U]): Matcher[T with GenTraversable[U]] =
      leftMatcher.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain a negativeNumber or not contain a primeNumber)
     *                                                    ^
     * </pre>
     */
    def contain[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      leftMatcher.or(MatcherWords.not.contain(resultOfAWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain an oddNumber or not contain an invalidNumber)
     *                                                ^
     * </pre>
     */
    def contain[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      leftMatcher.or(MatcherWords.not.contain(resultOfAnWordApplication))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (2) or not contain value (3))
   *                                                           ^
   * </pre>
   */
  def or(notWord: NotWord): OrNotWord = new OrNotWord
}
