package org.scalatest.matchers

import org.scalatest._
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.util.matching.Regex
import org.scalautils.TripleEqualsInvocation

trait InspectorsShorthands extends matchers.ClassicMatchers {
  
  import org.scalatest.InspectorsHelper._
  
  private sealed trait Collected
  private case object AllCollected extends Collected
  private case object EveryCollected extends Collected
  private case class BetweenCollected(from: Int, to: Int) extends Collected
  private case class AtLeastCollected(num: Int) extends Collected
  private case class AtMostCollected(num: Int) extends Collected
  private case object NoCollected extends Collected
  private case class ExactlyCollected(num: Int) extends Collected
  
  def doCollected[T](collected: Collected, xs: GenTraversable[T], methodName: String, stackDepth: Int)(fun: T => Unit) {
    collected match {
      case AllCollected =>
        forAll(xs, "InspectorsShorthands.scala", methodName, stackDepth) { e => 
          fun(e)
        }
      case AtLeastCollected(num) => 
        forAtLeast(num, xs, "InspectorsShorthands.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case EveryCollected => 
        forEvery(xs, "InspectorsShorthands.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case ExactlyCollected(num) => 
        forExactly(num, xs, "InspectorsShorthands.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case NoCollected =>
        forNo(xs, "InspectorsShorthands.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case BetweenCollected(from, to) =>
        forBetween(from, to, xs, "InspectorsShorthands.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case AtMostCollected(num) =>
        forAtMost(num, xs, "InspectorsShorthands.scala", methodName, stackDepth) { e =>
          fun(e)
        }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedAny[T](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) {

    import org.scalatest.InspectorsHelper._
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (7)
     *                    ^
     * </pre>
     */
    def equal(right: Any) {
      doCollected(collected, xs, "equal", 1) { e =>
        if ((e == right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEqual" else "equaled",
              e,
              right
            ), 
            None, 
            10
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (7)
     *                    ^
     * </pre>
     */
    def be(right: Any) {
      doCollected(collected, xs, "be", 1) { e =>
        if ((e == right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
              e,
              right
            ), 
            None, 
            10
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be <= (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            10
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be >= (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            10
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be < (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
              e,
              comparison.right
            ), 
            None, 
            10
          ) 
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be > (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
              e,
              comparison.right
            ), 
            None, 
            10
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be === (7)
     *                    ^
     * </pre>
     */
    def be(comparison: TripleEqualsInvocation[_]) {
      doCollected(collected, xs, "be", 1) { e => 
        if ((e == comparison.right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
              e,
              comparison.right
            ), 
            None, 
            10
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to
     * a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (odd)
     *                    ^
     * </pre>
     */
    def be(beMatcher: BeMatcher[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = beMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              result.failureMessage
            else
              result.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
     * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (empty)
     *                    ^
     * </pre>
     */
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNot", e, UnquotedString(result.propertyName))
            else
              FailureMessages("was", e, UnquotedString(result.propertyName)), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be a (file)
     *                    ^
     * </pre>
     */
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = resultOfAWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotA", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasA", e, UnquotedString(result.propertyName)), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * all(keyEvents) should not be an (actionKey)
     *                           ^
     * </pre>
     */
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = resultOfAnWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotAn", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasAn", e, UnquotedString(result.propertyName)), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be theSameInstanceAs (string)
     *                    ^
     * </pre>
     */
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        e match {
          case ref: AnyRef =>
            if ((resultOfSameInstanceAsApplication.right eq ref) != shouldBeTrue) {
              throw newTestFailedException(
                FailureMessages(
                  if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
                  e,
                  resultOfSameInstanceAsApplication.right
                ), 
                None, 
                10
              )
            }
          case _ => 
            throw new IllegalArgumentException("theSameInstanceAs should only be used for AnyRef")
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should not have (title ("One Hundred Years of Solitude"))
     *                       ^
     * </pre>
     */
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {
      doCollected(collected, xs, "have", 1) { e => 
      
        val results =
          for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
            propertyVerifier(e)

        val firstFailureOption = results.find(pv => !pv.matches)

        val justOneProperty = propertyMatchers.length == 0

        // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
        // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
        if (firstFailureOption.isDefined == shouldBeTrue) {
          firstFailureOption match {
            case Some(firstFailure) =>
              // This is one of these cases, thus will only get here if shouldBeTrue is true
              // 0 0 | 0 | 1
              // 0 1 | 0 | 1
              // 1 0 | 0 | 1
              throw newTestFailedException(
                FailureMessages(
                  "propertyDidNotHaveExpectedValue",
                  UnquotedString(firstFailure.propertyName),
                  firstFailure.expectedValue,
                  firstFailure.actualValue,
                  e
                ), 
                None, 
                10
              )
            case None =>
              // This is this cases, thus will only get here if shouldBeTrue is false
              // 1 1 | 1 | 0
              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "propertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    e
                  )
                }
                else FailureMessages("allPropertiesHadExpectedValues", e)

              throw newTestFailedException(failureMessage, None, 10)
          } 
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
    extends ResultOfNotWordForCollectedAny(collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (null)
     *                    ^
     * </pre>
     */
    def be(o: Null) {
      doCollected(collected, xs, "be", 1) { e => 
        if ((e == null) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotNull", e) 
            else
              FailureMessages("wasNull"), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be ('empty)
     *                    ^
     * </pre>
     */
    def be(symbol: Symbol) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, false, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be a ('file)
     *                    ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, resultOfAWordApplication.symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be an ('actionKey)
     *                    ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              10
            )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[String](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not have length (12)
     *                        ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not have size (12)
     *                        ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not startWith ("1.7")
     *                        ^
     * </pre>
     */
    def startWith(right: String) {
      doCollected(collected, xs, "startWith", 1) { e =>
        if ((e.indexOf(right) == 0) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWith" else "startedWith",
              e,
              right
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not startWith regex ("Hel*o")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "startWith", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.pattern.matcher(e).lookingAt != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not endWith ("1.7")
     *                        ^
     * </pre>
     */
    def endWith(expectedSubstring: String) {
      doCollected(collected, xs, "endWith", 1) { e =>
        if ((e endsWith expectedSubstring) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWith" else "endedWith",
              e,
              expectedSubstring
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not endWith regex ("wor.d")
     *                        ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "endWith", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        val allMatches = rightRegex.findAllIn(e)
        if (allMatches.hasNext && (allMatches.end == e.length) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not include regex ("wo.ld")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "include", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.findFirstIn(e).isDefined != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
              e,
              rightRegex
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not include ("world")
     *                        ^
     * </pre>
     */
    def include(expectedSubstring: String) {
      doCollected(collected, xs, "include", 1) { e =>
        if ((e.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeSubstring" else "includedSubstring",
              e,
              expectedSubstring
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "fullyMatch", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.pattern.matcher(e).matches != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
              e,
              rightRegex
            ), 
            None, 
            10
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedGenTraversable[E, T <: GenTraversable[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not have size (12)
     *                                          ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not have length (12)
     *                                          ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not contain ("one")
     *                                          ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.exists(_ == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedGenSeq[E, T <: GenSeq[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedGenTraversable[E, T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(seqOfSeq) should not have length (12)
     *                          ^
     * </pre>
     */
    override def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedArray[E, T <: Array[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be ('empty)
     *                            ^
     * </pre>
     */
    override def be(symbol: Symbol) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, symbol, false, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be a ('file)
     *                            ^
     * </pre>
     */
    override def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, resultOfAWordApplication.symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be an ('actionKey)
     *                            ^
     * </pre>
     */
    override def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              10
            )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfArray) should not have size (12)
     *                                    ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfArray) should not contain ("one")
     *                                    ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.exists(_ == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(seqOfArray) should not have length (12)
     *                            ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedGenMap[K, V, T <: GenMap[K, V]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedGenTraversable[(K, V), T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain key ("three")
     *                          ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfKeyWordApplication.expectedKey
        if ((e.exists(_._1 == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain value (3)
     *                          ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfValueWordApplication.expectedValue
        if ((e.exists(_._2 == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfNotWordForCollectedJavaCollection[E, T <: java.util.Collection[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not have size (3)
     *                                     ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not have length (12)
     *                                     ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not contain ("elephant")
     *                                     ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.contains(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedJavaMap[K, V, T <: java.util.Map[K, V]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue){
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have size (3)
     *                              ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have length (12)
     *                              ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not contain key ("three")
     *                              ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfKeyWordApplication.expectedKey
        if ((e.containsKey(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not contain value (3)
     *                              ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfValueWordApplication.expectedValue
        if ((e.containsValue(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              right
            ), 
            None, 
            10
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfBeWordForCollectedAny[T](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfBeWordForCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
    extends ResultOfBeWordForCollectedAny(collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      doCollected(collected, xs, "theSameInstanceAs", 1) { e =>
        if ((e eq right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
              e,
              right
            ),
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be a ('file)
     *                   ^
     * </pre>
     */
    def a(symbol: Symbol) {
      doCollected(collected, xs, "a", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be an ('orange)
     *                   ^
     * </pre>
     */
    def an(symbol: Symbol) {
      doCollected(collected, xs, "an", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be a (goodRead)
     *                      ^
     * </pre>
     */
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "a", 1) { e =>
        val result = bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotA", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasA", e, UnquotedString(result.propertyName)), 
            None, 
            10
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be an (excellentRead)
     *                      ^
     * </pre>
     */
    def an(beTrueMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "an", 1) { e =>
        val beTrueMatchResult = beTrueMatcher(e)
        if (beTrueMatchResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotAn", e, UnquotedString(beTrueMatchResult.propertyName))
            else
              FailureMessages("wasAn", e, UnquotedString(beTrueMatchResult.propertyName)), 
            None, 
            10
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfBeWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) 
    extends ResultOfBeWordForCollectedAnyRef(collected, xs, shouldBeTrue) {
  
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should be ('empty)
     *                           ^
     * </pre>
     */
    def apply(right: Symbol): Matcher[Array[T]] =
      new Matcher[Array[T]] {
        def apply(left: Array[T]): MatchResult = matchSymbolToPredicateMethod(left.deep, right, false, false)
      }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) {
  
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should contain (element)
     *                        ^
     * </pre>
     */
    def apply(expectedElement: T): Matcher[Array[T]] = 
      new Matcher[Array[T]] {
        def apply(left: Array[T]): MatchResult =
          MatchResult(
            left.exists(_ == expectedElement), 
            FailureMessages("didNotContainExpectedElement", left, expectedElement),
            FailureMessages("containedExpectedElement", left, expectedElement)
          )
      }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfCollectedAny[T](collected: Collected, xs: GenTraversable[T]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should(rightMatcher: Matcher[T]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAny[T](collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedAny[T] = 
      new ResultOfNotWordForCollectedAny(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should(rightMatcher: Matcher[T]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef[T](collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedAnyRef[T] = 
      new ResultOfNotWordForCollectedAnyRef(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedString(collected: Collected, xs: GenTraversable[String]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should equal ("hi")
     *             ^
     * </pre>
     */
    def should(rightMatcher: Matcher[String]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should be theSameInstanceAs anotherObject
     *             ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should have length (3)
     *             ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedString = 
      new ResultOfHaveWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should not have length (3)
     *             ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedString = 
      new ResultOfNotWordForCollectedString(collected, xs, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *             ^
     * </pre>
     */
    def should(startWithWord: StartWithWord): ResultOfStartWithWordForCollectedString = 
      new ResultOfStartWithWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(endWithWord: EndWithWord): ResultOfEndWithWordForCollectedString = 
      new ResultOfEndWithWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(includeWord: IncludeWord): ResultOfIncludeWordForCollectedString = 
      new ResultOfIncludeWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForCollectedString = 
      new ResultOfFullyMatchWordForCollectedString(collected, xs, true)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should have length (12)
     *                         ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.length == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should have size (12)
     *                         ^
     * </pre>
     */
    def size(expectedSize: Int) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            10
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfStartWithWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *                              ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o".r)
     *                              ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.pattern.matcher(e).lookingAt != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfIncludeWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("world")
     *                            ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.findFirstIn(e).isDefined != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfEndWithWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d")
     *                            ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        val allMatches = rightRegex.findAllIn(e)
        if ((allMatches.hasNext && (allMatches.end == e.length)) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfFullyMatchWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("Hel*o world")
     *                              ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullymatch regex ("Hel*o world".r)
     *                               ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.pattern.matcher(e).matches != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have size (3)
     *                       ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedGenTraversable[T] = 
      new ResultOfHaveWordForCollectedGenTraversable(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should equal (Set(1, 2, 3))
     *                       ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenTraversable[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be theSameInstanceAs anotherObject
     *                       ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should not have size (3)
     *                       ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenTraversable[T, GenTraversable[T]] = 
      new ResultOfNotWordForCollectedGenTraversable(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have size (12)
     *                                   ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have length (12)
     *                                   ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.size == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            10
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedGenSeq[T](collected: Collected, xs: GenTraversable[GenSeq[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have length (3)
     *               ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedGenSeq[T] = 
      new ResultOfHaveWordForCollectedGenSeq(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should equal (List(1, 2, 3))
     *               ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenSeq[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should be theSameInstanceAs anotherObject
     *               ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should not have length (3)
     *               ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenSeq[T, GenSeq[T]] = 
      new ResultOfNotWordForCollectedGenSeq(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedGenSeq[T](collected: Collected, xs: GenTraversable[GenSeq[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have length (12)
     *                           ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.length == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have size (12)
     *                           ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            10
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have size (3)
     *                 ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedArray[T] = 
      new ResultOfHaveWordForCollectedArray(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should equal (Set(1, 2, 3))
     *                       ^
     * </pre>
     */
    def should[T](rightMatcher: Matcher[GenTraversable[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e.deep.asInstanceOf[IndexedSeq[T]]) match {  // TODO: Ugly but safe cast here because e is Array[T]
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should be theSameInstanceAs anotherObject
     *                 ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedArray(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not have size (3)
     *                 ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedArray[T, Array[T]] = 
      new ResultOfNotWordForCollectedArray(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have size (12)
     *                             ^
     * </pre>
     */
    def size(expectedSize: Int) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have length (12)
     *                             ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.length == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            10
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedGenMap[K, V](collected: Collected, xs: GenTraversable[GenMap[K, V]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key (10)
     *               ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedGenMap[K, V] = 
      new ResultOfContainWordForCollectedGenMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should equal (Map(1 -> "one", 2 -> "two"))
     *               ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenMap[K, V]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should be theSameInstanceAs (anotherMap)
     *               ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not have size (3)
     *               ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenMap[K, V, GenMap[K, V]] = 
      new ResultOfNotWordForCollectedGenMap(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedGenMap[K, V](collected: Collected, xs: GenTraversable[GenMap[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key ("one")
     *                              ^
     * </pre>
     */
    def key(expectedKey: K) {
      doCollected(collected, xs, "key", 1) { e =>
        if (e.exists(_._1 == expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              expectedKey), 
              None, 
              10
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain value (1)
     *                              ^
     * </pre>
     */
    def value(expectedValue: V) {
      doCollected(collected, xs, "value", 1) { e =>
        if (e.exists(expectedValue == _._2) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              expectedValue), 
            None, 
            10
          )
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have size (3)
     *                   ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedJavaCollection[T] = 
      new ResultOfHaveWordForCollectedJavaCollection(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should equal (aJavaSet)
     *                   ^
     * </pre>
     */
    def should(rightMatcher: Matcher[java.util.Collection[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should not have size (3)
     *                   ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedJavaCollection[T, java.util.Collection[T]] = 
      new ResultOfNotWordForCollectedJavaCollection(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have size (10)
     *                   ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize), 
            None, 
            10
          )
       }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have length (12)
     *                   ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.size == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength), 
            None, 
            10
          )
        }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have size (3)
     *                   ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedJavaMap[K, V] = 
      new ResultOfHaveWordForCollectedJavaMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain value (3)
     *                   ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedJavaMap[K, V] = 
      new ResultOfContainWordForCollectedJavaMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should equal (someJavaMap)
     *                   ^
     * </pre>
     */
    def should(rightMatcher: Matcher[java.util.Map[K, V]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have length (3)
     *                   ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedJavaMap[K, V, java.util.Map[K, V]] = 
      new ResultOfNotWordForCollectedJavaMap(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have size (10)
     *                               ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize), 
            None, 
            10
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have length (10)
     *                               ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.size == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength), 
            None, 
            10
          )
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain key ("two")
     *                        ^
     * </pre>
     */
    def key(expectedKey: K) {
      doCollected(collected, xs, "key", 1) { e =>
        if (e.containsKey(expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              expectedKey), 
            None, 
            10
          )
      }
    }

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain value ("2")
     *                        ^
     * </pre>
     */
    def value(expectedValue: V) {
      doCollected(collected, xs, "value", 1) { e =>
        if (e.containsValue(expectedValue) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              expectedValue), 
              None, 
              10
          )
      }
    }
    
  }

  def all[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(AllCollected, xs)
  
  def all(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(AllCollected, xs)
  
  def all(xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(AllCollected, xs)
  
  def all[T](xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(AllCollected, xs)
  
  def all[T](xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(AllCollected, xs)
  
  def all[T](xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(AllCollected, xs)
  
  def all[K, V](xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(AllCollected, xs)
  
  def all[T](xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(AllCollected, xs)
  
  def all[K, V](xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(AllCollected, xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(AtLeastCollected(num), xs)
  
  def atLeast(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(AtLeastCollected(num), xs)
  
  def atLeast(num: Int, xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(AtLeastCollected(num), xs)
  
  def atLeast[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(AtLeastCollected(num), xs)
  
  def atLeast[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(AtLeastCollected(num), xs)
  
  def every[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(EveryCollected, xs)
  
  def every(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(EveryCollected, xs)
  
  def every(xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(EveryCollected, xs)
  
  def every[K, V](xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(EveryCollected, xs)
  
  def every[K, V](xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(EveryCollected, xs)
  
  def exactly[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(ExactlyCollected(num), xs)
  
  def exactly(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(ExactlyCollected(num), xs)
  
  def exactly(num: Int, xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(ExactlyCollected(num), xs)
  
  def exactly[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(ExactlyCollected(num), xs)
  
  def exactly[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(ExactlyCollected(num), xs)

  def no[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(NoCollected, xs)

  def no(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(NoCollected, xs)

  def no(xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(NoCollected, xs)

  def no[T](xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(NoCollected, xs)

  def no[T](xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(NoCollected, xs)

  def no[T](xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(NoCollected, xs)

  def no[K, V](xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(NoCollected, xs)

  def no[T](xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(NoCollected, xs)

  def no[K, V](xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(NoCollected, xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), xs)

  def between(from: Int, upTo:Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(BetweenCollected(from, upTo), xs)

  def between(from: Int, upTo:Int, xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(BetweenCollected(from, upTo), xs)

  def between[K, V](from: Int, upTo:Int, xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(BetweenCollected(from, upTo), xs)

  def between[K, V](from: Int, upTo:Int, xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(BetweenCollected(from, upTo), xs)

  def atMost[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(AtMostCollected(num), xs)

  def atMost(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(AtMostCollected(num), xs)

  def atMost(num: Int, xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(AtMostCollected(num), xs)

  def atMost[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(AtMostCollected(num), xs)

  def atMost[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(AtMostCollected(num), xs)
}