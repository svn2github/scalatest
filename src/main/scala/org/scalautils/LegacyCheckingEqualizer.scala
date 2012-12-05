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
package org.scalautils

/**
 * Class used via an implicit conversion to enable any two objects to be compared with
 * <code>===</code> in assertions in tests. For example:
 *
 * <pre class="stHighlight">
 * assert(a === b)
 * </pre>
 *
 * <p>
 * The benefit of using <code>assert(a === b)</code> rather than <code>assert(a == b)</code> is
 * that a <code>TestFailedException</code> produced by the former will include the values of <code>a</code> and <code>b</code>
 * in its detail message.
 * The implicit method that performs the conversion from <code>Any</code> to <code>Equalizer</code> is
 * <code>convertToEqualizer</code> in trait <code>Assertions</code>.
 * </p>
 *
 * <p>
 * In case you're not familiar with how implicit conversions work in Scala, here's a quick explanation.
 * The <code>convertToEqualizer</code> method in <code>Assertions</code> is defined as an "implicit" method that takes an
 * <code>Any</code>, which means you can pass in any object, and it will convert it to an <code>Equalizer</code>.
 * The <code>Equalizer</code> has <code>===</code> defined. Most objects don't have <code>===</code> defined as a method
 * on them. Take two Strings, for example:
 * </p>
 *
 * <pre class="stHighlight">
 * assert("hello" === "world")
 * </pre>
 *
 * <p>
 * Given this code, the Scala compiler looks for an <code>===</code> method on class <code>String</code>, because that's the class of
 * <code>"hello"</code>. <code>String</code> doesn't define <code>===</code>, so the compiler looks for an implicit conversion from
 * <code>String</code> to something that does have an <code>===</code> method, and it finds the <code>convertToEqualizer</code> method. It
 * then rewrites the code to this:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(convertToEqualizer("hello").===("world"))
 * </pre>
 *
 * <p>
 * So inside a <code>Suite</code> (which mixes in <code>Assertions</code>, <code>===</code> will work on anything. The only
 * situation in which the implicit conversion wouldn't 
 * happen is on types that have an <code>===</code> method already defined.
 * </p>
 * 
 * <p>
 * The primary constructor takes one object, <code>left</code>, whose type is being converted to <code>Equalizer</code>. The <code>left</code>
 * value may be a <code>null</code> reference, because this is allowed by Scala's <code>==</code> operator.
 * </p>
 *
 * @param left An object to convert to <code>Equalizer</code>, which represents the <code>left</code> value
 *     of an assertion.
 *
 * @author Bill Venners
 */
// final class Equalizer(left: Any) {
  /**
   * The <code>===</code> operation compares this <code>Equalizer</code>'s <code>left</code> value (passed
   * to the constructor, usually via an implicit conversion) with the passed <code>right</code> value 
   * for equality as determined by the expression <code>left == right</code>.
   * If <code>true</code>, <code>===</code> returns <code>None</code>. Else, <code>===</code> returns
   * a <code>Some</code> whose <code>String</code> value indicates the <code>left</code> and <code>right</code> values.
   *
   * <p>
   * In its typical usage, the <code>Option[String]</code> returned by <code>===</code> will be passed to one of two
   * of trait <code>Assertion</code>' overloaded <code>assert</code> methods. If <code>None</code>,
   * which indicates the assertion succeeded, <code>assert</code> will return normally. But if <code>Some</code> is passed,
   * which indicates the assertion failed, <code>assert</code> will throw a <code>TestFailedException</code> whose detail
   * message will include the <code>String</code> contained inside the <code>Some</code>, which in turn includes the
   * <code>left</code> and <code>right</code> values. This <code>TestFailedException</code> is typically embedded in a 
   * <code>Report</code> and passed to a <code>Reporter</code>, which can present the <code>left</code> and <code>right</code>
   * values to the user.
   * </p>
   */
/*
  def ===(right: Any) = {
    def diffStrings(s: String, t: String): Tuple2[String, String] = {
      def findCommonPrefixLength(s: String, t: String): Int = {
        val max = s.length.min(t.length) // the maximum potential size of the prefix
        var i = 0
        var found = false
        while (i < max & !found) {
          found = (s.charAt(i) != t.charAt(i))
          if (!found)
            i = i + 1
        }
        i
      }
      def findCommonSuffixLength(s: String, t: String): Int = {
        val max = s.length.min(t.length) // the maximum potential size of the suffix
        var i = 0
        var found = false
        while (i < max & !found) {
          found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
          if (!found)
            i = i + 1
        }
        i
      }
      val commonPrefixLength = findCommonPrefixLength(s, t)
      val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
      val prefix = s.substring(0, commonPrefixLength)
      val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
      val sMiddleEnd = s.length - commonSuffixLength
      val tMiddleEnd = t.length - commonSuffixLength
      val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
      val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
      val MaxContext = 20
      val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
      val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
      (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
    }

    // If the objects are two strings, replace them with whatever is returned by diffStrings.
    // Otherwise, use the same objects.
    def getObjectsForFailureMessage(a: Any, b: Any) =
      a match {
        case aStr: String => {
          b match {
            case bStr: String => {
              diffStrings(aStr, bStr)
            }
            case _ => (a, b)
          }
        }
        case _ => (a, b)
      }

    def areEqualComparingArraysStructurally(left: Any, right: Any) = {
      left match {
        case leftArray: Array[_] =>
          right match {
            case rightArray: Array[_] => leftArray.deep.equals(rightArray.deep)
            case _ => left == right
        }
        case _ => left == right
      }
    }
    if (areEqualComparingArraysStructurally(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("didNotEqual", leftee, rightee))
      //Some(leftee + " did not equal " + rightee)
    }
  }
*/
/*
  def !==(right: Any) =
    if (left != right)
      None
    else {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
      Some(FailureMessages("equaled", leftee, rightee))
    }
*/
// }
class LegacyCheckingEqualizer[L](left: L) {

  private def diffStrings(s: String, t: String): Tuple2[String, String] = {
    def findCommonPrefixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the prefix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(i) != t.charAt(i))
        if (!found)
          i = i + 1
      }
      i
    }
    def findCommonSuffixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the suffix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
        if (!found)
          i = i + 1
      }
      i
    }
    val commonPrefixLength = findCommonPrefixLength(s, t)
    val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
    val prefix = s.substring(0, commonPrefixLength)
    val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
    val sMiddleEnd = s.length - commonSuffixLength
    val tMiddleEnd = t.length - commonSuffixLength
    val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
    val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
    val MaxContext = 20
    val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
    val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
    (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
  }

  // If the objects are two strings, replace them with whatever is returned by diffStrings.
  // Otherwise, use the same objects.
  def getObjectsForFailureMessage(a: Any, b: Any) =
    a match {
      case aStr: String => {
        b match {
          case bStr: String => {
            diffStrings(aStr, bStr)
          }
          case _ => (a, b)
        }
      }
      case _ => (a, b)
    }

  def ===[R](right: R)(implicit constraint: EqualityConstraint[L, R]): Option[String] = 
    if (constraint.areEqual(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("didNotEqual", leftee, rightee))
    }

  def !==[R](right: R)(implicit constraint: EqualityConstraint[L, R]): Option[String] =
    if (!constraint.areEqual(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("equaled", leftee, rightee))
    }

  def ===(interval: Interval[L]): Option[String] =
    if (interval == null) {
      if (left == null)
        None
      else {
        val (leftee, rightee) = getObjectsForFailureMessage(left, interval)
        Some(FailureMessages("equaled", leftee, rightee))
      }
    }
    else {
      if (interval.isWithin(left))
        None
      else
        Some(FailureMessages("wasNotPlusOrMinus", left, interval.right, interval.tolerance))
    }

  def !==(interval: Interval[L]): Option[String] =
    if (interval == null) {
      if (left != null)
        None
      else {
        val (leftee, rightee) = getObjectsForFailureMessage(left, interval)
        Some(FailureMessages("equaled", leftee, rightee))
      }
    }
    else {
      if (if (interval != null) !interval.isWithin(left) else left != interval)
        None
      else
        Some(FailureMessages("wasPlusOrMinus", left, interval.right, interval.tolerance))
    }
  }

