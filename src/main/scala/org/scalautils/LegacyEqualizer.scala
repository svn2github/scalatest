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
class LegacyEqualizer[L](left: L) {

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

  def ===(right: Any)(implicit equality: Equality[L]): Option[String] = 
    if (equality.areEqual(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("didNotEqual", leftee, rightee))
    }

  def !==(right: Any)(implicit equality: Equality[L]): Option[String] =
    if (!equality.areEqual(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("equaled", leftee, rightee))
    }

/*
  def ===(interval: Interval[L]): Option[String] =
    if (if (interval != null) interval.isWithin(left) else left == interval)
      None
    else
      Some(FailureMessages("wasNotPlusOrMinus", left, interval.right, interval.tolerance))

  def !==(interval: Interval[L]): Option[String] =
    if (if (interval != null) !interval.isWithin(left) else left != interval)
      None
    else
      Some(FailureMessages("wasPlusOrMinus", left, interval.right, interval.tolerance))
*/

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

