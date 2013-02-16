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
 * Class used via an implicit conversion to enable any two objects to be compared with
 * <code>===</code> and <code>!==</code> with a <code>Boolean</code> result and no enforced type constraint between
 * two object types. For example:
 *
 * <pre class="stHighlight">
 * assert(a === b)
 * assert(c !== d)
 * </pre>
 *
 * <p>
 * You can also check numeric values against another with a tolerance. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(a === (2.0 +- 0.1))
 * assert(c !== (2.0 +- 0.1))
 * </pre>
 *
 * @param left An object to convert to <code>Equalizer</code>, which represents the value
 *     on the left side of a <code>===</code> or <code>!==</code> invocation.
 *
 * @author Bill Venners
 */
class Equalizer[L](left: L) {

  /**
   * Compare two objects for equality, returning a <code>Boolean</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
   *
   * @param right the object to compare for equality with <code>left</code>, passed to the constructor
   * @param equality an implicit <code>Equality</code> type class that defines a way of calculating equality for objects of type <code>L</code>
   * @return true if the <code>left</code> and <code>right</code> objects are equal according to the passed <code>Equality</code> type class.
   */
  def ===(right: Any)(implicit equality: Equality[L]): Boolean = equality.areEqual(left, right)

  /**
   * Compare two objects for inequality, returning a <code>Boolean</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
   *
   * @param right the object to compare for inequality with <code>left</code>, passed to the constructor
   * @param equality an implicit <code>Equality</code> type class that defines a way of calculating equality for objects of type <code>L</code>
   * @return true if the <code>left</code> and <code>right</code> objects are <em>not</em> equal according to the passed <code>Equality</code> type class.
   */
  def !==(right: Any)(implicit equality: Equality[L]): Boolean = !equality.areEqual(left, right)

  /**
   * Determine whether a numeric object is within the passed <code>Interval</code>, returning a <code>Boolean</code>.
   *
   * @param interval the <code>Interval</code> against which to compare the value passed to the constructor as <code>left</code> 
   * @return true if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Interval</code> passed to this method.
   */
  def ===(interval: Interval[L]): Boolean = if (interval != null) interval.isWithin(left) else left == interval

  /**
   * Determine whether a numeric object is outside the passed <code>Interval</code>, returning a <code>Boolean</code>.
   *
   * @param interval the <code>Interval</code> against which to compare the value passed to the constructor as <code>left</code> 
   * @return true if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Interval</code> passed to this method.
   */
  def !==(interval: Interval[L]): Boolean = if (interval != null) !interval.isWithin(left) else left != interval

  /**
   * Compare two objects for equality, returning a <code>Boolean</code>, using the <code>EqualityCandidate</code> instance passed as <code>candidate</code>.
   *
   * <p>
   * This method is used to enable syntax such as:
   * </p>
   *
   * <pre class="stHighlight">
   * assert(result === (17 decidedBy defaultEquality[Int]))
   * <pre>
   *
   * @param candidate the <code>EqualityCandidate</code> containing the object to compare for equality with <code>left</code> as well as the <code>Equality</code>
   *     to consult
   * @return true if the <code>left</code> object is equal according to the passed <code>EqualityCandidate</code>
   */
  def ===[R](candidate: EqualityCandidate[L, R]): Boolean = candidate.isEqual(left)

  /**
   * Compare two objects for inequality, returning a <code>Boolean</code>, using the <code>EqualityCandidate</code> instance passed as <code>candidate</code>.
   *
   * @param candidate the <code>EqualityCandidate</code> containing the object to compare for inequality with <code>left</code> as well as the <code>Equality</code>
   *     to consult
   * @return true if the <code>left</code> object is unequal according to the passed <code>EqualityCandidate</code>
   */
  def !==[R](candidate: EqualityCandidate[L, R]): Boolean = !candidate.isEqual(left)

   // TODO: Hey, no null here. Do we get an ambiguous implicit bug?
}

