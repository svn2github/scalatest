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
 * <code>===</code> with a <code>Boolean</code> result and no enforced type constraint between
 * two object types. For example:
 *
 * <pre class="stHighlight">
 * assert(a === b)
 * </pre>
 *
 * @param left An object to convert to <code>Equalizer</code>, which represents the value
 *     on the left side of a <code>===</code> invocation.
 *
 * @author Bill Venners
 */
class Equalizer[L](left: L) {

  /**
   * Compare two objects for equality, returning a <code>Boolean</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
   *
   * @param right the object to compare for equality with <code>left</code>, passed to the constructor
   * @return true if the <code>left</code> and <code>right</code> objects are equal according to the passed <code>Equality</code> type class.
   */
  def ===(right: Any)(implicit equality: Equality[L]): Boolean = equality.areEqual(left, right)

  /**
   * Compare two objects for inequality, returning a <code>Boolean</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
   *
   * @param right the object to compare for inequality with <code>left</code>, passed to the constructor
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
}

