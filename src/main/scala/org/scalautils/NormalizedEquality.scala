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
 * A default <code>Equality</code> type class implementation (which can be used for any type) whose
 * <code>areEqual</code> method compares the passed objects with <code>==</code>, calling <code>.deep</code>
 * first on any passed object that is an array.
 * </p>
 */
trait NormalizedEquality[A] extends Equality[A] {

// TODO: May want to still do the thing on arrays, else can't normalize arrays.
  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by invoking <code>==</code> on <code>a</code>
   * passing in <code>b</code>, treating arrays specially by invoking <code>.deep</code> on <code>a</code> and/or </code>b</code> if they
   * are arrays, and using the result or results of invoking <code>.deep</code> in the equality check.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   */
  final def areEqual(a: A, b: Any): Boolean = {
    val nb = if (isInstanceOfA(b)) normalized(b.asInstanceOf[A]) else b
    normalized(a) == nb
  }
  def isInstanceOfA(b: Any): Boolean
  def normalized(a: A): A
/*
  def apply(b: U): PartiallyAppliedEquality[T, U] = // This can be on Equality
    new PartiallyAppliedEquality[T, U] { // The U is for EqualityConstraints only, when used with ===
      def isEqual(a: T): Boolean = areEqual(a, b)
    }
  def apply(ne: NormalizedEquality[A]): NormalizedEquality[A] =
    new NormalizedEquality[A] { inner =>
      def isInstanceOfT(b: Any) = outer.isInstanceOfT(b)
      def normalized(a: A): A = ne.normalized(outer.normalize(a))
    }
*/
}

