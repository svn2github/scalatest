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
 * Class representing an interval (<em>i.e.</em>, range) between two numbers.
 * 
 * <p>
 * The interval is expressed in terms of a <code>Numeric</code> <em>pivot</em> and <em>tolerance</em>.
 * The interval extends from <code>pivot - tolerance</code> to <code>pivot + tolerance</code>, inclusive.
 * </p>
 * 
 * @param pivot the pivot number at the center of the interval
 * @param tolerance the tolerance that determines the high and low point of the interval
 * 
 * @author Bill Venners
 */
final case class Interval[T : Numeric](pivot: T, tolerance: T) {

  private val numeric = implicitly[Numeric[T]]

  require(numeric.signum(tolerance) >= 0, "tolerance must be zero or greater, but was " + tolerance)

  private val max = numeric.plus(pivot, tolerance)
  private val min = numeric.minus(pivot, tolerance)

  /**
   * Determines whether the passed <code>Numeric</code> value <code>n</code> is within the interval represented
   * by this <code>Interval</code> instance.
   */
  def isWithin(n: T): Boolean = {
    numeric.gteq(n, min) && numeric.lteq(n, max)
  }

  /**
   * Returns <code>true</code> if the passed number, <code>n</code>, is within the interval represented by this <code>Interval</code> instance
   *
   * <p>
   * The purpose of this method, which will likely be used only rarely, is to achieve symmetry around the <code>===</code> operator. The
   * <code>TripleEquals</code> trait (and its type-checking siblings <code>TypeCheckedTripleEquals</code> and <code>ConversionCheckedTripleEquals</code>) enable you to write:
   * </p>
   *
   * <pre>
   * a === (1.0 +- 0.1)
   * </pre>
   *
   * <p>
   * This method ensures the following mirrored form means the same thing:
   * </p>
   *
   * <pre>
   * (1.0 +- 0.1) === a
   * </pre>
   *
   * @param n a number that may or may not lie within this interval
   */
  def ===(n: T): Boolean = isWithin(n)

  /**
   * Returns <code>false</code> if the passed number, <code>n</code>, is within the interval represented by this <code>Interval</code> instance
   *
   * <p>
   * The purpose of this method, which will likely be used only rarely, is to achieve symmetry around the <code>!==</code> operator. The
   * <code>TripleEquals</code> trait (and its type-checking siblings <code>TypeCheckedTripleEquals</code> and <code>ConversionCheckedTripleEquals</code>) enable you to write:
   * </p>
   *
   * <pre>
   * a !== (1.0 +- 0.1)
   * </pre>
   *
   * <p>
   * This method ensures the following mirrored form means the same thing:
   * </p>
   *
   * <pre>
   * (1.0 +- 0.1) !== a
   * </pre>
   *
   * @param n a number that may or may not lie within this interval
   */
  def !==(n: T): Boolean = !isWithin(n)
}

trait Tolerance {

  /**
   * This class is part of the ScalaUtils tolerance DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class PlusOrMinusWrapper[T: Numeric](pivot: T) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (17.0 +- 0.2)
     *                            ^
     * </pre>
     */
    def +-(tolerance: T): Interval[T] = {
      val numeric = implicitly[Numeric[T]]
      if (numeric.lteq(tolerance, numeric.zero))
        throw new IllegalArgumentException(tolerance.toString + " passed to +- was zero or negative. Must be a positive non-zero number.")
        // throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      Interval(pivot, tolerance)
    }

    /**
     * <strong>The plusOrMinus method has been deprecated and will be removed in a future version of ScalaTest. Please use +- instead.</strong>
     *
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (17.0 plusOrMinus 0.2)
     *                            ^
     * </pre>
     */
    @deprecated("The plusOrMinus method has been deprecated and will be removed in a future version of ScalaTest. Please use +- instead.")
    def plusOrMinus(tolerance: T): Interval[T] = {
      val numeric = implicitly[Numeric[T]]
      if (numeric.lteq(tolerance, numeric.zero))
        throw new IllegalArgumentException(tolerance.toString + " passed to +- was zero or negative. Must be a positive non-zero number.")
        // throw newTestFailedException(Resources("negativeOrZeroRange", tolerance.toString))
      Interval(pivot, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Byte</code> to a <code>BytePlusOrMinusWrapper</code>,
   * to enable a <code>+-</code> method to be invokable on that object.
   */
  implicit def convertNumericToPlusOrMinusWrapper[T : Numeric](pivot: T): PlusOrMinusWrapper[T] = new PlusOrMinusWrapper(pivot)
}

object Tolerance extends Tolerance

