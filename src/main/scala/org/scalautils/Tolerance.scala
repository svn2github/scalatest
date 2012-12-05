/*
 * Copyright 2001-2012 Artima, Inc.
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
 * This class is part of the ScalaUtils tolerance DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final case class Interval[T : Numeric](right: T, tolerance: T) {
  private val numeric = implicitly[Numeric[T]]
  private val max = numeric.plus(right, tolerance)
  private val min = numeric.minus(right, tolerance)
  def isWithin(n: T): Boolean = {
    numeric.gteq(n, min) && numeric.lteq(n, max)
  }
  def ===(fartherRight: T): Boolean = isWithin(fartherRight)
  def !==(fartherRight: T): Boolean = !isWithin(fartherRight)
}

trait Tolerance {

  /**
   * This class is part of the ScalaUtils tolerance DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class PlusOrMinusWrapper[T: Numeric](right: T) {

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
      Interval(right, tolerance)
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
      Interval(right, tolerance)
    }
  }

  /**
   * Implicitly converts an object of type <code>Byte</code> to a <code>BytePlusOrMinusWrapper</code>,
   * to enable a <code>+-</code> method to be invokable on that object.
   */
  implicit def convertNumericToPlusOrMinusWrapper[T : Numeric](right: T): PlusOrMinusWrapper[T] = new PlusOrMinusWrapper(right)
}
