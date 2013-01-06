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
package org.scalatest.matchers

import org.scalatest._
import org.scalautils._

class ShouldEqualToleranceSpec extends Spec with ShouldMatchers with Tolerance {

  val sevenDotOh = 7.0
  val minusSevenDotOh = -7.0
  val sevenDotOhFloat = 7.0f
  val minusSevenDotOhFloat = -7.0f
  val sevenLong = 7L
  val minusSevenLong = -7L
  val sevenInt = 7
  val minusSevenInt = -7
  val sevenShort: Short = 7
  val minusSevenShort: Short = -7
  val sevenByte: Byte = 7
  val minusSevenByte: Byte = -7

  /*
    I decided that for X +- Y, Y can be any numeric type that's implicitly
    convertible to X. So if X is Double, Y could be Double, Float, Long, Int, Short, Byte.
    If X is Long, Y could be Long, Int, Short, Byte. If X is Short, Y could be Short or Byte.
    And if X is Byte, Y must be Byte.
    assert(minusSevenDotOhFloat === (-6.8f +- 0.2d))
  */
  object `The should equal syntax` {

    def `should be true if the number is within the given interval` {

      // Double +- Double
      sevenDotOh should equal (7.1 +- 0.2)
      sevenDotOh should equal (6.9 +- 0.2)
      sevenDotOh should equal (7.0 +- 0.2)
      sevenDotOh should equal (7.2 +- 0.2)
      sevenDotOh should equal (6.8 +- 0.2)
      minusSevenDotOh should equal (-7.1 +- 0.2)
      minusSevenDotOh should equal (-6.9 +- 0.2)
      minusSevenDotOh should equal (-7.0 +- 0.2)
      minusSevenDotOh should equal (-7.2 +- 0.2)
      minusSevenDotOh should equal (-6.8 +- 0.2)

      // Double +- Float
      sevenDotOh should equal (7.1 +- 0.2f)
      sevenDotOh should equal (6.9 +- 0.2f)
      sevenDotOh should equal (7.0 +- 0.2f)
      sevenDotOh should equal (7.2 +- 0.2f)
      sevenDotOh should equal (6.8 +- 0.2f)
      minusSevenDotOh should equal (-7.1 +- 0.2f)
      minusSevenDotOh should equal (-6.9 +- 0.2f)
      minusSevenDotOh should equal (-7.0 +- 0.2f)
      minusSevenDotOh should equal (-7.2 +- 0.2f)
      minusSevenDotOh should equal (-6.8 +- 0.2f)

      // Double +- Long
      sevenDotOh should equal (7.1 +- 2L)
      sevenDotOh should equal (6.9 +- 2L)
      sevenDotOh should equal (7.0 +- 2L)
      sevenDotOh should equal (7.2 +- 2L)
      sevenDotOh should equal (6.8 +- 2L)
      minusSevenDotOh should equal (-7.1 +- 2L)
      minusSevenDotOh should equal (-6.9 +- 2L)
      minusSevenDotOh should equal (-7.0 +- 2L)
      minusSevenDotOh should equal (-7.2 +- 2L)
      minusSevenDotOh should equal (-6.8 +- 2L)

      // Double +- Int
      sevenDotOh should equal (7.1 +- 2)
      sevenDotOh should equal (6.9 +- 2)
      sevenDotOh should equal (7.0 +- 2)
      sevenDotOh should equal (7.2 +- 2)
      sevenDotOh should equal (6.8 +- 2)
      minusSevenDotOh should equal (-7.1 +- 2)
      minusSevenDotOh should equal (-6.9 +- 2)
      minusSevenDotOh should equal (-7.0 +- 2)
      minusSevenDotOh should equal (-7.2 +- 2)
      minusSevenDotOh should equal (-6.8 +- 2)

      // Double +- Short
      sevenDotOh should equal (7.1 +- 2.toShort)
      sevenDotOh should equal (6.9 +- 2.toShort)
      sevenDotOh should equal (7.0 +- 2.toShort)
      sevenDotOh should equal (7.2 +- 2.toShort)
      sevenDotOh should equal (6.8 +- 2.toShort)
      minusSevenDotOh should equal (-7.1 +- 2.toShort)
      minusSevenDotOh should equal (-6.9 +- 2.toShort)
      minusSevenDotOh should equal (-7.0 +- 2.toShort)
      minusSevenDotOh should equal (-7.2 +- 2.toShort)
      minusSevenDotOh should equal (-6.8 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should equal (7.1 +- 2.toByte)
      sevenDotOh should equal (6.9 +- 2.toByte)
      sevenDotOh should equal (7.0 +- 2.toByte)
      sevenDotOh should equal (7.2 +- 2.toByte)
      sevenDotOh should equal (6.8 +- 2.toByte)
      minusSevenDotOh should equal (-7.1 +- 2.toByte)
      minusSevenDotOh should equal (-6.9 +- 2.toByte)
      minusSevenDotOh should equal (-7.0 +- 2.toByte)
      minusSevenDotOh should equal (-7.2 +- 2.toByte)
      minusSevenDotOh should equal (-6.8 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should equal (7.1f +- 0.2f)
      sevenDotOhFloat should equal (6.9f +- 0.2f)
      sevenDotOhFloat should equal (7.0f +- 0.2f)
      sevenDotOhFloat should equal (7.2f +- 0.2f)
      sevenDotOhFloat should equal (6.8f +- 0.2f)
      minusSevenDotOhFloat should equal (-7.1f +- 0.2f)
      minusSevenDotOhFloat should equal (-6.9f +- 0.2f)
      minusSevenDotOhFloat should equal (-7.0f +- 0.2f)
      minusSevenDotOhFloat should equal (-7.2f +- 0.2f)
      minusSevenDotOhFloat should equal (-6.8f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should equal (7.1f +- 2L)
      sevenDotOhFloat should equal (6.9f +- 2L)
      sevenDotOhFloat should equal (7.0f +- 2L)
      sevenDotOhFloat should equal (7.2f +- 2L)
      sevenDotOhFloat should equal (6.8f +- 2L)
      minusSevenDotOhFloat should equal (-7.1f +- 2L)
      minusSevenDotOhFloat should equal (-6.9f +- 2L)
      minusSevenDotOhFloat should equal (-7.0f +- 2L)
      minusSevenDotOhFloat should equal (-7.2f +- 2L)
      minusSevenDotOhFloat should equal (-6.8f +- 2L)

      // Float +- Int
      sevenDotOhFloat should equal (7.1f +- 2)
      sevenDotOhFloat should equal (6.9f +- 2)
      sevenDotOhFloat should equal (7.0f +- 2)
      sevenDotOhFloat should equal (7.2f +- 2)
      sevenDotOhFloat should equal (6.8f +- 2)
      minusSevenDotOhFloat should equal (-7.1f +- 2)
      minusSevenDotOhFloat should equal (-6.9f +- 2)
      minusSevenDotOhFloat should equal (-7.0f +- 2)
      minusSevenDotOhFloat should equal (-7.2f +- 2)
      minusSevenDotOhFloat should equal (-6.8f +- 2)

      // Float +- Short
      sevenDotOhFloat should equal (7.1f +- 2.toShort)
      sevenDotOhFloat should equal (6.9f +- 2.toShort)
      sevenDotOhFloat should equal (7.0f +- 2.toShort)
      sevenDotOhFloat should equal (7.2f +- 2.toShort)
      sevenDotOhFloat should equal (6.8f +- 2.toShort)
      minusSevenDotOhFloat should equal (-7.1f +- 2.toShort)
      minusSevenDotOhFloat should equal (-6.9f +- 2.toShort)
      minusSevenDotOhFloat should equal (-7.0f +- 2.toShort)
      minusSevenDotOhFloat should equal (-7.2f +- 2.toShort)
      minusSevenDotOhFloat should equal (-6.8f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should equal (7.1f +- 2.toByte)
      sevenDotOhFloat should equal (6.9f +- 2.toByte)
      sevenDotOhFloat should equal (7.0f +- 2.toByte)
      sevenDotOhFloat should equal (7.2f +- 2.toByte)
      sevenDotOhFloat should equal (6.8f +- 2.toByte)
      minusSevenDotOhFloat should equal (-7.1f +- 2.toByte)
      minusSevenDotOhFloat should equal (-6.9f +- 2.toByte)
      minusSevenDotOhFloat should equal (-7.0f +- 2.toByte)
      minusSevenDotOhFloat should equal (-7.2f +- 2.toByte)
      minusSevenDotOhFloat should equal (-6.8f +- 2.toByte)

      // Long +- Long
      sevenLong should equal (9L +- 2L)
      sevenLong should equal (8L +- 2L)
      sevenLong should equal (7L +- 2L)
      sevenLong should equal (6L +- 2L)
      sevenLong should equal (5L +- 2L)
      minusSevenLong should equal (-9L +- 2L)
      minusSevenLong should equal (-8L +- 2L)
      minusSevenLong should equal (-7L +- 2L)
      minusSevenLong should equal (-6L +- 2L)
      minusSevenLong should equal (-5L +- 2L)

      // Long +- Int
      sevenLong should equal (9L +- 2)
      sevenLong should equal (8L +- 2)
      sevenLong should equal (7L +- 2)
      sevenLong should equal (6L +- 2)
      sevenLong should equal (5L +- 2)
      minusSevenLong should equal (-9L +- 2)
      minusSevenLong should equal (-8L +- 2)
      minusSevenLong should equal (-7L +- 2)
      minusSevenLong should equal (-6L +- 2)
      minusSevenLong should equal (-5L +- 2)

      // Long +- Short
      sevenLong should equal (9L +- 2.toShort)
      sevenLong should equal (8L +- 2.toShort)
      sevenLong should equal (7L +- 2.toShort)
      sevenLong should equal (6L +- 2.toShort)
      sevenLong should equal (5L +- 2.toShort)
      minusSevenLong should equal (-9L +- 2.toShort)
      minusSevenLong should equal (-8L +- 2.toShort)
      minusSevenLong should equal (-7L +- 2.toShort)
      minusSevenLong should equal (-6L +- 2.toShort)
      minusSevenLong should equal (-5L +- 2.toShort)

      // Long +- Byte
      sevenLong should equal (9L +- 2.toByte)
      sevenLong should equal (8L +- 2.toByte)
      sevenLong should equal (7L +- 2.toByte)
      sevenLong should equal (6L +- 2.toByte)
      sevenLong should equal (5L +- 2.toByte)
      minusSevenLong should equal (-9L +- 2.toByte)
      minusSevenLong should equal (-8L +- 2.toByte)
      minusSevenLong should equal (-7L +- 2.toByte)
      minusSevenLong should equal (-6L +- 2.toByte)
      minusSevenLong should equal (-5L +- 2.toByte)

      // Int +- Int
      sevenInt should equal (9 +- 2)
      sevenInt should equal (8 +- 2)
      sevenInt should equal (7 +- 2)
      sevenInt should equal (6 +- 2)
      sevenInt should equal (5 +- 2)
      minusSevenInt should equal (-9 +- 2)
      minusSevenInt should equal (-8 +- 2)
      minusSevenInt should equal (-7 +- 2)
      minusSevenInt should equal (-6 +- 2)
      minusSevenInt should equal (-5 +- 2)

      // Int +- Short
      sevenInt should equal (9 +- 2.toShort)
      sevenInt should equal (8 +- 2.toShort)
      sevenInt should equal (7 +- 2.toShort)
      sevenInt should equal (6 +- 2.toShort)
      sevenInt should equal (5 +- 2.toShort)
      minusSevenInt should equal (-9 +- 2.toShort)
      minusSevenInt should equal (-8 +- 2.toShort)
      minusSevenInt should equal (-7 +- 2.toShort)
      minusSevenInt should equal (-6 +- 2.toShort)
      minusSevenInt should equal (-5 +- 2.toShort)

      // Int +- Byte
      sevenInt should equal (9 +- 2.toByte)
      sevenInt should equal (8 +- 2.toByte)
      sevenInt should equal (7 +- 2.toByte)
      sevenInt should equal (6 +- 2.toByte)
      sevenInt should equal (5 +- 2.toByte)
      minusSevenInt should equal (-9 +- 2.toByte)
      minusSevenInt should equal (-8 +- 2.toByte)
      minusSevenInt should equal (-7 +- 2.toByte)
      minusSevenInt should equal (-6 +- 2.toByte)
      minusSevenInt should equal (-5 +- 2.toByte)

      // Short +- Short
      sevenShort should equal (9.toShort +- 2.toShort)
      sevenShort should equal (8.toShort +- 2.toShort)
      sevenShort should equal (7.toShort +- 2.toShort)
      sevenShort should equal (6.toShort +- 2.toShort)
      sevenShort should equal (5.toShort +- 2.toShort)
      minusSevenShort should equal ((-9).toShort +- 2.toShort)
      minusSevenShort should equal ((-8).toShort +- 2.toShort)
      minusSevenShort should equal ((-7).toShort +- 2.toShort)
      minusSevenShort should equal ((-6).toShort +- 2.toShort)
      minusSevenShort should equal ((-5).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should equal (9.toShort +- 2.toByte)
      sevenShort should equal (8.toShort +- 2.toByte)
      sevenShort should equal (7.toShort +- 2.toByte)
      sevenShort should equal (6.toShort +- 2.toByte)
      sevenShort should equal (5.toShort +- 2.toByte)
      minusSevenShort should equal ((-9).toShort +- 2.toByte)
      minusSevenShort should equal ((-8).toShort +- 2.toByte)
      minusSevenShort should equal ((-7).toShort +- 2.toByte)
      minusSevenShort should equal ((-6).toShort +- 2.toByte)
      minusSevenShort should equal ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should equal (9.toByte +- 2.toByte)
      sevenByte should equal (8.toByte +- 2.toByte)
      sevenByte should equal (7.toByte +- 2.toByte)
      sevenByte should equal (6.toByte +- 2.toByte)
      sevenByte should equal (5.toByte +- 2.toByte)
      minusSevenByte should equal ((-9).toByte +- 2.toByte)
      minusSevenByte should equal ((-8).toByte +- 2.toByte)
      minusSevenByte should equal ((-7).toByte +- 2.toByte)
      minusSevenByte should equal ((-6).toByte +- 2.toByte)
      minusSevenByte should equal ((-5).toByte +- 2.toByte)
    }

/*
    Decided against symmetry
    def `should, for symmetry, be true if the number is within the given interval when the interval is placed on the left hand side` {

      // Double +- Double
      (7.1 +- 0.2) should equal (sevenDotOh)
      (6.9 +- 0.2) should equal (sevenDotOh)
      (7.0 +- 0.2) should equal (sevenDotOh)
      (7.2 +- 0.2) should equal (sevenDotOh)
      (6.8 +- 0.2) should equal (sevenDotOh)
      (-7.1 +- 0.2) should equal (minusSevenDotOh)
      (-6.9 +- 0.2) should equal (minusSevenDotOh)
      (-7.0 +- 0.2) should equal (minusSevenDotOh)
      (-7.2 +- 0.2) should equal (minusSevenDotOh)
      (-6.8 +- 0.2) should equal (minusSevenDotOh)

      // Double +- Float
      (7.1 +- 0.2f) should equal (sevenDotOh)
      (6.9 +- 0.2f) should equal (sevenDotOh)
      (7.0 +- 0.2f) should equal (sevenDotOh)
      (7.2 +- 0.2f) should equal (sevenDotOh)
      (6.8 +- 0.2f) should equal (sevenDotOh)
      (-7.1 +- 0.2f) should equal (minusSevenDotOh)
      (-6.9 +- 0.2f) should equal (minusSevenDotOh)
      (-7.0 +- 0.2f) should equal (minusSevenDotOh)
      (-7.2 +- 0.2f) should equal (minusSevenDotOh)
      (-6.8 +- 0.2f) should equal (minusSevenDotOh)

      // Double +- Long
      (7.1 +- 2L) should equal (sevenDotOh)
      (6.9 +- 2L) should equal (sevenDotOh)
      (7.0 +- 2L) should equal (sevenDotOh)
      (7.2 +- 2L) should equal (sevenDotOh)
      (6.8 +- 2L) should equal (sevenDotOh)
      (-7.1 +- 2L) should equal (minusSevenDotOh)
      (-6.9 +- 2L) should equal (minusSevenDotOh)
      (-7.0 +- 2L) should equal (minusSevenDotOh)
      (-7.2 +- 2L) should equal (minusSevenDotOh)
      (-6.8 +- 2L) should equal (minusSevenDotOh)

      // Double +- Int
      (7.1 +- 2) should equal (sevenDotOh)
      (6.9 +- 2) should equal (sevenDotOh)
      (7.0 +- 2) should equal (sevenDotOh)
      (7.2 +- 2) should equal (sevenDotOh)
      (6.8 +- 2) should equal (sevenDotOh)
      (-7.1 +- 2) should equal (minusSevenDotOh)
      (-6.9 +- 2) should equal (minusSevenDotOh)
      (-7.0 +- 2) should equal (minusSevenDotOh)
      (-7.2 +- 2) should equal (minusSevenDotOh)
      (-6.8 +- 2) should equal (minusSevenDotOh)

      // Double +- Short
      (7.1 +- 2.toShort) should equal (sevenDotOh)
      (6.9 +- 2.toShort) should equal (sevenDotOh)
      (7.0 +- 2.toShort) should equal (sevenDotOh)
      (7.2 +- 2.toShort) should equal (sevenDotOh)
      (6.8 +- 2.toShort) should equal (sevenDotOh)
      (-7.1 +- 2.toShort) should equal (minusSevenDotOh)
      (-6.9 +- 2.toShort) should equal (minusSevenDotOh)
      (-7.0 +- 2.toShort) should equal (minusSevenDotOh)
      (-7.2 +- 2.toShort) should equal (minusSevenDotOh)
      (-6.8 +- 2.toShort) should equal (minusSevenDotOh)

      // Double +- Byte
      (7.1 +- 2.toByte) should equal (sevenDotOh)
      (6.9 +- 2.toByte) should equal (sevenDotOh)
      (7.0 +- 2.toByte) should equal (sevenDotOh)
      (7.2 +- 2.toByte) should equal (sevenDotOh)
      (6.8 +- 2.toByte) should equal (sevenDotOh)
      (-7.1 +- 2.toByte) should equal (minusSevenDotOh)
      (-6.9 +- 2.toByte) should equal (minusSevenDotOh)
      (-7.0 +- 2.toByte) should equal (minusSevenDotOh)
      (-7.2 +- 2.toByte) should equal (minusSevenDotOh)
      (-6.8 +- 2.toByte) should equal (minusSevenDotOh)

      // Float +- Float
      (7.1f +- 0.2f) should equal (sevenDotOhFloat)
      (6.9f +- 0.2f) should equal (sevenDotOhFloat)
      (7.0f +- 0.2f) should equal (sevenDotOhFloat)
      (7.2f +- 0.2f) should equal (sevenDotOhFloat)
      (6.8f +- 0.2f) should equal (sevenDotOhFloat)
      (-7.1f +- 0.2f) should equal (minusSevenDotOhFloat)
      (-6.9f +- 0.2f) should equal (minusSevenDotOhFloat)
      (-7.0f +- 0.2f) should equal (minusSevenDotOhFloat)
      (-7.2f +- 0.2f) should equal (minusSevenDotOhFloat)
      (-6.8f +- 0.2f) should equal (minusSevenDotOhFloat)

      // Float +- Long
      (7.1f +- 2L) should equal (sevenDotOhFloat)
      (6.9f +- 2L) should equal (sevenDotOhFloat)
      (7.0f +- 2L) should equal (sevenDotOhFloat)
      (7.2f +- 2L) should equal (sevenDotOhFloat)
      (6.8f +- 2L) should equal (sevenDotOhFloat)
      (-7.1f +- 2L) should equal (minusSevenDotOhFloat)
      (-6.9f +- 2L) should equal (minusSevenDotOhFloat)
      (-7.0f +- 2L) should equal (minusSevenDotOhFloat)
      (-7.2f +- 2L) should equal (minusSevenDotOhFloat)
      (-6.8f +- 2L) should equal (minusSevenDotOhFloat)

      // Float +- Int
      (7.1f +- 2) should equal (sevenDotOhFloat)
      (6.9f +- 2) should equal (sevenDotOhFloat)
      (7.0f +- 2) should equal (sevenDotOhFloat)
      (7.2f +- 2) should equal (sevenDotOhFloat)
      (6.8f +- 2) should equal (sevenDotOhFloat)
      (-7.1f +- 2) should equal (minusSevenDotOhFloat)
      (-6.9f +- 2) should equal (minusSevenDotOhFloat)
      (-7.0f +- 2) should equal (minusSevenDotOhFloat)
      (-7.2f +- 2) should equal (minusSevenDotOhFloat)
      (-6.8f +- 2) should equal (minusSevenDotOhFloat)

      // Float +- Short
      (7.1f +- 2.toShort) should equal (sevenDotOhFloat)
      (6.9f +- 2.toShort) should equal (sevenDotOhFloat)
      (7.0f +- 2.toShort) should equal (sevenDotOhFloat)
      (7.2f +- 2.toShort) should equal (sevenDotOhFloat)
      (6.8f +- 2.toShort) should equal (sevenDotOhFloat)
      (-7.1f +- 2.toShort) should equal (minusSevenDotOhFloat)
      (-6.9f +- 2.toShort) should equal (minusSevenDotOhFloat)
      (-7.0f +- 2.toShort) should equal (minusSevenDotOhFloat)
      (-7.2f +- 2.toShort) should equal (minusSevenDotOhFloat)
      (-6.8f +- 2.toShort) should equal (minusSevenDotOhFloat)

      // Float +- Byte
      (7.1f +- 2.toByte) should equal (sevenDotOhFloat)
      (6.9f +- 2.toByte) should equal (sevenDotOhFloat)
      (7.0f +- 2.toByte) should equal (sevenDotOhFloat)
      (7.2f +- 2.toByte) should equal (sevenDotOhFloat)
      (6.8f +- 2.toByte) should equal (sevenDotOhFloat)
      (-7.1f +- 2.toByte) should equal (minusSevenDotOhFloat)
      (-6.9f +- 2.toByte) should equal (minusSevenDotOhFloat)
      (-7.0f +- 2.toByte) should equal (minusSevenDotOhFloat)
      (-7.2f +- 2.toByte) should equal (minusSevenDotOhFloat)
      (-6.8f +- 2.toByte) should equal (minusSevenDotOhFloat)

      // Long +- Long
      (9L +- 2L) should equal (sevenLong)
      (8L +- 2L) should equal (sevenLong)
      (7L +- 2L) should equal (sevenLong)
      (6L +- 2L) should equal (sevenLong)
      (5L +- 2L) should equal (sevenLong)
      (-9L +- 2L) should equal (minusSevenLong)
      (-8L +- 2L) should equal (minusSevenLong)
      (-7L +- 2L) should equal (minusSevenLong)
      (-6L +- 2L) should equal (minusSevenLong)
      (-5L +- 2L) should equal (minusSevenLong)

      // Long +- Int
      (9L +- 2) should equal (sevenLong)
      (8L +- 2) should equal (sevenLong)
      (7L +- 2) should equal (sevenLong)
      (6L +- 2) should equal (sevenLong)
      (5L +- 2) should equal (sevenLong)
      (-9L +- 2) should equal (minusSevenLong)
      (-8L +- 2) should equal (minusSevenLong)
      (-7L +- 2) should equal (minusSevenLong)
      (-6L +- 2) should equal (minusSevenLong)
      (-5L +- 2) should equal (minusSevenLong)

      // Long +- Short
      (9L +- 2.toShort) should equal (sevenLong)
      (8L +- 2.toShort) should equal (sevenLong)
      (7L +- 2.toShort) should equal (sevenLong)
      (6L +- 2.toShort) should equal (sevenLong)
      (5L +- 2.toShort) should equal (sevenLong)
      (-9L +- 2.toShort) should equal (minusSevenLong)
      (-8L +- 2.toShort) should equal (minusSevenLong)
      (-7L +- 2.toShort) should equal (minusSevenLong)
      (-6L +- 2.toShort) should equal (minusSevenLong)
      (-5L +- 2.toShort) should equal (minusSevenLong)

      // Long +- Byte
      (9L +- 2.toByte) should equal (sevenLong)
      (8L +- 2.toByte) should equal (sevenLong)
      (7L +- 2.toByte) should equal (sevenLong)
      (6L +- 2.toByte) should equal (sevenLong)
      (5L +- 2.toByte) should equal (sevenLong)
      (-9L +- 2.toByte) should equal (minusSevenLong)
      (-8L +- 2.toByte) should equal (minusSevenLong)
      (-7L +- 2.toByte) should equal (minusSevenLong)
      (-6L +- 2.toByte) should equal (minusSevenLong)
      (-5L +- 2.toByte) should equal (minusSevenLong)

      // Int +- Int
      (9 +- 2) should equal (sevenInt)
      (8 +- 2) should equal (sevenInt)
      (7 +- 2) should equal (sevenInt)
      (6 +- 2) should equal (sevenInt)
      (5 +- 2) should equal (sevenInt)
      (-9 +- 2) should equal (minusSevenInt)
      (-8 +- 2) should equal (minusSevenInt)
      (-7 +- 2) should equal (minusSevenInt)
      (-6 +- 2) should equal (minusSevenInt)
      (-5 +- 2) should equal (minusSevenInt)

      // Int +- Short
      (9 +- 2.toShort) should equal (sevenInt)
      (8 +- 2.toShort) should equal (sevenInt)
      (7 +- 2.toShort) should equal (sevenInt)
      (6 +- 2.toShort) should equal (sevenInt)
      (5 +- 2.toShort) should equal (sevenInt)
      (-9 +- 2.toShort) should equal (minusSevenInt)
      (-8 +- 2.toShort) should equal (minusSevenInt)
      (-7 +- 2.toShort) should equal (minusSevenInt)
      (-6 +- 2.toShort) should equal (minusSevenInt)
      (-5 +- 2.toShort) should equal (minusSevenInt)

      // Int +- Byte
      (9 +- 2.toByte) should equal (sevenInt)
      (8 +- 2.toByte) should equal (sevenInt)
      (7 +- 2.toByte) should equal (sevenInt)
      (6 +- 2.toByte) should equal (sevenInt)
      (5 +- 2.toByte) should equal (sevenInt)
      (-9 +- 2.toByte) should equal (minusSevenInt)
      (-8 +- 2.toByte) should equal (minusSevenInt)
      (-7 +- 2.toByte) should equal (minusSevenInt)
      (-6 +- 2.toByte) should equal (minusSevenInt)
      (-5 +- 2.toByte) should equal (minusSevenInt)

      // Short +- Short
      (9.toShort +- 2.toShort) should equal (sevenShort)
      (8.toShort +- 2.toShort) should equal (sevenShort)
      (7.toShort +- 2.toShort) should equal (sevenShort)
      (6.toShort +- 2.toShort) should equal (sevenShort)
      (5.toShort +- 2.toShort) should equal (sevenShort)
      ((-9).toShort +- 2.toShort) should equal (minusSevenShort)
      ((-8).toShort +- 2.toShort) should equal (minusSevenShort)
      ((-7).toShort +- 2.toShort) should equal (minusSevenShort)
      ((-6).toShort +- 2.toShort) should equal (minusSevenShort)
      ((-5).toShort +- 2.toShort) should equal (minusSevenShort)

      // Short +- Byte
      (9.toShort +- 2.toByte) should equal (sevenShort)
      (8.toShort +- 2.toByte) should equal (sevenShort)
      (7.toShort +- 2.toByte) should equal (sevenShort)
      (6.toShort +- 2.toByte) should equal (sevenShort)
      (5.toShort +- 2.toByte) should equal (sevenShort)
      ((-9).toShort +- 2.toByte) should equal (minusSevenShort)
      ((-8).toShort +- 2.toByte) should equal (minusSevenShort)
      ((-7).toShort +- 2.toByte) should equal (minusSevenShort)
      ((-6).toShort +- 2.toByte) should equal (minusSevenShort)
      ((-5).toShort +- 2.toByte) should equal (minusSevenShort)

      // Byte +- Byte
      (9.toByte +- 2.toByte) should equal (sevenByte)
      (8.toByte +- 2.toByte) should equal (sevenByte)
      (7.toByte +- 2.toByte) should equal (sevenByte)
      (6.toByte +- 2.toByte) should equal (sevenByte)
      (5.toByte +- 2.toByte) should equal (sevenByte)
      ((-9).toByte +- 2.toByte) should equal (minusSevenByte)
      ((-8).toByte +- 2.toByte) should equal (minusSevenByte)
      ((-7).toByte +- 2.toByte) should equal (minusSevenByte)
      ((-6).toByte +- 2.toByte) should equal (minusSevenByte)
      ((-5).toByte +- 2.toByte) should equal (minusSevenByte)
    }
*/

    def `should throw TFE if the number is outside the given interval` {

      // Double +- Double
      intercept[TestFailedException] { sevenDotOh should equal (7.5 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should equal (6.5 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-7.5 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-6.5 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { sevenDotOh should equal (7.5 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should equal (6.5 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-7.5 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-6.5 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { sevenDotOh should equal (4.0 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should equal (9.1 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { sevenDotOh should equal (4.0 +- 2) }
      intercept[TestFailedException] { sevenDotOh should equal (9.1 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { sevenDotOh should equal (4.0 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should equal (9.1 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { sevenDotOh should equal (4.0 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should equal (9.1 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-4.0 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should equal (-9.1 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { sevenDotOhFloat should equal (7.5f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should equal (6.5f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-7.5f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-6.5f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { sevenDotOhFloat should equal (4.0f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should equal (9.1f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-4.0f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should equal (-9.1f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { sevenLong should equal (4L +- 2L) }
      intercept[TestFailedException] { sevenLong should equal (10L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should equal (-4L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should equal (-10L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { sevenLong should equal (4L +- 2) }
      intercept[TestFailedException] { sevenLong should equal (10L +- 2) }
      intercept[TestFailedException] { minusSevenLong should equal (-4L +- 2) }
      intercept[TestFailedException] { minusSevenLong should equal (-10L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { sevenLong should equal (4L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should equal (10L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should equal (-4L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should equal (-10L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { sevenLong should equal (4L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should equal (10L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should equal (-4L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should equal (-10L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { sevenInt should equal (4 +- 2) }
      intercept[TestFailedException] { sevenInt should equal (10 +- 2) }
      intercept[TestFailedException] { minusSevenInt should equal (-4 +- 2) }
      intercept[TestFailedException] { minusSevenInt should equal (-10 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { sevenInt should equal (4 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should equal (10 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should equal (-4 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should equal (-10 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { sevenInt should equal (4 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should equal (10 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should equal (-4 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should equal (-10 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { sevenShort should equal (4.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should equal (10.toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should equal ((-4).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should equal ((-10).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { sevenShort should equal (4.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should equal (10.toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should equal ((-4).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should equal ((-10).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { sevenByte should equal (4.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should equal (10.toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should equal ((-4).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should equal ((-10).toByte +- 2.toByte) }
    }

/*
    Decided against this symmetry
    def `should, for symmetry, throw TFE if the number is outside the given interval, when the interval is on the left hand side` {

      // Double +- Double
      intercept[TestFailedException] { (7.5 +- 0.2) should equal (sevenDotOh) }
      intercept[TestFailedException] { (6.5 +- 0.2) should equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.5 +- 0.2) should equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.5 +- 0.2) should equal (minusSevenDotOh) }

      // Double +- Float
      intercept[TestFailedException] { (7.5 +- 0.2f) should equal (sevenDotOh) }
      intercept[TestFailedException] { (6.5 +- 0.2f) should equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.5 +- 0.2f) should equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.5 +- 0.2f) should equal (minusSevenDotOh) }

      // Double +- Long
      intercept[TestFailedException] { (4.0 +- 2L) should equal (sevenDotOh) }
      intercept[TestFailedException] { (9.1 +- 2L) should equal (sevenDotOh) }
      intercept[TestFailedException] { (-4.0 +- 2L) should equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-9.1 +- 2L) should equal (minusSevenDotOh) }

      // Double +- Int
      intercept[TestFailedException] { (4.0 +- 2) should equal (sevenDotOh) }
      intercept[TestFailedException] { (9.1 +- 2) should equal (sevenDotOh) }
      intercept[TestFailedException] { (-4.0 +- 2) should equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-9.1 +- 2) should equal (minusSevenDotOh) }

      // Double +- Short
      intercept[TestFailedException] { (4.0 +- 2.toShort) should equal (sevenDotOh) }
      intercept[TestFailedException] { (9.1 +- 2.toShort) should equal (sevenDotOh) }
      intercept[TestFailedException] { (-4.0 +- 2.toShort) should equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-9.1 +- 2.toShort) should equal (minusSevenDotOh) }

      // Double +- Byte
      intercept[TestFailedException] { (4.0 +- 2.toByte) should equal (sevenDotOh) }
      intercept[TestFailedException] { (9.1 +- 2.toByte) should equal (sevenDotOh) }
      intercept[TestFailedException] { (-4.0 +- 2.toByte) should equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-9.1 +- 2.toByte) should equal (minusSevenDotOh) }

      // Float +- Float
      intercept[TestFailedException] { (7.5f +- 0.2f) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.5f +- 0.2f) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-7.5f +- 0.2f) should equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.5f +- 0.2f) should equal (minusSevenDotOhFloat) }

      // Float +- Long
      intercept[TestFailedException] { (4.0f +- 2L) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (9.1f +- 2L) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-4.0f +- 2L) should equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-9.1f +- 2L) should equal (minusSevenDotOhFloat) }

      // Float +- Int
      intercept[TestFailedException] { (4.0f +- 2) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (9.1f +- 2) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-4.0f +- 2) should equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-9.1f +- 2) should equal (minusSevenDotOhFloat) }

      // Float +- Short
      intercept[TestFailedException] { (4.0f +- 2.toShort) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (9.1f +- 2.toShort) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-4.0f +- 2.toShort) should equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-9.1f +- 2.toShort) should equal (minusSevenDotOhFloat) }

      // Float +- Byte
      intercept[TestFailedException] { (4.0f +- 2.toByte) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (9.1f +- 2.toByte) should equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-4.0f +- 2.toByte) should equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-9.1f +- 2.toByte) should equal (minusSevenDotOhFloat) }

      // Long +- Long
      intercept[TestFailedException] { (4L +- 2L) should equal (sevenLong) }
      intercept[TestFailedException] { (10L +- 2L) should equal (sevenLong) }
      intercept[TestFailedException] { (-4L +- 2L) should equal (minusSevenLong) }
      intercept[TestFailedException] { (-10L +- 2L) should equal (minusSevenLong) }

      // Long +- Int
      intercept[TestFailedException] { (4L +- 2) should equal (sevenLong) }
      intercept[TestFailedException] { (10L +- 2) should equal (sevenLong) }
      intercept[TestFailedException] { (-4L +- 2) should equal (minusSevenLong) }
      intercept[TestFailedException] { (-10L +- 2) should equal (minusSevenLong) }

      // Long +- Short
      intercept[TestFailedException] { (4L +- 2.toShort) should equal (sevenLong) }
      intercept[TestFailedException] { (10L +- 2.toShort) should equal (sevenLong) }
      intercept[TestFailedException] { (-4L +- 2.toShort) should equal (minusSevenLong) }
      intercept[TestFailedException] { (-10L +- 2.toShort) should equal (minusSevenLong) }

      // Long +- Byte
      intercept[TestFailedException] { (4L +- 2.toByte) should equal (sevenLong) }
      intercept[TestFailedException] { (10L +- 2.toByte) should equal (sevenLong) }
      intercept[TestFailedException] { (-4L +- 2.toByte) should equal (minusSevenLong) }
      intercept[TestFailedException] { (-10L +- 2.toByte) should equal (minusSevenLong) }

      // Int +- Int
      intercept[TestFailedException] { (4 +- 2) should equal (sevenInt) }
      intercept[TestFailedException] { (10 +- 2) should equal (sevenInt) }
      intercept[TestFailedException] { (-4 +- 2) should equal (minusSevenInt) }
      intercept[TestFailedException] { (-10 +- 2) should equal (minusSevenInt) }

      // Int +- Short
      intercept[TestFailedException] { (4 +- 2.toShort) should equal (sevenInt) }
      intercept[TestFailedException] { (10 +- 2.toShort) should equal (sevenInt) }
      intercept[TestFailedException] { (-4 +- 2.toShort) should equal (minusSevenInt) }
      intercept[TestFailedException] { (-10 +- 2.toShort) should equal (minusSevenInt) }

      // Int +- Byte
      intercept[TestFailedException] { (4 +- 2.toByte) should equal (sevenInt) }
      intercept[TestFailedException] { (10 +- 2.toByte) should equal (sevenInt) }
      intercept[TestFailedException] { (-4 +- 2.toByte) should equal (minusSevenInt) }
      intercept[TestFailedException] { (-10 +- 2.toByte) should equal (minusSevenInt) }

      // Short +- Short
      intercept[TestFailedException] { (4.toShort +- 2.toShort) should equal (sevenShort) }
      intercept[TestFailedException] { (10.toShort +- 2.toShort) should equal (sevenShort) }
      intercept[TestFailedException] { ((-4).toShort +- 2.toShort) should equal (minusSevenShort) }
      intercept[TestFailedException] { ((-10).toShort +- 2.toShort) should equal (minusSevenShort) }

      // Short +- Byte
      intercept[TestFailedException] { (4.toShort +- 2.toByte) should equal (sevenShort) }
      intercept[TestFailedException] { (10.toShort +- 2.toByte) should equal (sevenShort) }
      intercept[TestFailedException] { ((-4).toShort +- 2.toByte) should equal (minusSevenShort) }
      intercept[TestFailedException] { ((-10).toShort +- 2.toByte) should equal (minusSevenShort) }

      // Byte +- Byte
      intercept[TestFailedException] { (4.toByte +- 2.toByte) should equal (sevenByte) }
      intercept[TestFailedException] { (10.toByte +- 2.toByte) should equal (sevenByte) }
      intercept[TestFailedException] { ((-4).toByte +- 2.toByte) should equal (minusSevenByte) }
      intercept[TestFailedException] { ((-10).toByte +- 2.toByte) should equal (minusSevenByte) }
    }
*/
  }

  object `The not equal syntax` {

    def `should succeed if the number is outside the given interval` {

      // Double +- Double
      sevenDotOh should not equal (7.5 +- 0.2)
      sevenDotOh should not equal (6.5 +- 0.2)
      minusSevenDotOh should not equal (-7.5 +- 0.2)
      minusSevenDotOh should not equal (-6.5 +- 0.2)

      // Double +- Float
      sevenDotOh should not equal (7.5 +- 0.2f)
      sevenDotOh should not equal (6.5 +- 0.2f)
      minusSevenDotOh should not equal (-7.5 +- 0.2f)
      minusSevenDotOh should not equal (-6.5 +- 0.2f)

      // Double +- Long
      sevenDotOh should not equal (4.0 +- 2L)
      sevenDotOh should not equal (9.1 +- 2L)
      minusSevenDotOh should not equal (-4.0 +- 2L)
      minusSevenDotOh should not equal (-9.1 +- 2L)

      // Double +- Int
      sevenDotOh should not equal (4.0 +- 2)
      sevenDotOh should not equal (9.1 +- 2)
      minusSevenDotOh should not equal (-4.0 +- 2)
      minusSevenDotOh should not equal (-9.1 +- 2)

      // Double +- Short
      sevenDotOh should not equal (4.0 +- 2.toShort)
      sevenDotOh should not equal (9.1 +- 2.toShort)
      minusSevenDotOh should not equal (-4.0 +- 2.toShort)
      minusSevenDotOh should not equal (-9.1 +- 2.toShort)

      // Double +- Byte
      sevenDotOh should not equal (4.0 +- 2.toByte)
      sevenDotOh should not equal (9.1 +- 2.toByte)
      minusSevenDotOh should not equal (-4.0 +- 2.toByte)
      minusSevenDotOh should not equal (-9.1 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat should not equal (7.5f +- 0.2f)
      sevenDotOhFloat should not equal (6.5f +- 0.2f)
      minusSevenDotOhFloat should not equal (-7.5f +- 0.2f)
      minusSevenDotOhFloat should not equal (-6.5f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat should not equal (4.0f +- 2L)
      sevenDotOhFloat should not equal (9.1f +- 2L)
      minusSevenDotOhFloat should not equal (-4.0f +- 2L)
      minusSevenDotOhFloat should not equal (-9.1f +- 2L)

      // Float +- Int
      sevenDotOhFloat should not equal (4.0f +- 2)
      sevenDotOhFloat should not equal (9.1f +- 2)
      minusSevenDotOhFloat should not equal (-4.0f +- 2)
      minusSevenDotOhFloat should not equal (-9.1f +- 2)

      // Float +- Short
      sevenDotOhFloat should not equal (4.0f +- 2.toShort)
      sevenDotOhFloat should not equal (9.1f +- 2.toShort)
      minusSevenDotOhFloat should not equal (-4.0f +- 2.toShort)
      minusSevenDotOhFloat should not equal (-9.1f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat should not equal (4.0f +- 2.toByte)
      sevenDotOhFloat should not equal (9.1f +- 2.toByte)
      minusSevenDotOhFloat should not equal (-4.0f +- 2.toByte)
      minusSevenDotOhFloat should not equal (-9.1f +- 2.toByte)

      // Long +- Long
      sevenLong should not equal (4L +- 2L)
      sevenLong should not equal (10L +- 2L)
      minusSevenLong should not equal (-4L +- 2L)
      minusSevenLong should not equal (-10L +- 2L)

      // Long +- Int
      sevenLong should not equal (4L +- 2)
      sevenLong should not equal (10L +- 2)
      minusSevenLong should not equal (-4L +- 2)
      minusSevenLong should not equal (-10L +- 2)

      // Long +- Short
      sevenLong should not equal (4L +- 2.toShort)
      sevenLong should not equal (10L +- 2.toShort)
      minusSevenLong should not equal (-4L +- 2.toShort)
      minusSevenLong should not equal (-10L +- 2.toShort)

      // Long +- Byte
      sevenLong should not equal (4L +- 2.toByte)
      sevenLong should not equal (10L +- 2.toByte)
      minusSevenLong should not equal (-4L +- 2.toByte)
      minusSevenLong should not equal (-10L +- 2.toByte)

      // Int +- Int
      sevenInt should not equal (4 +- 2)
      sevenInt should not equal (10 +- 2)
      minusSevenInt should not equal (-4 +- 2)
      minusSevenInt should not equal (-10 +- 2)

      // Int +- Short
      sevenInt should not equal (4 +- 2.toShort)
      sevenInt should not equal (10 +- 2.toShort)
      minusSevenInt should not equal (-4 +- 2.toShort)
      minusSevenInt should not equal (-10 +- 2.toShort)

      // Int +- Byte
      sevenInt should not equal (4 +- 2.toByte)
      sevenInt should not equal (10 +- 2.toByte)
      minusSevenInt should not equal (-4 +- 2.toByte)
      minusSevenInt should not equal (-10 +- 2.toByte)

      // Short +- Short
      sevenShort should not equal (4.toShort +- 2.toShort)
      sevenShort should not equal (10.toShort +- 2.toShort)
      minusSevenShort should not equal ((-4).toShort +- 2.toShort)
      minusSevenShort should not equal ((-10).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort should not equal (4.toShort +- 2.toByte)
      sevenShort should not equal (10.toShort +- 2.toByte)
      minusSevenShort should not equal ((-4).toShort +- 2.toByte)
      minusSevenShort should not equal ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte should not equal (4.toByte +- 2.toByte)
      sevenByte should not equal (10.toByte +- 2.toByte)
      minusSevenByte should not equal ((-4).toByte +- 2.toByte)
      minusSevenByte should not equal ((-10).toByte +- 2.toByte)
    }

/* Chose not to do the symmetry thing, because implementing it would require an implicit that no one would need.
    def `should, for symmetry, succeed if the number is outside the given interval when the interval is placed on the left hand side` {

      // Double +- Double
      (7.5 +- 0.2) should not equal sevenDotOh
      (6.5 +- 0.2) should not equal sevenDotOh
      (-7.5 +- 0.2) should not equal minusSevenDotOh
      (-6.5 +- 0.2) should not equal minusSevenDotOh

      // Double +- Float
      (7.5 +- 0.2f) should not equal sevenDotOh
      (6.5 +- 0.2f) should not equal sevenDotOh
      (-7.5 +- 0.2f) should not equal minusSevenDotOh
      (-6.5 +- 0.2f) should not equal minusSevenDotOh

      // Double +- Long
      (4.0 +- 2L) should not equal sevenDotOh
      (9.1 +- 2L) should not equal sevenDotOh
      (-4.0 +- 2L) should not equal minusSevenDotOh
      (-9.1 +- 2L) should not equal minusSevenDotOh

      // Double +- Int
      (4.0 +- 2) should not equal sevenDotOh
      (9.1 +- 2) should not equal sevenDotOh
      (-4.0 +- 2) should not equal minusSevenDotOh
      (-9.1 +- 2) should not equal minusSevenDotOh

      // Double +- Short
      (4.0 +- 2.toShort) should not equal sevenDotOh
      (9.1 +- 2.toShort) should not equal sevenDotOh
      (-4.0 +- 2.toShort) should not equal minusSevenDotOh
      (-9.1 +- 2.toShort) should not equal minusSevenDotOh

      // Double +- Byte
      (4.0 +- 2.toByte) should not equal sevenDotOh
      (9.1 +- 2.toByte) should not equal sevenDotOh
      (-4.0 +- 2.toByte) should not equal minusSevenDotOh
      (-9.1 +- 2.toByte) should not equal minusSevenDotOh

      // Float +- Float
      (7.5f +- 0.2f) should not equal sevenDotOhFloat
      (6.5f +- 0.2f) should not equal sevenDotOhFloat
      (-7.5f +- 0.2f) should not equal minusSevenDotOhFloat
      (-6.5f +- 0.2f) should not equal minusSevenDotOhFloat

      // Float +- Long
      (4.0f +- 2L) should not equal sevenDotOhFloat
      (9.1f +- 2L) should not equal sevenDotOhFloat
      (-4.0f +- 2L) should not equal minusSevenDotOhFloat
      (-9.1f +- 2L) should not equal minusSevenDotOhFloat

      // Float +- Int
      (4.0f +- 2) should not equal sevenDotOhFloat
      (9.1f +- 2) should not equal sevenDotOhFloat
      (-4.0f +- 2) should not equal minusSevenDotOhFloat
      (-9.1f +- 2) should not equal minusSevenDotOhFloat

      // Float +- Short
      (4.0f +- 2.toShort) should not equal sevenDotOhFloat
      (9.1f +- 2.toShort) should not equal sevenDotOhFloat
      (-4.0f +- 2.toShort) should not equal minusSevenDotOhFloat
      (-9.1f +- 2.toShort) should not equal minusSevenDotOhFloat

      // Float +- Byte
      (4.0f +- 2.toByte) should not equal sevenDotOhFloat
      (9.1f +- 2.toByte) should not equal sevenDotOhFloat
      (-4.0f +- 2.toByte) should not equal minusSevenDotOhFloat
      (-9.1f +- 2.toByte) should not equal minusSevenDotOhFloat

      // Long +- Long
      (4L +- 2L) should not equal sevenLong
      (10L +- 2L) should not equal sevenLong
      (-4L +- 2L) should not equal minusSevenLong
      (-10L +- 2L) should not equal minusSevenLong

      // Long +- Int
      (4L +- 2) should not equal sevenLong
      (10L +- 2) should not equal sevenLong
      (-4L +- 2) should not equal minusSevenLong
      (-10L +- 2) should not equal minusSevenLong

      // Long +- Short
      (4L +- 2.toShort) should not equal sevenLong
      (10L +- 2.toShort) should not equal sevenLong
      (-4L +- 2.toShort) should not equal minusSevenLong
      (-10L +- 2.toShort) should not equal minusSevenLong

      // Long +- Byte
      (4L +- 2.toByte) should not equal sevenLong
      (10L +- 2.toByte) should not equal sevenLong
      (-4L +- 2.toByte) should not equal minusSevenLong
      (-10L +- 2.toByte) should not equal minusSevenLong

      // Int +- Int
      (4 +- 2) should not equal sevenInt
      (10 +- 2) should not equal sevenInt
      (-4 +- 2) should not equal minusSevenInt
      (-10 +- 2) should not equal minusSevenInt

      // Int +- Short
      (4 +- 2.toShort) should not equal sevenInt
      (10 +- 2.toShort) should not equal sevenInt
      (-4 +- 2.toShort) should not equal minusSevenInt
      (-10 +- 2.toShort) should not equal minusSevenInt

      // Int +- Byte
      (4 +- 2.toByte) should not equal sevenInt
      (10 +- 2.toByte) should not equal sevenInt
      (-4 +- 2.toByte) should not equal minusSevenInt
      (-10 +- 2.toByte) should not equal minusSevenInt

      // Short +- Short
      (4.toShort +- 2.toShort) should not equal sevenShort
      (10.toShort +- 2.toShort) should not equal sevenShort
      ((-4).toShort +- 2.toShort) should not equal minusSevenShort
      ((-10).toShort +- 2.toShort) should not equal minusSevenShort

      // Short +- Byte
      (4.toShort +- 2.toByte) should not equal sevenShort
      (10.toShort +- 2.toByte) should not equal sevenShort
      ((-4).toShort +- 2.toByte) should not equal minusSevenShort
      ((-10).toShort +- 2.toByte) should not equal minusSevenShort

      // Byte +- Byte
      (4.toByte +- 2.toByte) should not equal sevenByte
      (10.toByte +- 2.toByte) should not equal sevenByte
      ((-4).toByte +- 2.toByte) should not equal minusSevenByte
      ((-10).toByte +- 2.toByte) should not equal minusSevenByte
    }
*/

    def `should throw TFE if the number is within the given interval` {

      // Double +- Double
      intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.9 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.0 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.2 +- 0.2) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.8 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 0.2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 0.2) }

      // Double +- Float
      intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.9 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.0 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.2 +- 0.2f) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.8 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 0.2f) }

      // Double +- Long
      intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.9 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.0 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.2 +- 2L) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.8 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2L) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2L) }

      // Double +- Int
      intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 2) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.9 +- 2) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.0 +- 2) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.2 +- 2) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.8 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2) }

      // Double +- Short
      intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.9 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.0 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.2 +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.8 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2.toShort) }

      // Double +- Byte
      intercept[TestFailedException] { sevenDotOh should not equal (7.1 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.9 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.0 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should not equal (7.2 +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOh should not equal (6.8 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.1 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.9 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.0 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-7.2 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOh should not equal (-6.8 +- 2.toByte) }

      // Float +- Float
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 0.2f) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 0.2f) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 0.2f) }

      // Float +- Long
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2L) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2L) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2L) }

      // Float +- Int
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2) }

      // Float +- Short
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2.toShort) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2.toShort) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2.toShort) }

      // Float +- Byte
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.1f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.9f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.0f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (7.2f +- 2.toByte) }
      intercept[TestFailedException] { sevenDotOhFloat should not equal (6.8f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.1f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.9f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.0f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-7.2f +- 2.toByte) }
      intercept[TestFailedException] { minusSevenDotOhFloat should not equal (-6.8f +- 2.toByte) }

      // Long +- Long
      intercept[TestFailedException] { sevenLong should not equal (9L +- 2L) }
      intercept[TestFailedException] { sevenLong should not equal (8L +- 2L) }
      intercept[TestFailedException] { sevenLong should not equal (7L +- 2L) }
      intercept[TestFailedException] { sevenLong should not equal (6L +- 2L) }
      intercept[TestFailedException] { sevenLong should not equal (5L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should not equal (-9L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should not equal (-8L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should not equal (-7L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should not equal (-6L +- 2L) }
      intercept[TestFailedException] { minusSevenLong should not equal (-5L +- 2L) }

      // Long +- Int
      intercept[TestFailedException] { sevenLong should not equal (9L +- 2) }
      intercept[TestFailedException] { sevenLong should not equal (8L +- 2) }
      intercept[TestFailedException] { sevenLong should not equal (7L +- 2) }
      intercept[TestFailedException] { sevenLong should not equal (6L +- 2) }
      intercept[TestFailedException] { sevenLong should not equal (5L +- 2) }
      intercept[TestFailedException] { minusSevenLong should not equal (-9L +- 2) }
      intercept[TestFailedException] { minusSevenLong should not equal (-8L +- 2) }
      intercept[TestFailedException] { minusSevenLong should not equal (-7L +- 2) }
      intercept[TestFailedException] { minusSevenLong should not equal (-6L +- 2) }
      intercept[TestFailedException] { minusSevenLong should not equal (-5L +- 2) }

      // Long +- Short
      intercept[TestFailedException] { sevenLong should not equal (9L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should not equal (8L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should not equal (7L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should not equal (6L +- 2.toShort) }
      intercept[TestFailedException] { sevenLong should not equal (5L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should not equal (-9L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should not equal (-8L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should not equal (-7L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should not equal (-6L +- 2.toShort) }
      intercept[TestFailedException] { minusSevenLong should not equal (-5L +- 2.toShort) }

      // Long +- Byte
      intercept[TestFailedException] { sevenLong should not equal (9L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should not equal (8L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should not equal (7L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should not equal (6L +- 2.toByte) }
      intercept[TestFailedException] { sevenLong should not equal (5L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should not equal (-9L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should not equal (-8L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should not equal (-7L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should not equal (-6L +- 2.toByte) }
      intercept[TestFailedException] { minusSevenLong should not equal (-5L +- 2.toByte) }

      // Int +- Int
      intercept[TestFailedException] { sevenInt should not equal (9 +- 2) }
      intercept[TestFailedException] { sevenInt should not equal (8 +- 2) }
      intercept[TestFailedException] { sevenInt should not equal (7 +- 2) }
      intercept[TestFailedException] { sevenInt should not equal (6 +- 2) }
      intercept[TestFailedException] { sevenInt should not equal (5 +- 2) }
      intercept[TestFailedException] { minusSevenInt should not equal (-9 +- 2) }
      intercept[TestFailedException] { minusSevenInt should not equal (-8 +- 2) }
      intercept[TestFailedException] { minusSevenInt should not equal (-7 +- 2) }
      intercept[TestFailedException] { minusSevenInt should not equal (-6 +- 2) }
      intercept[TestFailedException] { minusSevenInt should not equal (-5 +- 2) }

      // Int +- Short
      intercept[TestFailedException] { sevenInt should not equal (9 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should not equal (8 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should not equal (7 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should not equal (6 +- 2.toShort) }
      intercept[TestFailedException] { sevenInt should not equal (5 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should not equal (-9 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should not equal (-8 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should not equal (-7 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should not equal (-6 +- 2.toShort) }
      intercept[TestFailedException] { minusSevenInt should not equal (-5 +- 2.toShort) }

      // Int +- Byte
      intercept[TestFailedException] { sevenInt should not equal (9 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should not equal (8 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should not equal (7 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should not equal (6 +- 2.toByte) }
      intercept[TestFailedException] { sevenInt should not equal (5 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should not equal (-9 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should not equal (-8 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should not equal (-7 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should not equal (-6 +- 2.toByte) }
      intercept[TestFailedException] { minusSevenInt should not equal (-5 +- 2.toByte) }

      // Short +- Short
      intercept[TestFailedException] { sevenShort should not equal (9.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should not equal (8.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should not equal (7.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should not equal (6.toShort +- 2.toShort) }
      intercept[TestFailedException] { sevenShort should not equal (5.toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-9).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-8).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-7).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-6).toShort +- 2.toShort) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-5).toShort +- 2.toShort) }

      // Short +- Byte
      intercept[TestFailedException] { sevenShort should not equal (9.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should not equal (8.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should not equal (7.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should not equal (6.toShort +- 2.toByte) }
      intercept[TestFailedException] { sevenShort should not equal (5.toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-9).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-8).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-7).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-6).toShort +- 2.toByte) }
      intercept[TestFailedException] { minusSevenShort should not equal ((-5).toShort +- 2.toByte) }

      // Byte +- Byte
      intercept[TestFailedException] { sevenByte should not equal (9.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should not equal (8.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should not equal (7.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should not equal (6.toByte +- 2.toByte) }
      intercept[TestFailedException] { sevenByte should not equal (5.toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should not equal ((-9).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should not equal ((-8).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should not equal ((-7).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should not equal ((-6).toByte +- 2.toByte) }
      intercept[TestFailedException] { minusSevenByte should not equal ((-5).toByte +- 2.toByte) }
    }

/*
    Decided against this symmetry
    def `should, for symmetry, throw TFE if the number is within the given interval, when the interval is placed on the left hand side` {

      // Double +- Double
      intercept[TestFailedException] { (7.1 +- 0.2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.9 +- 0.2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.0 +- 0.2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.2 +- 0.2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.8 +- 0.2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.1 +- 0.2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.9 +- 0.2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.0 +- 0.2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.2 +- 0.2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.8 +- 0.2) should not equal (minusSevenDotOh) }

      // Double +- Float
      intercept[TestFailedException] { (7.1 +- 0.2f) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.9 +- 0.2f) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.0 +- 0.2f) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.2 +- 0.2f) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.8 +- 0.2f) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.1 +- 0.2f) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.9 +- 0.2f) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.0 +- 0.2f) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.2 +- 0.2f) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.8 +- 0.2f) should not equal (minusSevenDotOh) }

      // Double +- Long
      intercept[TestFailedException] { (7.1 +- 2L) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.9 +- 2L) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.0 +- 2L) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.2 +- 2L) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.8 +- 2L) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.1 +- 2L) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.9 +- 2L) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.0 +- 2L) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.2 +- 2L) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.8 +- 2L) should not equal (minusSevenDotOh) }

      // Double +- Int
      intercept[TestFailedException] { (7.1 +- 2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.9 +- 2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.0 +- 2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.2 +- 2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.8 +- 2) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.1 +- 2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.9 +- 2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.0 +- 2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.2 +- 2) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.8 +- 2) should not equal (minusSevenDotOh) }

      // Double +- Short
      intercept[TestFailedException] { (7.1 +- 2.toShort) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.9 +- 2.toShort) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.0 +- 2.toShort) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.2 +- 2.toShort) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.8 +- 2.toShort) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.1 +- 2.toShort) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.9 +- 2.toShort) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.0 +- 2.toShort) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.2 +- 2.toShort) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.8 +- 2.toShort) should not equal (minusSevenDotOh) }

      // Double +- Byte
      intercept[TestFailedException] { (7.1 +- 2.toByte) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.9 +- 2.toByte) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.0 +- 2.toByte) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (7.2 +- 2.toByte) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (6.8 +- 2.toByte) should not equal (sevenDotOh) }
      intercept[TestFailedException] { (-7.1 +- 2.toByte) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.9 +- 2.toByte) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.0 +- 2.toByte) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-7.2 +- 2.toByte) should not equal (minusSevenDotOh) }
      intercept[TestFailedException] { (-6.8 +- 2.toByte) should not equal (minusSevenDotOh) }

      // Float +- Float
      intercept[TestFailedException] { (7.1f +- 0.2f) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.9f +- 0.2f) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.0f +- 0.2f) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.2f +- 0.2f) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.8f +- 0.2f) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-7.1f +- 0.2f) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.9f +- 0.2f) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.0f +- 0.2f) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.2f +- 0.2f) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.8f +- 0.2f) should not equal (minusSevenDotOhFloat) }

      // Float +- Long
      intercept[TestFailedException] { (7.1f +- 2L) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.9f +- 2L) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.0f +- 2L) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.2f +- 2L) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.8f +- 2L) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-7.1f +- 2L) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.9f +- 2L) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.0f +- 2L) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.2f +- 2L) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.8f +- 2L) should not equal (minusSevenDotOhFloat) }

      // Float +- Int
      intercept[TestFailedException] { (7.1f +- 2) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.9f +- 2) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.0f +- 2) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.2f +- 2) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.8f +- 2) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-7.1f +- 2) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.9f +- 2) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.0f +- 2) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.2f +- 2) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.8f +- 2) should not equal (minusSevenDotOhFloat) }

      // Float +- Short
      intercept[TestFailedException] { (7.1f +- 2.toShort) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.9f +- 2.toShort) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.0f +- 2.toShort) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.2f +- 2.toShort) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.8f +- 2.toShort) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-7.1f +- 2.toShort) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.9f +- 2.toShort) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.0f +- 2.toShort) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.2f +- 2.toShort) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.8f +- 2.toShort) should not equal (minusSevenDotOhFloat) }

      // Float +- Byte
      intercept[TestFailedException] { (7.1f +- 2.toByte) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.9f +- 2.toByte) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.0f +- 2.toByte) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (7.2f +- 2.toByte) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (6.8f +- 2.toByte) should not equal (sevenDotOhFloat) }
      intercept[TestFailedException] { (-7.1f +- 2.toByte) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.9f +- 2.toByte) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.0f +- 2.toByte) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-7.2f +- 2.toByte) should not equal (minusSevenDotOhFloat) }
      intercept[TestFailedException] { (-6.8f +- 2.toByte) should not equal (minusSevenDotOhFloat) }

      // Long +- Long
      intercept[TestFailedException] { (9L +- 2L) should not equal (sevenLong) }
      intercept[TestFailedException] { (8L +- 2L) should not equal (sevenLong) }
      intercept[TestFailedException] { (7L +- 2L) should not equal (sevenLong) }
      intercept[TestFailedException] { (6L +- 2L) should not equal (sevenLong) }
      intercept[TestFailedException] { (5L +- 2L) should not equal (sevenLong) }
      intercept[TestFailedException] { (-9L +- 2L) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-8L +- 2L) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-7L +- 2L) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-6L +- 2L) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-5L +- 2L) should not equal (minusSevenLong) }

      // Long +- Int
      intercept[TestFailedException] { (9L +- 2) should not equal (sevenLong) }
      intercept[TestFailedException] { (8L +- 2) should not equal (sevenLong) }
      intercept[TestFailedException] { (7L +- 2) should not equal (sevenLong) }
      intercept[TestFailedException] { (6L +- 2) should not equal (sevenLong) }
      intercept[TestFailedException] { (5L +- 2) should not equal (sevenLong) }
      intercept[TestFailedException] { (-9L +- 2) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-8L +- 2) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-7L +- 2) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-6L +- 2) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-5L +- 2) should not equal (minusSevenLong) }

      // Long +- Short
      intercept[TestFailedException] { (9L +- 2.toShort) should not equal (sevenLong) }
      intercept[TestFailedException] { (8L +- 2.toShort) should not equal (sevenLong) }
      intercept[TestFailedException] { (7L +- 2.toShort) should not equal (sevenLong) }
      intercept[TestFailedException] { (6L +- 2.toShort) should not equal (sevenLong) }
      intercept[TestFailedException] { (5L +- 2.toShort) should not equal (sevenLong) }
      intercept[TestFailedException] { (-9L +- 2.toShort) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-8L +- 2.toShort) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-7L +- 2.toShort) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-6L +- 2.toShort) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-5L +- 2.toShort) should not equal (minusSevenLong) }

      // Long +- Byte
      intercept[TestFailedException] { (9L +- 2.toByte) should not equal (sevenLong) }
      intercept[TestFailedException] { (8L +- 2.toByte) should not equal (sevenLong) }
      intercept[TestFailedException] { (7L +- 2.toByte) should not equal (sevenLong) }
      intercept[TestFailedException] { (6L +- 2.toByte) should not equal (sevenLong) }
      intercept[TestFailedException] { (5L +- 2.toByte) should not equal (sevenLong) }
      intercept[TestFailedException] { (-9L +- 2.toByte) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-8L +- 2.toByte) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-7L +- 2.toByte) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-6L +- 2.toByte) should not equal (minusSevenLong) }
      intercept[TestFailedException] { (-5L +- 2.toByte) should not equal (minusSevenLong) }

      // Int +- Int
      intercept[TestFailedException] { (9 +- 2) should not equal (sevenInt) }
      intercept[TestFailedException] { (8 +- 2) should not equal (sevenInt) }
      intercept[TestFailedException] { (7 +- 2) should not equal (sevenInt) }
      intercept[TestFailedException] { (6 +- 2) should not equal (sevenInt) }
      intercept[TestFailedException] { (5 +- 2) should not equal (sevenInt) }
      intercept[TestFailedException] { (-9 +- 2) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-8 +- 2) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-7 +- 2) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-6 +- 2) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-5 +- 2) should not equal (minusSevenInt) }

      // Int +- Short
      intercept[TestFailedException] { (9 +- 2.toShort) should not equal (sevenInt) }
      intercept[TestFailedException] { (8 +- 2.toShort) should not equal (sevenInt) }
      intercept[TestFailedException] { (7 +- 2.toShort) should not equal (sevenInt) }
      intercept[TestFailedException] { (6 +- 2.toShort) should not equal (sevenInt) }
      intercept[TestFailedException] { (5 +- 2.toShort) should not equal (sevenInt) }
      intercept[TestFailedException] { (-9 +- 2.toShort) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-8 +- 2.toShort) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-7 +- 2.toShort) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-6 +- 2.toShort) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-5 +- 2.toShort) should not equal (minusSevenInt) }

      // Int +- Byte
      intercept[TestFailedException] { (9 +- 2.toByte) should not equal (sevenInt) }
      intercept[TestFailedException] { (8 +- 2.toByte) should not equal (sevenInt) }
      intercept[TestFailedException] { (7 +- 2.toByte) should not equal (sevenInt) }
      intercept[TestFailedException] { (6 +- 2.toByte) should not equal (sevenInt) }
      intercept[TestFailedException] { (5 +- 2.toByte) should not equal (sevenInt) }
      intercept[TestFailedException] { (-9 +- 2.toByte) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-8 +- 2.toByte) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-7 +- 2.toByte) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-6 +- 2.toByte) should not equal (minusSevenInt) }
      intercept[TestFailedException] { (-5 +- 2.toByte) should not equal (minusSevenInt) }

      // Short +- Short
      intercept[TestFailedException] { (9.toShort +- 2.toShort) should not equal (sevenShort) }
      intercept[TestFailedException] { (8.toShort +- 2.toShort) should not equal (sevenShort) }
      intercept[TestFailedException] { (7.toShort +- 2.toShort) should not equal (sevenShort) }
      intercept[TestFailedException] { (6.toShort +- 2.toShort) should not equal (sevenShort) }
      intercept[TestFailedException] { (5.toShort +- 2.toShort) should not equal (sevenShort) }
      intercept[TestFailedException] { ((-9).toShort +- 2.toShort) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-8).toShort +- 2.toShort) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-7).toShort +- 2.toShort) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-6).toShort +- 2.toShort) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-5).toShort +- 2.toShort) should not equal (minusSevenShort) }

      // Short +- Byte
      intercept[TestFailedException] { (9.toShort +- 2.toByte) should not equal (sevenShort) }
      intercept[TestFailedException] { (8.toShort +- 2.toByte) should not equal (sevenShort) }
      intercept[TestFailedException] { (7.toShort +- 2.toByte) should not equal (sevenShort) }
      intercept[TestFailedException] { (6.toShort +- 2.toByte) should not equal (sevenShort) }
      intercept[TestFailedException] { (5.toShort +- 2.toByte) should not equal (sevenShort) }
      intercept[TestFailedException] { ((-9).toShort +- 2.toByte) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-8).toShort +- 2.toByte) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-7).toShort +- 2.toByte) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-6).toShort +- 2.toByte) should not equal (minusSevenShort) }
      intercept[TestFailedException] { ((-5).toShort +- 2.toByte) should not equal (minusSevenShort) }

      // Byte +- Byte
      intercept[TestFailedException] { (9.toByte +- 2.toByte) should not equal (sevenByte) }
      intercept[TestFailedException] { (8.toByte +- 2.toByte) should not equal (sevenByte) }
      intercept[TestFailedException] { (7.toByte +- 2.toByte) should not equal (sevenByte) }
      intercept[TestFailedException] { (6.toByte +- 2.toByte) should not equal (sevenByte) }
      intercept[TestFailedException] { (5.toByte +- 2.toByte) should not equal (sevenByte) }
      intercept[TestFailedException] { ((-9).toByte +- 2.toByte) should not equal (minusSevenByte) }
      intercept[TestFailedException] { ((-8).toByte +- 2.toByte) should not equal (minusSevenByte) }
      intercept[TestFailedException] { ((-7).toByte +- 2.toByte) should not equal (minusSevenByte) }
      intercept[TestFailedException] { ((-6).toByte +- 2.toByte) should not equal (minusSevenByte) }
      intercept[TestFailedException] { ((-5).toByte +- 2.toByte) should not equal (minusSevenByte) }
    }
*/
  }

  object `The X +- Y syntax` {

    def `should throw IllegalArgumentException if the number passed to the right is 0 or negative` {

      // Double +- Double
      val caught1 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -0.2)
      }
      assert(caught1.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.", caught1.getMessage)

      // Double +- Float
      val caught2 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -0.2f)
      }
      assert(caught2.getMessage === "-0.20000000298023224 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Long
      val caught3 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -2L)
      }
      assert(caught3.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Int
      val caught4 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- -2)
      }
      assert(caught4.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Short
      val caught5 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- (-2).toShort)
      }
      assert(caught5.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Double +- Byte
      val caught6 = intercept[IllegalArgumentException] {
        sevenDotOh should equal (7.1 +- (-2).toByte)
      }
      assert(caught6.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Float
      val caught7 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- -0.2f)
      }
      assert(caught7.getMessage === "-0.2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Long
      val caught8 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- -2L)
      }
      assert(caught8.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Int
      val caught9 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- -2)
      }
      assert(caught9.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Short
      val caught10 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- (-2).toShort)
      }
      assert(caught10.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Float +- Byte
      val caught11 = intercept[IllegalArgumentException] {
        sevenDotOhFloat should equal (7.1f +- (-2).toByte)
      }
      assert(caught11.getMessage === "-2.0 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Long
      val caught12 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- -2L)
      }
      assert(caught12.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Int
      val caught13 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- -2)
      }
      assert(caught13.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Short
      val caught14 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- (-2).toShort)
      }
      assert(caught14.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Long +- Byte
      val caught15 = intercept[IllegalArgumentException] {
        sevenLong should equal (9L +- (-2).toByte)
      }
      assert(caught15.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Int
      val caught16 = intercept[IllegalArgumentException] {
        sevenInt should equal (9 +- -2)
      }
      assert(caught16.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Short
      val caught17 = intercept[IllegalArgumentException] {
        sevenInt should equal (9 +- (-2).toShort)
      }
      assert(caught17.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Int +- Byte
      val caught18 = intercept[IllegalArgumentException] {
        sevenInt should equal (9 +- (-2).toByte)
      }
      assert(caught18.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Short
      val caught19 = intercept[IllegalArgumentException] {
        sevenShort should equal (9.toShort +- (-2).toShort)
      }
      assert(caught19.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Short +- Byte
      val caught20 = intercept[IllegalArgumentException] {
        sevenShort should equal (9.toShort +- (-2).toByte)
      }
      assert(caught20.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")

      // Byte +- Byte
      val caught21 = intercept[IllegalArgumentException] {
        sevenByte should equal (9.toByte +- (-2).toByte)
      }
      assert(caught21.getMessage === "-2 passed to +- was zero or negative. Must be a positive non-zero number.")
    }
  }
}
