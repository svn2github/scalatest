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

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class NormalizedEqualitySpec extends Spec with NonImplicitAssertions {

  final case class StringWrapper(var value: String, var isNormalized: Boolean = false, var equalsWasCalled: Boolean = false) {
    override def equals(other: Any): Boolean = {
      equalsWasCalled = true
      other match {
        case that: StringWrapper => value == that.value
        case _ => false
      }
    }
  }

  class NormalizedStringWrapperEquality extends NormalizedEquality[StringWrapper] {
    def isInstanceOfA(b: Any): Boolean = b.isInstanceOf[StringWrapper]
    def normalized(sw: StringWrapper): StringWrapper = {
      sw.value = sw.value.toLowerCase
      sw.isNormalized = true
      sw
    }
  }

  object `A NormalizedEquality type class` {

    def `should call .equals on the left hand object (and not on the right hand object)` {

      val a = StringWrapper("HowDy")
      val b = StringWrapper("hoWdY")
      assert(!a.equalsWasCalled)
      assert(!b.equalsWasCalled)
      assert((new NormalizedStringWrapperEquality).areEqual(a, b))
      assert(a.equalsWasCalled)
      assert(!b.equalsWasCalled)
    }

    def `should normalize both sides when areEqual is called` {

      val a = StringWrapper("HowDy")
      val b = StringWrapper("hoWdY")
      assert(!a.isNormalized)
      assert(!b.isNormalized)
      assert((new NormalizedStringWrapperEquality).areEqual(a, b))
      assert(a.isNormalized)
      assert(b.isNormalized)
    }

    @Ignore def `should call .deep first if left side, right side, or both are Arrays` {
      val a = Array(1, 2, 3)
      val b = Array(1, 2, 3)
      val v = Vector(1, 2, 3)
      assert((new DefaultEquality[Array[Int]]).areEqual(a, v))
      assert((new DefaultEquality[Vector[Int]]).areEqual(v, a))
      assert((new DefaultEquality[Array[Int]]).areEqual(a, b))
    }
  }
}

