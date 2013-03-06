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
package org.scalatest

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestFailedException
import org.scalautils.Equality

// Calling this ShouldContainElementNewSpec so that it is easy to 
// keep track of the new tests that we'll need to port over to
// inspector shorthands.
class ShouldContainElementNewSpec extends Spec with Matchers {

  // Checking for a specific size
  object `The 'contain (<value>)' syntax` {
    def `should allow subtypes of the element type to be passed in if modified by asAny` {
      Vector(1, "2") should contain ("2".asAny)
      Vector(1, "2") should contain (1.asAny)
    }
    def `should use an Equality of the element type of the left-hand "holder"` {
      
      Vector(1, 2) should contain (2)
      intercept[TestFailedException] {
        Vector(1, 2) should not contain (2)
      }

      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      
      intercept[TestFailedException] {
        Vector(2, 2) should contain (2)
      }
      Vector(1, 1) should not contain (2)
    }
  }
}

