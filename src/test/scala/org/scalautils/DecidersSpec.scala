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

class DecidersSpec extends Spec with NonImplicitAssertions with TripleEquals with Deciders with StringNormalizations {

  object `The 'decided by' syntax` {
    def `should enable users to explicitly choose an Equality for a === use` { 
  
      assert(3 === 3)
      assert(3 !== 4)
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      assert(3 !== 3)
      assert(3 === 4)
      // And now with "decided by" to go back to defaultEquality
      assert((3 === 3) (decided by defaultEquality))
      assert(!(3 === 4) (decided by defaultEquality))
      assert((3 !== 4) (decided by defaultEquality))
      assert(!(3 !== 3) (decided by defaultEquality))
    }
  }

  object `The 'after being' syntax` {
    def `should enable users to explicitly choose a Normalization for a === use` { 

      assert("hello" !== "HELLO")
      assert(("hello" === "HELLo") (after being lowerCased))

      assert("HELLO" !== "hello")
      assert(("HELLO" === "hello") (after being lowerCased))

      assert("HeLlO" !== "hElLo")
      assert(("HeLlO" === "hElLo") (after being lowerCased))

      assert(("hello" !== "Helloooo") (after being lowerCased))
    }

    def `should enable users to explicitly build a Normalization for a === use by composing with 'and', with or without parens` { 
      assert("hello" !== "HELLO")
      assert(("hello" === " HELLo ") (after being (lowerCased and trimmed)))

      assert("HELLO" !== "hello")
      assert(("  HELLO " === "hello") (after being (lowerCased and trimmed)))

      assert("  HeLlO" !== "HeLlO\n")
      assert(("  HeLlO" === "HeLlO\n") (after being (lowerCased and trimmed)))

      assert("hello" !== "HELLO")
      assert(("hello" === " HELLo ") (after being lowerCased and trimmed))

      assert("HELLO" !== "hello")
      assert(("  HELLO " === "hello") (after being lowerCased and trimmed))

      assert("  HeLlO" !== "HeLlO\n")
      assert(("  HeLlO" === "HeLlO\n") (after being lowerCased and trimmed))
    }
  }
}

