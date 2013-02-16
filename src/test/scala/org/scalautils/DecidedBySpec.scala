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

class DecidedBySpec extends Spec with NonImplicitAssertions with TripleEquals with DecidedBy {

  object `The decidedBy syntax` {
    def `should enable users to explicitly chose an Equality for a === use` { 
        
      assert(3 === 3)
      assert(3 !== 4)
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      assert(3 !== 3)
      assert(3 === 4)
      // And now with decidedBy to go back to defaultEquality
      assert(3 === (3 decidedBy defaultEquality[Int]))
      assert(3 !== (4 decidedBy defaultEquality[Int]))
    }
  }
}

