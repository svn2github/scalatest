/*
 * Copyright 2001-2008 Artima, Inc.
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

class AsAnySpec extends Spec {

  object `the .asAny method` {

    object `provided by TypeCheckedTripleEquals` {
      def `should convert any type to Any` {
        new TypeCheckedTripleEquals {
          assert(1.asAny !== "1".asAny)
          assert(this.isInstanceOf[AsAny])

          // Should not compile
          // assert(1 !== "a")
        }
      }
    }

    object `provided by TypeCheckedLegacyTripleEquals` {
      def `should convert any type to Any` {
        new TypeCheckedLegacyTripleEquals {
          assert(1.asAny !== "1".asAny)
          assert(this.isInstanceOf[AsAny])

          // Should not compile
          // assert(1 !== "a")
        }
      }
    }

    object `provided by ConversionCheckedTripleEquals` {
      def `should convert any type to Any` {
        new ConversionCheckedTripleEquals {
          assert(1.asAny !== "1".asAny)
          assert(this.isInstanceOf[AsAny])

          // Should not compile
          // assert(1 !== "a")
        }
      }
    }

    object `provided by ConversionCheckedLegacyTripleEquals` {
      def `should convert any type to Any` {
        new ConversionCheckedLegacyTripleEquals {
          assert(1.asAny !== "1".asAny)
          assert(this.isInstanceOf[AsAny])

          // Should not compile
          // assert(1 !== "a")
        }
      }
    }
  }
}

