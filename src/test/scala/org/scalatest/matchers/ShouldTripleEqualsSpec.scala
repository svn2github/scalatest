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
package org.scalatest

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import org.scalautils.TripleEquals
import org.scalautils.TypeCheckedTripleEquals
import org.scalautils.ConversionCheckedTripleEquals

class ShouldTripleEqualsSpec extends Spec with NonImplicitAssertions with ShouldMatchers {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  object `the custom equality should === (operator` {

    object `with TripleEquals` {

      def `should compare anything with anything` {

        new TripleEquals {

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          1 should === (1L)
          intercept[TestFailedException] { 1 should !== (1L) }

          1L should === (1)
          intercept[TestFailedException] { 1L should !== (1) }

          "1" should !== (1)
          intercept[TestFailedException] { "1" should === (1) }

          1 should !== ("1")
          intercept[TestFailedException] { 1 should === ("1") }

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          intercept[TestFailedException] { super1 should === (null) }
          super1 should !== (null)

          nullSuper should === (null)
          intercept[TestFailedException] { nullSuper should !== (null) }
          intercept[TestFailedException] { nullSuper should === (super1) }
          nullSuper should !== (super1)
        }
      }

/*
      def `should be overridable with TypeCheckedTripleEquals locally when TripleEquals imported` {

        object O extends TripleEquals
        import O._

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          ap should === (fr)
          fr should === (ap)
          ap should === (cr)
          cr should === (ap)

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          // The rest should not compile
          // 1 should === (1L)
          // 1L should === (1)
          // 1 should !== (1L)
          // 1L should !== (1)

          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when TripleEquals mixed in` {

        object O extends TripleEquals {

          new TypeCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy
  
            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple
  
            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }
  
            ap should === (fr)
            fr should === (ap)
            ap should === (cr)
            cr should === (ap)

            super1 should !== (super2)
            super1 should !== (sub2)
            sub2 should !== (super1)
            super1 should === (super1)
            super1 should === (sub1)
            sub1 should === (super1)
  
            // The rest should not compile
            // 1 should === (1L)
            // 1L should === (1)
            // 1 should !== (1L)
            // 1L should !== (1)
  
            // "1" should === (1)
            // 1 should === ("1")
            // "1" should !== (1)
            // 1 should !== ("1")
  
            // fr should === (cr)
            // cr should === (fr)
          }
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TripleEquals imported` {

        object O extends TripleEquals
        import O._

        new ConversionCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }

            ap should === (fr)
            fr should === (ap)
            ap should === (cr)
            cr should === (ap)

            super1 should !== (super2)
            super1 should !== (sub2)
            sub2 should !== (super1)
            super1 should === (super1)
            super1 should === (sub1)
            sub1 should === (super1)

            // These should work with implicit conversions
            1 should === (1L)
            1L should === (1)
            intercept[TestFailedException] { 1 should !== (1L) }
            intercept[TestFailedException] { 1L should !== (1) }

            // The rest should not compile
            // "1" should === (1)
            // 1 should === ("1")
            // "1" should !== (1)
            // 1 should !== ("1")

            // fr should === (cr)
            // cr should === (fr)
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TripleEquals mixed in` {

        object O extends TripleEquals {

          new ConversionCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }

            ap should === (fr)
            fr should === (ap)
            ap should === (cr)
            cr should === (ap)

            super1 should !== (super2)
            super1 should !== (sub2)
            sub2 should !== (super1)
            super1 should === (super1)
            super1 should === (sub1)
            sub1 should === (super1)

            // These should work with implicit conversions
            1 should === (1L)
            1L should === (1)
            intercept[TestFailedException] { 1 should !== (1L) }
            intercept[TestFailedException] { 1L should !== (1) }

            // The rest should not compile
            // "1" should === (1)
            // 1 should === ("1")
            // "1" should !== (1)
            // 1 should !== ("1")

            // fr should === (cr)
            // cr should === (fr)
          }
        }
      }
*/
    }

    object `with TypeCheckedTripleEquals` {

      def `should compare supertypes with subtypes on either side` {

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          ap should === (fr)
          fr should === (ap)
          ap should === (cr)
          cr should === (ap)

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          intercept[TestFailedException] { super1 should === (null) }
          super1 should !== (null)

          nullSuper should === (null)
          intercept[TestFailedException] { nullSuper should !== (null) }
          intercept[TestFailedException] { nullSuper should === (super1) }
          nullSuper should !== (super1)

          // The rest should not compile
          // 1 should === (1L)
          // 1L should === (1)
          // 1 should !== (1L)
          // 1L should !== (1)

          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)
        }
      }

/*
      def `should be overridable with TripleEquals locally when TypeCheckedTripleEquals imported` {

        object O extends TypeCheckedTripleEquals
        import O._

        new TripleEquals {

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          1 should === (1L)
          intercept[TestFailedException] { 1 should !== (1L) }

          1L should === (1)
          intercept[TestFailedException] { 1L should !== (1) }

          "1" should !== (1)
          intercept[TestFailedException] { "1" should === (1) }

          1 should !== ("1")
          intercept[TestFailedException] { 1 should === ("1") }

          super1 should !== (super2)
          super1 should !== (sub2)
          // sub2 should !== (super1) // compiles on 2.10 but not 2.9
          super1 should === (super1)
          super1 should === (sub1)
          // sub1 should === (super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with TripleEquals locally when TypeCheckedTripleEquals mixed in` {

        object O extends TypeCheckedTripleEquals {

          new TripleEquals {

            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }

            1 should === (1L)
            intercept[TestFailedException] { 1 should !== (1L) }

            1L should === (1)
            intercept[TestFailedException] { 1L should !== (1) }

            "1" should !== (1)
            intercept[TestFailedException] { "1" should === (1) }

            1 should !== ("1")
            intercept[TestFailedException] { 1 should === ("1") }

            super1 should !== (super2)
            super1 should !== (sub2)
            // sub2 should !== (super1) // compiles on 2.10 but not 2.9
            super1 should === (super1)
            super1 should === (sub1)
            // sub1 should === (super1) // compiles on 2.10 but not 2.9
          }
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TypeCheckedTripleEquals imported` {

        object O extends TypeCheckedTripleEquals
        import O._

        new ConversionCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          // 1 should === (1) // compiles on 2.10 but not 2.9
          // intercept[TestFailedException] { 1 should !== (1)) // compiles on 2.10 but not 2. }

          // ap should === (fr) // compiles on 2.10 but not 2.9
          // compiles on 2.10 but not 2.9/ fr should === (ap) // compiles on 2.10 but not 2.9
          // ap should === (cr) // compiles on 2.10 but not 2.9
          // cr should === (ap) // compiles on 2.10 but not 2.9

          // super1 should !== (super2) // compiles on 2.10 but not 2.9
          // super1 should !== (sub2) // compiles on 2.10 but not 2.9
          // sub2 should !== (super1) // compiles on 2.10 but not 2.9
          // super1 should === (super1) // compiles on 2.10 but not 2.9
          // super1 should === (sub1) // compiles on 2.10 but not 2.9
          // sub1 should === (super1) // compiles on 2.10 but not 2.9

          // These should work with implicit conversions
          1 should === (1L)
          1L should === (1)
          intercept[TestFailedException] { 1 should !== (1L) }
          intercept[TestFailedException] { 1L should !== (1) }

          // The rest should not compile
          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TypeCheckedTripleEquals mixed in` {

        object O extends TypeCheckedTripleEquals {

          new ConversionCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            // 1 should === (1) // compiles on 2.10 but not 2.9
            // intercept[TestFailedException] { 1 should !== (1)) // compiles on 2.10 but not 2. }

            // ap should === (fr) // compiles on 2.10 but not 2.9
            // fr should === (ap) // compiles on 2.10 but not 2.9
            // ap should === (cr) // compiles on 2.10 but not 2.9
            // cr should === (ap) // compiles on 2.10 but not 2.9

            // super1 should !== (super2) // compiles on 2.10 but not 2.9
            // super1 should !== (sub2) // compiles on 2.10 but not 2.9
            // sub2 should !== (super1) // compiles on 2.10 but not 2.9
            // super1 should === (super1) // compiles on 2.10 but not 2.9
            // super1 should === (sub1) // compiles on 2.10 but not 2.9
            // sub1 should === (super1) // compiles on 2.10 but not 2.9

            // These should work with implicit conversions
            1 should === (1L)
            1L should === (1)
            intercept[TestFailedException] { 1 should !== (1L) }
            intercept[TestFailedException] { 1L should !== (1) }

            // The rest should not compile
            // "1" should === (1)
            // 1 should === ("1")
            // "1" should !== (1)
            // 1 should !== ("1")

            // fr should === (cr)
            // cr should === (fr)
          }
        }
      }
*/
    }

    object `with ConversionCheckedTripleEquals` {

      def `should compare supertypes with subtypes on either side as well as types with implicit conversions in either direction` {

        new ConversionCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          ap should === (fr)
          fr should === (ap)
          ap should === (cr)
          cr should === (ap)

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          // These should work with implicit conversions
          1 should === (1L)
          1L should === (1)
          intercept[TestFailedException] { 1 should !== (1L) }
          intercept[TestFailedException] { 1L should !== (1) }

          // Should work sensibly with nulls
          intercept[TestFailedException] { super1 should === (null) }
          super1 should !== (null)

          nullSuper should === (null)
          intercept[TestFailedException] { nullSuper should !== (null) }
          intercept[TestFailedException] { nullSuper should === (super1) }
          nullSuper should !== (super1)

          // The rest should not compile
          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)
        }
      }

/*
      def `should be overridable with TripleEquals locally when ConversionCheckedTripleEquals imported` {

        object O extends ConversionCheckedTripleEquals
        import O._

        new TripleEquals {

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          // 1 should === (1L) // compiles on 2.10 but not 2.9
          // intercept[TestFailedException] { 1 should !== (1L)) // compiles on 2.10 but not 2. }

          1L should === (1)
          intercept[TestFailedException] { 1L should !== (1) }

          "1" should !== (1)
          intercept[TestFailedException] { "1" should === (1) }

          1 should !== ("1")
          intercept[TestFailedException] { 1 should === ("1") }

          super1 should !== (super2)
          super1 should !== (sub2)
          // sub2 should !== (super1) // compiles on 2.10 but not 2.9
          super1 should === (super1)
          super1 should === (sub1)
          // sub1 should === (super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with TripleEquals locally when ConversionCheckedTripleEquals mixed in` {

        object O extends ConversionCheckedTripleEquals {

          new TripleEquals {

            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }

            // 1 should === (1L) // compiles on 2.10 but not 2.9
            // intercept[TestFailedException] { 1 should !== (1L)) // compiles on 2.10 but not 2. }

            1L should === (1)
            intercept[TestFailedException] { 1L should !== (1) }

            "1" should !== (1)
            intercept[TestFailedException] { "1" should === (1) }

            1 should !== ("1")
            intercept[TestFailedException] { 1 should === ("1") }

            super1 should !== (super2)
            super1 should !== (sub2)
            // sub2 should !== (super1) // compiles on 2.10 but not 2.9
            super1 should === (super1)
            super1 should === (sub1)
            // sub1 should === (super1) // compiles on 2.10 but not 2.9
          }
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when ConversionCheckedTripleEquals imported` {

        object O extends ConversionCheckedTripleEquals
        import O._

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          // 1 should === (1) // compiles on 2.10 but not 2.9
          // intercept[TestFailedException] { 1 should !== (1)) // compiles on 2.10 but not 2. }

          // ap should === (fr) // compiles on 2.10 but not 2.9
          // fr should === (ap) // compiles on 2.10 but not 2.9
          // ap should === (cr) // compiles on 2.10 but not 2.9
          // cr should === (ap) // compiles on 2.10 but not 2.9

          // super1 should !== (super2) // compiles on 2.10 but not 2.9
          // super1 should !== (sub2) // compiles on 2.10 but not 2.9
          // sub2 should !== (super1) // compiles on 2.10 but not 2.9
          // super1 should === (super1) // compiles on 2.10 but not 2.9
          // super1 should === (sub1) // compiles on 2.10 but not 2.9
          // sub1 should === (super1) // compiles on 2.10 but not 2.9

          // The rest should not compile
          // 1 should === (1L)
          // 1L should === (1)
          // 1 should !== (1L)
          // 1L should !== (1)

          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when ConversionCheckedTripleEquals mixed in` {

        object O extends ConversionCheckedTripleEquals {

          new TypeCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            // 1 should === (1) // compiles on 2.10 but not 2.9
            // intercept[TestFailedException] { 1 should !== (1) } // compiles on 2.10 but not 2.9

            // ap should === (fr) // compiles on 2.10 but not 2.9
            // fr should === (ap) // compiles on 2.10 but not 2.9
            // ap should === (cr) // compiles on 2.10 but not 2.9
            // cr should === (ap) // compiles on 2.10 but not 2.9

            // super1 should !== (super2) // compiles on 2.10 but not 2.9
            // super1 should !== (sub2) // compiles on 2.10 but not 2.9
            // sub2 should !== (super1) // compiles on 2.10 but not 2.9
            // super1 should === (super1) // compiles on 2.10 but not 2.9
            // super1 should === (sub1) // compiles on 2.10 but not 2.9
            // sub1 should === (super1) // compiles on 2.10 but not 2.9

            // The rest should not compile
            // 1 should === (1L)
            // 1L should === (1)
            // 1 should !== (1L)
            // 1L should !== (1)

            // "1" should === (1)
            // 1 should === ("1")
            // "1" should !== (1)
            // 1 should !== ("1")

            // fr should === (cr)
            // cr should === (fr)
          }
        }
      }
*/
    }
  }
}

