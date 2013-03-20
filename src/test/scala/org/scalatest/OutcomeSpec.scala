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

class OutcomeSpec extends Spec with OptionValues with OutcomeOf {
  object `An Outcome` {
    def `can be Succeeded` {
      assert(Succeeded.isSucceeded)
      assert(!Succeeded.isFailed)
      assert(!Succeeded.isCanceled)
      assert(!Succeeded.isPending)
      assert(!Succeeded.isOmitted)
    }
    def `can be Failed` {
      val ex = new Exception
      assert(!Failed(ex).isSucceeded)
      assert(Failed(ex).isFailed)
      assert(!Failed(ex).isCanceled)
      assert(!Failed(ex).isPending)
      assert(!Failed(ex).isOmitted)
    }
    def `can be Canceled` {
      val ex = new exceptions.TestCanceledException(0)
      assert(!Canceled(ex).isSucceeded)
      assert(!Canceled(ex).isFailed)
      assert(Canceled(ex).isCanceled)
      assert(!Canceled(ex).isPending)
      assert(!Canceled(ex).isOmitted)
    }
    def `can be Pending` {
      assert(!Pending().isSucceeded)
      assert(!Pending().isFailed)
      assert(!Pending().isCanceled)
      assert(Pending().isPending)
      assert(!Pending().isOmitted)
    }
    def `can be Omitted` {
      assert(!Omitted.isSucceeded)
      assert(!Omitted.isFailed)
      assert(!Omitted.isCanceled)
      assert(!Omitted.isPending)
      assert(Omitted.isOmitted)
    }
    val res1: Outcome = Succeeded
    val ex2 = new Exception
    val res2: Outcome = Failed(ex2)
    val ex3 = new exceptions.TestCanceledException(0)
    val res3: Outcome = Canceled(ex3)
    val res4: Outcome = Pending()
    val res5: Outcome = Omitted
/*
    def `can be easily pattern matched on based on whether it is either Failed or Canceled` {
      def isFailedOrCanceled(res: Outcome): Boolean =
        res match {
          case _: FailedOrCanceled => true
          case _ => false
        }
      assert(!isFailedOrCanceled(res1))
      assert(isFailedOrCanceled(res2))
      assert(isFailedOrCanceled(res3))
      assert(!isFailedOrCanceled(res4))
      assert(!isFailedOrCanceled(res5))
    }
    def `can be easily pattern matched on, extracting the exception, based on whether it is either Failed or Canceled` {
      def insideFailedOrCanceled(res: Outcome): Option[Throwable] =
        res match {
          case ExceptionalH(ex) => Some(ex)
          case _ => None
        }
      assert(insideFailedOrCanceled(res1).isEmpty)
      assert(insideFailedOrCanceled(res2).value eq ex2)
      assert(insideFailedOrCanceled(res3).value eq ex3)
      assert(insideFailedOrCanceled(res4).isEmpty)
      assert(insideFailedOrCanceled(res5).isEmpty)
    }
*/
    def `can be easily pattern matched on based on whether it is Exceptional` {
      def isExceptional(res: Outcome): Boolean =
        res match {
          case _: Exceptional => true
          case _ => false
        }
      assert(!isExceptional(res1))
      assert(isExceptional(res2))
      assert(isExceptional(res3))
      assert(!isExceptional(res4))
      assert(!isExceptional(res5))
    }
    def `can be easily pattern matched on, extracting the exception, based on whether it is Exceptional` {
      def insideExceptional(res: Outcome): Option[Throwable] =
        res match {
          case Exceptional(ex) => Some(ex)
          case _ => None
        }
      assert(insideExceptional(res1).isEmpty)
      assert(insideExceptional(res2).value eq ex2)
      assert(insideExceptional(res3).value eq ex3)
      assert(insideExceptional(res4).isEmpty)
      assert(insideExceptional(res5).isEmpty)
    }
    def `can be queried to determine whether or not it is defined` {
      assert(!res1.isDefined)
      assert(res2.isDefined)
      assert(res3.isDefined)
      assert(!res4.isDefined)
      assert(!res5.isDefined)
    }
    def `can be queried to determine whether or not it is empty` {
      assert(res1.isEmpty)
      assert(!res2.isEmpty)
      assert(!res3.isEmpty)
      assert(res4.isEmpty)
      assert(res5.isEmpty)
    }
    def `can be transformed into an Option[Throwable]` {
      assert(res1.toOption.isEmpty)
      assert(res2.toOption.value eq ex2)
      assert(res3.toOption.value eq ex3)
      assert(res4.toOption.isEmpty)
      assert(res5.toOption.isEmpty)
    }
/*
    def `can be implicitly converted to an Iterable so it can be flattened` {
      assert(res1.iterator.size === 0)
      assert(res2.iterator.size === 1)
      assert(res3.iterator.size === 1)
      assert(res4.iterator.size === 1)
      assert(res5.iterator.size === 1)
      assert(Vector(res1, res2, res3, res4, res5).flatten === Vector(ex2, ex3))
    }
*/
  }

  object `The outcomeOf method` {
    def `must transform expression evaluations into the appropriate Outcome class` {
      assert(outcomeOf { 99 } == Succeeded)
      val tfe = new exceptions.TestFailedException(0)
      assert(outcomeOf { throw tfe } === Failed(tfe))
      val iae = new IllegalArgumentException
      assert(outcomeOf { throw iae } === Failed(iae))
      val tce = new exceptions.TestCanceledException(0)
      assert(outcomeOf { throw tce } === Canceled(tce))
      val tpe = new exceptions.TestPendingException
      assert(outcomeOf { throw tpe } === Pending(None))
      val toe = new exceptions.TestOmittedException
      assert(outcomeOf { throw toe } === Omitted)
    }
    def `if UnknownError is thrown, should complete abruptly with that exception` {
      intercept[UnknownError] {
        outcomeOf { throw new UnknownError }
      }
    }
  }
}

