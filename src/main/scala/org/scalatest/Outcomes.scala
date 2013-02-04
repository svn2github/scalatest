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
package org.scalatest

import exceptions.TestCanceledException
import exceptions.TestOmittedException

// Note no Ignored outcome, because ignore is done with a tag and is known
// before a test is executed. Outcome is only modeling the outcomes of
// executing a test body.
private[scalatest] trait Outcomes {
  trait FailedOrCanceled
  sealed abstract class Outcome {
    def isSucceeded: Boolean = false
    def isFailed: Boolean = false
    def isCanceled: Boolean = false
    def isPending: Boolean = false
    def isOmitted: Boolean = false
    def isDefined: Boolean = true
    def isEmpty: Boolean = false
    def toOption: Option[Throwable] = None
  }
  abstract class Exceptional(ex: Throwable) extends Outcome {
    override def toOption: Option[Throwable] = Some(ex)
  }
  case object Succeeded extends Outcome {
    override def isSucceeded: Boolean = true
    override def isDefined: Boolean = false
    override def isEmpty: Boolean = true
  }
  case class Failed(ex: Throwable) extends Exceptional(ex) with FailedOrCanceled {
    override def isFailed: Boolean = true
  }
  case class Canceled(ex: TestCanceledException) extends Exceptional(ex) with FailedOrCanceled {
    override def isCanceled: Boolean = true
  }
  case class Pending(ex: TestPendingException) extends Exceptional(ex) {
    override def isPending: Boolean = true
  }
  case class Omitted(ex: TestOmittedException) extends Exceptional(ex) {
    override def isOmitted: Boolean = true
  }
  object FailedOrCanceled {
    def unapply(res: Outcome): Option[Throwable] = 
      res match {
        case Failed(ex) => Some(ex)
        case Canceled(ex) => Some(ex)
        case _ => None
      }
  }
  object Exceptional {
    def unapply(res: Outcome): Option[Throwable] = 
      res match {
        case Failed(ex) => Some(ex)
        case Canceled(ex) => Some(ex)
        case Pending(ex) => Some(ex)
        case Omitted(ex) => Some(ex)
        case _ => None
      }
  }
  object Outcome {
    implicit def convertOutcomeToIterable(res: Outcome): scala.collection.immutable.Iterable[Throwable] = {
      res match {
        case Exceptional(ex) => Vector(ex)
        case _ => Vector.empty
      }
    }
  }

  def outcomeOf(f: => Unit): Outcome = {
    try {                                         
      f                                           
      Succeeded
    }                                             
    catch {                                       
      case ex: TestCanceledException => Canceled(ex)                           
      case ex: TestPendingException => Pending(ex)                           
      case ex: TestOmittedException => Omitted(ex)                           
      case ex: Throwable if !Suite.anErrorThatShouldCauseAnAbort(ex) => Failed(ex)                           
    }
  }
}

