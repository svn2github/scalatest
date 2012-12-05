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

import annotation.implicitNotFound

@implicitNotFound(msg = "types ${A} and ${B} do not adhere to the equality constraint selected for the === and !== operators; they must either be in a subtype/supertype relationship, or, if ConversionCheckedTripleEquals is in force, implicitly convertible in one direction or the other; the missing implicit parameter is of type org.scalautils.EqualityConstraint[${A},${B}]")
abstract class EqualityConstraint[A, B] {
  def areEqual(left: A, right: B): Boolean
}

class BasicEqualityConstraint[A, B](equalityOfA: Equality[A]) extends EqualityConstraint[A, B] {
  def areEqual(left: A, right: B): Boolean = equalityOfA.areEqual(left, right)
}

class BToAEqualityConstraint[A, B](equalityOfA: Equality[A], cnv: B => A) extends EqualityConstraint[A, B] {
  override def areEqual(left: A, right: B): Boolean = equalityOfA.areEqual(left, cnv(right))
}

class AToBEqualityConstraint[A, B](equalityOfB: Equality[B], cnv: A => B) extends EqualityConstraint[A, B] {
  override def areEqual(left: A, right: B): Boolean = equalityOfB.areEqual(cnv(left), right)
}
