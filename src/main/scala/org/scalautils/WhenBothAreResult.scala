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

final class WhenBothAreResult[B](normalization: Normalization[B], b: B) {

  def isEqual(a: B): Boolean = Equality(normalization).areEqual(a, b)

  def and(otherNormalization: Normalization[B]): WhenBothAreResult[B] = 
    new WhenBothAreResult(normalization and otherNormalization, b)
} 

