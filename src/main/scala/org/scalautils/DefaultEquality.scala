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

final class DefaultEquality[A] extends Equality[A] {

  def areEqual(a: A, b: Any): Boolean = {
    a match {
      case arr: Array[_] =>
        b match {
          case brr: Array[_] => arr.deep == brr.deep
          case _ => arr.deep == b
        }
      case _ => {
        b match {
          case brr: Array[_] => a == brr.deep
          case _ => a == b
        }
      }
    }
  }
}
