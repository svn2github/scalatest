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

trait TimesOnInt {
  class Repeater(num: Int) {
    require(num >= 0, "The integer on which times was invoked was less than zero: " + num)
    def times(f: => Unit) {
      var i = 0
      while (i < num) {
        f
        i += 1
      }
    }
  }
  implicit def convertIntToRepeater(num: Int): Repeater = new Repeater(num)
}

object TimesOnInt extends TimesOnInt

