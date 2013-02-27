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
package org.scalatest.words

trait MatcherWords {

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (fullyMatch regex ("Hel*o, wor.d") and not have length (99))
   *                ^
   * </pre>
   */
  val fullyMatch = new FullyMatchWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (startWith ("Four") and include ("year"))
   *                ^
   * </pre>
   */
  val startWith = new StartWithWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (endWith ("ago") and include ("score"))
   *                ^
   * </pre>
   */
  val endWith = new EndWithWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (include ("hope") and not startWith ("no"))
   *                ^
   * </pre>
   */
  val include = new IncludeWord

/*
  /**
   * This field enables syntax like the following: 
   *
   * <pre class="stHighlight">
   * myFile should (not be an (directory) and not have ('name ("foo.bar")))
   *                ^
   * </pre>
   */
  val not = new NotWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
   *             ^
   * </pre>
   */
  val be = new BeWord

/*
    In HaveWord's methods key, value, length, and size, I can give type parameters.
    The type HaveWord can contain a key method that takes a S or what not, and returns a matcher, which
    stores the key value in a val and whose apply method checks the passed map for the remembered key. This one would be used in things like:

    map should { have key 9 and have value "bob" }

    There's an overloaded should method on Shouldifier that takes a HaveWord. This method results in
    a different type that also has a key method that takes an S. So when you say:

    map should have key 9

    what happens is that this alternate should method gets invoked. The result is this other class that
    has a key method, and its constructor takes the map and stores it in a val. So this time when key is
    invoked, it checks to make sure the passed key is in the remembered map, and does the assertion.

    length and size can probably use structural types, because I want to use length on string and array for
    starters, and other people may create classes that have length methods. Would be nice to be able to use them.
  */

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (have length (3) and not contain ('a'))
   *              ^
   * </pre>
   */
  val have = new HaveWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (contain ('a') and have length (7))
   *              ^
   * </pre>
   */
  val contain = new ContainWord
*/
}

object MatcherWords extends MatcherWords
