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
package org.scalatest.matchers

import org.scalatest._

// TODO: Deprecate this one

/**
 * Companion object for trait <code>BeMatcher</code> that provides a
 * factory method that creates a <code>BeMatcher[T]</code> from a
 * passed function of type <code>(T =&gt; MatchResult)</code>.
 *
 * @author Bill Venners
 */
object BeMatcher {

  /**
   * Factory method that creates a <code>BeMatcher[T]</code> from a
   * passed function of type <code>(T =&gt; MatchResult)</code>.
   *
   * @author Bill Venners
   */
  def apply[T](fun: T => MatchResult): BeMatcher[T] =
    new BeMatcher[T] {
      def apply(left: T) = fun(left)
    }
}

