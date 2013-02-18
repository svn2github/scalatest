/*
 * Copyright 2001-2011 Artima, Inc.
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

/**
 * Holder for deprecated type aliases for traits and classes moved from package <code>org.scalatest.matchers</code> to <code>org.scalatest</code>.
 */
package object matchers {

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.BeMatcher</code> to <code>org.scalatest.BeMatcher</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.BeMatcher instead.")
  type BeMatcher[-T] = org.scalatest.BeMatcher[T]

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.HavePropertyMatcher</code> to <code>org.scalatest.HavePropertyMatcher</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.HavePropertyMatcher instead.")
  type HavePropertyMatcher[-T, P] = org.scalatest.HavePropertyMatcher[T, P]

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.BePropertyMatchResult</code> to <code>org.scalatest.BePropertyMatchResult</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.BePropertyMatchResult instead.")
  type BePropertyMatchResult = org.scalatest.BePropertyMatchResult 

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.MatchResult</code> to <code>org.scalatest.MatchResult</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.MatchResult instead.")
  type MatchResult = org.scalatest.MatchResult 

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.BePropertyMatcher</code> to <code>org.scalatest.BePropertyMatcher</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.BePropertyMatcher instead.")
  type BePropertyMatcher[-T] = org.scalatest.BePropertyMatcher[T]

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.HavePropertyMatchResult</code> to <code>org.scalatest.HavePropertyMatchResult</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.HavePropertyMatchResult instead.")
  type HavePropertyMatchResult[P] = org.scalatest.HavePropertyMatchResult[P]

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.Matcher</code> to <code>org.scalatest.Matcher</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.Matcher instead.")
  type Matcher[-T] = org.scalatest.Matcher[T]
}

