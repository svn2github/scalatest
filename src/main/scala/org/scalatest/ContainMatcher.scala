package org.scalatest

import collection.GenTraversable
import matchers.MatchResult
import matchers.Matcher

trait ContainMatcher[T] extends Function1[GenTraversable[T], MatchResult] {
  
  def apply(left: GenTraversable[T]): MatchResult
  
}
