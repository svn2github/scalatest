package org.scalatest.matchers

import collection.GenTraversable

trait ContainMatcher[T] {
  
  def apply(left: GenTraversable[T]): MatchResult
  
}