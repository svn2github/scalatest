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
 * Trait that when mixed into a <code>Suite</code> cancels any remaining tests in that
 * <code>Suite</code> instance after a test fails.
 *
 * <p>
 * Note that this trait only cancels tests in the same <code>Suite</code> instance.
 * If you are running each test in its own instance, 
 * </p>
 */
trait CancelAfterFailure extends AbstractSuite { this: Suite =>

  @volatile private var cancelRemaining = false

  abstract override def withFixture(test: NoArgTest) {
    if (cancelRemaining) cancel("Canceled by CancelOnFailure because a test failed previously")
    try super.withFixture(test)
    catch {
      case e: TestFailedException =>
        cancelRemaining = true
        throw e
    }
  }

  /**
   */
  final def newInstance: Suite with OneInstancePerTest = throw new UnsupportedOperationException
}
