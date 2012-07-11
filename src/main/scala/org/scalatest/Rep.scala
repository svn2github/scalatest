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

/**
 * Trait used to pass an <code>Informer</code> and a <code>Documenter</code> to
 * test methods in trait <code>Suite</code> and <code>fixture.Suite</code>.
 */
trait Rep {

  /**
   * An <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter.  This method can be called safely by any thread.
   *
   * <p>
   * This field is implicit to enable it to be easily used with constructs like the <code>given</code>,
   * <code>when</code>, and <code>then</code> methods of trait
   * <a href="GivenWhenThen.html"><code>GivenWhenThen</code></a>.
   * </p>
   */
  implicit val info: Informer

  /**
   * A <code>Documenter</code> that during test execution will forward strings passed to its
   * <code>apply</code> method to the current reporter. This method can be called safely by any thread.
   */
  implicit val markup: Documenter
}
