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
 * Container for run arguments.
 *
 * <p>
 * An instance of <code>RunArgs</code> is passed to the <code>run</code>, <code>runNestedSuites</code>, <code>runTests</code>, and <code>runTest</code> methods
 * of trait <a href="AbstractSuite.html".<code>AbstractSuite</code></a>.
 * </p>
 *
 * @param reporter the <code>Reporter</code> to which results will be reported
 * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
 * @param filter a <code>Filter</code> with which to filter tests based on their tags
 * @param configMap a <code>Map</code> of key-value pairs that can be used by the executing <code>Suite</code> of tests.
 * @param distributor an optional <code>Distributor</code>, into which to put nested <code>Suite</code>s to be executed
 *              by another entity, such as concurrently by a pool of threads. If <code>None</code>, nested <code>Suite</code>s will be executed sequentially.
 * @param tracker a <code>Tracker</code> tracking <code>Ordinal</code>s being fired by the current thread.
 * @param chosenStyles a (possibly empty) <code>Set</code> of <code>String</code>s specifying the run's <a href="tools/Runner$.html#chosenStyles"><em>chosen styles</em></a>
 *
 * @throws NullPointerException if any passed parameter is <code>null</code>.
 *
 */
case class RunArgs(
  reporter: Reporter,
  stopper: Stopper,
  filter: Filter,
  configMap: Map[String, Any],
  distributor: Option[Distributor],
  tracker: Tracker,
  chosenStyles: Set[String]
) {
  if (reporter == null)
    throw new NullPointerException("reporter was null")
  if (stopper == null)
    throw new NullPointerException("stopper was null")
  if (filter == null)
    throw new NullPointerException("filter was null")
  if (configMap == null)
    throw new NullPointerException("configMap was null")
  if (distributor == null)
    throw new NullPointerException("distributor was null")
  if (tracker == null)
    throw new NullPointerException("tracker was null")
  if (chosenStyles == null)
    throw new NullPointerException("chosenStyles was null")
}
