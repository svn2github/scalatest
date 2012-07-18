package org.scalatest

/**
 * A bundle of information about the current test.
 *
 * <p>
 * A <code>TestData</code> object is passed to the <code>withFixture</code> methods of traits <code>Suite</code> and <code>fixture.Suite</code>
 * (both <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> and <a href="fixture/Suite$OneArgTest.html"><code>OneArgTest</code></a>
 * extend <code>TestData</code>) and to the <code>beforeEach</code> and <code>afterEach</code>
 * methods of trait <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>. This enables fixtures and tests to make use
 * of the test name and configuration objects in the config map.
 * </p>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
trait TestData {

  /**
   * A <code>Map[String, Any]</code> containing objects that can be used
   * to configure the fixture and test.
   */
  def configMap: Map[String, Any] 

  /**
   * The name of this test.
   */
  def name: String
}
