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
package org.scalautils

/**
 * Trait that provides <code>===</code> and <code>!==</code> operators that can be configured to enforce compile-time type constraints for
 * equality comparisons.
 *
 * <p>
 * This trait defines several methods that are selectively made implicit by subclasses to enable a spectrum of type constraints for the
 * <code>===</code> and <code>!==</code> operators. As an illustration, if in the expression, <code>a === b</code>, the type of <code>a</code>
 * is <code>A</code> and the type of <code>b</code> is <code>B</code>, the following three levels of compile-time checking can be obtained from
 * <code>EqualityConstraints</code> subtraits:
 * </p>
 *
 * <p>
 * <b>Unchecked</b> - <code>A</code> and <code>B</code> can be any two types. This (weakest) constraint level is available from
 * subtraits <a href="TripleEquals.html"><code>TripleEquals</code></a> and <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>.
 * </p>
 * 
 * <p>
 * <b>Conversion checked</b> - <code>A</code> must be a subtype of <code>B</code>, or vice versa, or an implicit conversion must be available that converts
 * <code>A</code> to <code>B</code>, or vice versa. (Both <code>A</code> and <code>B</code> can be the same type, because a type is considered a subtype
 * of itself.)
 * This (intermediate) constraint level is available from subtraits <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> and <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>.
 * </p>
 * 
 * <p>
 * <b>Type checked</b> - <code>A</code> must be a subtype of <code>B</code>, or vice versa.
 * (Both <code>A</code> and <code>B</code> can be the same type, because a type is considered a subtype
 * of itself.)
 * This (strongest) constraint level is available from subtraits <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>.
 * </p>
 *
 * <p>
 * The difference between the regular and &ldquo;legacy&rdquo; variants of each pair of traits is that the  <code>===</code> and <code>!==</code> operators
 * provided by the regular variants result in <code>Boolean</code>, whereas those of the legacy variants result in <code>Option[String]</code>. For example, were you
 * to mix in <code>TripleEquals</code>, the expression <code>1 + 1 === 3</code> would return <code>false</code>. Were you to mix in <code>LegacyTripleEquals</code>,
 * by contrast, the expression <code>1 + 1 === 3</code> would return <code>Some("2 did not equal 3")</code>.
 * </p>
 *
 * <p>
 * The purpose of the legacy variants is to maintain compatibility with
 * existing code that uses ScalaTest's original <code>===</code> defined in trait <a href="../scalatest/Assertions.html"><code>org.scalatest.Assertions</code></a>. This
 * <code>===</code> operator returned an
 * <code>Option[String]</code> to facilitate better error messages. With the advent of macros in Scala 2.10, it is possible to obtain good error messages by making
 * <code>assert</code> a macro. Once ScalaTest no longer supports Scala 2.9, the legacy variants (<code>LegacyTripleEquals</code>,
 * <code>ConversionCheckedLegacyTripleEquals</code>, and <code>TypeCheckedLegacyTripleEquals</code>) will be deprecated and eventually removed, and good error
 * messages will be obtained via macros. 
 * </p>
 * 
 * <p>
 * This trait defines all methods that need to be defined implicitly by the six subtraits so that if multiple subtraits are used together, the inner-most
 * subtrait in scope can not only enable the implicits it needs by overriding or hiding those methods (currently-in-scope as regular, non-implicit methods) and making
 * them implicit, it can also <em>disable</em> any implicits enabled by its sibling subtraits in enclosing scopes. For example, if your test class mixes
 * in <code>TypeCheckedTripleEquals</code>, inside your test class the following methods will be implicit:
 * </p>
 *
 * <ul>
 * <li><code>convertToCheckingEqualize</code></li>
 * <li><code>typeCheckedEqualityConstraint</code></li>
 * <li><code>lowPriorityTypeCheckedEqualityConstraint</code></li>
 * </ul>
 * 
 * <p>
 * If in the body of a test you want to turn off the type checking, you can import the members
 * of <code>TripleEquals</code> in the body of that test. This will not only hide
 * non-implicit methods <code>convertToEqualizer</code> <code>unconstrainedEquality</code> of <code>TypeCheckedTripleEquals</code>,
 * replacing those with implicit ones defined in <code>TripleEquals</code>, it will also hide the three methods made implicit in <code>TypeCheckedTripleEquals</code>
 * (and listed above), replacing them by <em>non-implicit</em> ones.
 * </p>
 * 
 * <p>
 * In short, you should be able to select a primary constraint level via either a mixin or import, then change that in nested scopes
 * however you want, again either through a mixin or import, without getting any implicit conversion ambiguity. The innermost constraint level in scope
 * will always be in force.
 * <p>
 * 
 * @author Bill Venners
 */
trait EqualityConstraints { // TODO: Maybe say DefaultEquality[A] as the return type of defaultEquality. And of course, see if i can make it implicit here and leave it out of subtraits. Tried that once and it didn't work, but didn't have time to investigate.

  /**
   * Return an <code>Equality[A]</code> for any type <code>A</code> that determines equality via the <code>==</code> operator on type <code>A</code>.
   *
   * @return a <code>DefaultEquality</code> for type <code>A</code>
   */
  def defaultEquality[A]: Equality[A] = new DefaultEquality[A]

  /**
   * Convert to an <a href="Equalizer.html"><code>Equalizer</code></a> that provides <code>===</code> and <code>!==</code> operators that
   * result in <code>Boolean</code> and enforce no type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtrait <a href="TripleEquals.html"><code>TripleEquals</code></a> and overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)

  /**
   * Convert to a <a href="LegacyEqualizer.html"><code>LegacyEqualizer</code></a> that provides <code>===</code> and <code>!==</code> operators that
   * result in <code>Option[String]</code> and enforce no type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtrait <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a> and overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>LegacyEqualizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T] = new LegacyEqualizer(left)

  /**
   * Convert to an <a href="CheckingEqualizer.html"><code>CheckingEqualizer</code></a> that provides <code>===</code> and <code>!==</code> operators that result in <code>Boolean</code> and enforce a type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtraits <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>, and overriden as
   * non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>CheckingEqualizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  /**
   * Convert to a <a href="LegacyCheckingEqualizer.html"><code>LegacyCheckingEqualizer</code></a> that provides <code>===</code> and <code>!==</code> operators that result in <code>Option[String]</code> and
   * enforce a type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtraits <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a> and <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>LegacyCheckingEqualizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T] = new LegacyCheckingEqualizer(left)

  /**
   * 
   */
  def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  def lowPriorityTypeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: A <:< B): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
  def typeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: B <:< A): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  def lowPriorityConversionCheckedEqualityConstraint[A, B](implicit equalityOfB: Equality[B], cnv: A => B): EqualityConstraint[A, B] = new AToBEqualityConstraint[A, B](equalityOfB, cnv)
  def conversionCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], cnv: B => A): EqualityConstraint[A, B] = new BToAEqualityConstraint[A, B](equalityOfA, cnv)

  def ===[T](right: T): TripleEqualsInvocation[T] = new TripleEqualsInvocation[T](right, true)
  def !==[T](right: T): TripleEqualsInvocation[T] = new TripleEqualsInvocation[T](right, false)

  def ===(right: Null): TripleEqualsInvocation[Null] = new TripleEqualsInvocation[Null](right, true)
  def !==(right: Null): TripleEqualsInvocation[Null] = new TripleEqualsInvocation[Null](right, false)

  def ===[T](right: Interval[T]): TripleEqualsInvocationOnInterval[T] = new TripleEqualsInvocationOnInterval[T](right, true)
  def !==[T](right: Interval[T]): TripleEqualsInvocationOnInterval[T] = new TripleEqualsInvocationOnInterval[T](right, false)
}

