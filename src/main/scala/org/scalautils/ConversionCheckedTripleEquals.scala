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

trait LowPriorityConversionCheckedConstraint extends EqualityConstraints {
  implicit override def lowPriorityConversionCheckedEqualityConstraint[A, B](implicit equalityOfB: Equality[B], cnv: A => B): EqualityConstraint[A, B] = new AToBEqualityConstraint[A, B](equalityOfB, cnv)
}

/**
 * Trait that enables a type constraint at uses of the <code>===</code> operator requiring that the two types being compared
 * with <code>===</code> either be in a subtype/supertype relationship, or that an implicit conversion
 * is available that can convert from one type to the other.
 *
 * <p>
 * This trait is the middle ground of the three <em>triple equals</em> traits, in between
 * <code>TripleEquals</code>, the most lenient, and <code>TypeCheckedTripleEquals</code>, the most strict.
 * If <code>TripleEquals</code> is mixed in or imported, the <code>===</code> can be used with any two types
 * and still compile. If <code>TypeCheckedTripleEquals</code> is mixed in or imported, however, only types in 
 * a subtype or supertype relationship with each other (including when both types are exactly the same) will compile.
 * <code>ConversionCheckedTripleEquals</code> is slightly more accomodating, because in addition to compiling any
 * use of <code>===</code> that will compile under</code>TypeCheckedTripleEquals</code>, it will also compile
 * type types that would be rejected by <code>TypeCheckedTripleEquals</code>, so long as their is an implicit
 * conversion (in either direction) from one type to another.
 * </p>
 *
 * <p>
 * For example, under <code>TypeCheckedTripleEquals</code>, the following use of <code>===</code> will not compile,
 * because <code>Int<code> and <code>Long</code> are not in a subtype/supertype relationship. (<em>I.e.</em>, <code>Int</code>
 * is not a subtype or supertype of <code>Long</code>):
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 * 
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * &lt;console&gt;:14: error: types Int and Long do not adhere to the equality constraint selected for the === and !== operators; they must either be in a subtype/supertype relationship, or, if ConversionCheckedTripleEquals is in force, implicitly convertible in one direction or the other; the missing implicit parameter is of type org.scalautils.EqualityConstraint[Int,Long]
 *               1 === 1L
 *                 ^
 * </pre>
 *
 * <p>
 * Trait <code>TypeCheckedTripleEquals</code> rejects types <code>Int</code> and <code>Long</code> because they are not directly related via
 * subtyping. However, an implicit widening conversion from <code>Int</code> to </code>Long</code> does exist, so <code>ConversionCheckedTripleEquals</code>
 * will allow it:
 * </p>
 * 
 * <pre class="stHighlight">
 * scala&gt; import ConversionCheckedTripleEquals._
 * import ConversionCheckedTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * res1: Boolean = true
 * </pre>
 * 
 * <p>
 * The implicit conversion can go in either direction: from the left type to the right type, or vice versa. In the above expression the 
 * implicit conversion goes from left to right (the <code>Int</code> on the left to the <code>Long</code> on the right). It also works
 * the other way:
 * </p>
 * 
 * <pre class="stHighlight">
 * scala&gt; 1L === 1
 * res2: Boolean = true
 * </pre>
 * 
 */
trait ConversionCheckedTripleEquals extends LowPriorityConversionCheckedConstraint with AsAny {

  override def convertToAsAnyWrapper(o: Any): AsAnyWrapper = super.convertToAsAnyWrapper(o)

  implicit override def defaultEquality[A]: Equality[A] = new DefaultEquality[A]

  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  implicit override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T] = new LegacyEqualizer(left)
  override def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T] = new LegacyCheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  override def lowPriorityTypeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: A <:< B): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
  override def typeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: B <:< A): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  implicit override def conversionCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], cnv: B => A): EqualityConstraint[A, B] = new BToAEqualityConstraint[A, B](equalityOfA, cnv)
}

/**
 * Companion object to trait <code>ConversionCheckedTripleEquals</code> that facilitates the importing of <code>ConversionCheckedTripleEquals</code> members as 
 * an alternative to mixing it in. One use case is to import <code>ConversionCheckedTripleEquals</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalautils.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 * 
 * scala&gt; import ConversionCheckedTripleEquals._
 * import ConversionCheckedTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * res0: Boolean = true
 * </pre>
 */
object ConversionCheckedTripleEquals extends ConversionCheckedTripleEquals

