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
package org.scalatest

// TODO: Use this Helper in the matchers/ClassicMatchers.scala

import org.scalatest._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import Helper.transformOperatorChars
import scala.collection.Traversable
import Assertions.areEqualComparingArraysStructurally
import org.scalatest.exceptions.TestFailedException
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalautils.Tolerance
import org.scalautils.Interval
import org.scalautils.TripleEqualsInvocation
import scala.annotation.tailrec
import org.scalautils.Equality
import org.scalatest.verb.ShouldVerb
import org.scalautils.TripleEqualsInvocationOnInterval
import org.scalautils.EqualityConstraint
import org.scalautils.AsAny
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.HavePropertyMatchResult
import org.scalatest.matchers.BePropertyMatcher
import org.scalatest.matchers.BePropertyMatchResult
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

// TODO: drop generic support for be as an equality comparison, in favor of specific ones.
// TODO: mention on JUnit and TestNG docs that you can now mix in ShouldMatchers or MustMatchers
// TODO: Put links from ShouldMatchers to wherever I reveal the matrix and algo of how properties are checked dynamically.
// TODO: double check that I wrote tests for (length (7)) and (size (8)) in parens
// TODO: document how to turn off the === implicit conversion
// TODO: Document you can use JMock, EasyMock, etc.

private[scalatest] object Helper {

  // If the symbol passed is 'title, this will look for a field named "title", a method named "title", or a
  // method named "getTitle". The method must take no parameters.
  //
  // F (field) | M (method) | G (get or is method) | Result
  // 0           0            0                      None
  // 0           0            1                      Some(G)
  // 0           1            0                      Some(M)
  // 0           1            1                      Some(M) prefer a Scala style one of a Java style, such as when using BeanProperty annotation
  // 1           0            0                      Some(F) ignore the field if there's a method. in Java often name a field and get method the same
  // 1           0            1                      Some(G)
  // 1           1            0                      Some(M)
  // 1           1            1                      Some(M) prefer a Scala style one of a Java style, such as when using BeanProperty annotation
  // 
  def accessProperty(objectWithProperty: AnyRef, propertySymbol: Symbol, isBooleanProperty: Boolean): Option[Any] = {

    // If 'title passed, propertyName would be "title"
    val propertyName = propertySymbol.name

    // if propertyName is '>, mangledPropertyName would be "$greater"
    val mangledPropertyName = transformOperatorChars(propertyName)

    // fieldNameToAccess and methodNameToInvoke would also be "title"
    val fieldNameToAccess = mangledPropertyName
    val methodNameToInvoke = mangledPropertyName

    // methodNameToInvokeWithGet would be "getTitle"
    val prefix = if (isBooleanProperty) "is" else "get"
    val methodNameToInvokeWithGet = prefix + mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

    val firstChar = propertyName(0).toLower
    val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
      firstChar == 'o' || firstChar == 'u'

    def isFieldToAccess(field: Field): Boolean = field.getName == fieldNameToAccess

    // If it is a predicate, I check the result type, otherwise I don't. Maybe I should just do that. Could be a later enhancement.
    def isMethodToInvoke(method: Method): Boolean =
      method.getName == methodNameToInvoke && method.getParameterTypes.length == 0 && !Modifier.isStatic(method.getModifiers()) &&
        (!isBooleanProperty || method.getReturnType == classOf[Boolean])

    def isGetMethodToInvoke(method: Method): Boolean =
      method.getName == methodNameToInvokeWithGet && method.getParameterTypes.length == 0 && !Modifier.isStatic(method.getModifiers()) &&
        (!isBooleanProperty || method.getReturnType == classOf[Boolean])

    val fieldOption = objectWithProperty.getClass.getFields.find(isFieldToAccess)

    val methodOption = objectWithProperty.getClass.getMethods.find(isMethodToInvoke)

    val getMethodOption = objectWithProperty.getClass.getMethods.find(isGetMethodToInvoke)

    (fieldOption, methodOption, getMethodOption) match {

      case (_, Some(method), _) => Some(method.invoke(objectWithProperty, Array[AnyRef](): _*))

      case (_, None, Some(getMethod)) => Some(getMethod.invoke(objectWithProperty, Array[AnyRef](): _*))

      case (Some(field), None, None) => Some(field.get(objectWithProperty))

      case (None, None, None) => None
    }
  }

  def transformOperatorChars(s: String): String = {
    val builder = new StringBuilder
    for (i <- 0 until s.length) {
      val ch = s.charAt(i)
      val replacement =
        ch match {
          case '!' => "$bang"
          case '#' => "$hash"
          case '~' => "$tilde"
          case '|' => "$bar"
          case '^' => "$up"
          case '\\' => "$bslash"
          case '@' => "$at"
          case '?' => "$qmark"
          case '>' => "$greater"
          case '=' => "$eq"
          case '<' => "$less"
          case ':' => "$colon"
          case '/' => "$div"
          case '-' => "$minus"
          case '+' => "$plus"
          case '*' => "$times"
          case '&' => "$amp"
          case '%' => "$percent"
          case _ => ""
        }

      if (replacement.length > 0)
        builder.append(replacement)
      else
        builder.append(ch)
    }
    builder.toString
  }
}

import Helper.accessProperty

/**
 * Trait that provides a domain specific language (DSL) for expressing assertions in tests
 * using the word <code>should</code>. For example, if you mix <code>Matchers</code> into
 * a suite class, you can write an equality assertion in that suite like this:
 * 
 * <pre class="stHighlight">
 * result should equal (3)
 * </pre>
 * 
 * <p>
 * Here <code>result</code> is a variable, and can be of any type. If the object is an
 * <code>Int</code> with the value 3, execution will continue (<em>i.e.</em>, the expression will result
 * in the unit value, <code>()</code>). Otherwise, a <code>TestFailedException</code>
 * will be thrown with a detail message that explains the problem, such as <code>"7 did not equal 3"</code>.
 * This <code>TestFailedException</code> will cause the test to fail.
 * </p>
 * 
 * <p>
 * The <code>left should equal (right)</code> syntax works by calling <code>==</code>  on the <code>left</code>
 * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
 * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
 * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
 * </p>
 * 
 * <pre class="stHighlight">
 * Array(1, 2) == Array(1, 2) // yields false
 * </pre>
 *
 * <p>
 * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest compares
 * the two arrays structurally, taking into consideration the equality of the array's contents:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
 * <code>be theSameInstanceAs</code> syntax, described below.
 * </p>
 *
 * <h2>Checking size and length</h2>
 * 
 * <p>
 * You can check the size or length of just about any type of object for which it
 * would make sense. Here's how checking for length looks:
 * </p>
 * <pre class="stHighlight">
 * result should have length (3)
 * </pre>
 * 
 * <p>
 * Size is similar:
 * </p>
 * 
 * <pre class="stHighlight">
 * result should have size (10)
 * </pre>
 * 
 * <p>
 * The <code>length</code> syntax can be used with <code>String</code>, <code>Array</code>, any <code>scala.collection.GenSeq</code>,
 * any <code>java.util.List</code>, and any type <code>T</code> for which an implicit <code>Length[T]</code> type class is 
 * available in scope.
 * Similarly, the <code>size</code> syntax can be used with <code>Array</code>, any <code>scala.collection.GenTraversable</code>,
 * any <code>java.util.List</code>, and any type <code>T</code> for which an implicit <code>Size[T]</code> type class is 
 * available in scope. You can enable the <code>length</code> or <code>size</code> syntax for your own arbitrary types, therefore,
 * by defining <a href="ClassicMatchers$Length.html"><code>Length</code></a> or <a href="ClassicMatchers$Size.html"><code>Size</code></a> type
 * classes for those types.
 * </p>
 * 
 * <h2>Checking strings</h2>
 * 
 * <p>
 * You can check for whether a string starts with, ends with, or includes a substring like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * string should startWith ("Hello")
 * string should endWith ("world")
 * string should include ("seven")
 * </pre>
 * 
 * <p>
 * You can check for whether a string starts with, ends with, or includes a regular expression, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * string should startWith regex ("Hel*o")
 * string should endWith regex ("wo.ld")
 * string should include regex ("wo.ld")
 * </pre>
 * 
 * <p>
 * And you can check whether a string fully matches a regular expression, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * string should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
 * </pre>
 * 
 * <p>
 * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
 * or a <code>scala.util.matching.Regex</code>.
 * </p>
 *
 * <h2>Greater and less than</h2>
 * <p>
 * You can check whether any type that is, or can be implicitly converted to,
 * an <code>Ordered[T]</code> is greater than, less than, greater than or equal, or less
 * than or equal to a value of type <code>T</code>. The syntax is:
 * </p>
 * <pre class="stHighlight">
 * one should be < (7)
 * one should be > (0)
 * one should be <= (7)
 * one should be >= (0)
 * </pre>
 * 
 * <h2>Checking equality with <code>be</code> <code>=</code><code>=</code><code>=</code></h2>
 *
 * <p>
 * An alternate way to check for equality of two objects is to use <code>be</code> with
 * <code>===</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * result should be === (3)
 * </pre>
 *
 * <p>
 * Here <code>result</code> is a variable, and can be of any type. If the object is an
 * <code>Int</code> with the value 3, execution will continue (<em>i.e.</em>, the expression will result
 * in the unit value, <code>()</code>). Otherwise, a <code>TestFailedException</code>
 * will be thrown with a detail message that explains the problem, such as <code>"7 was not equal to 3"</code>.
 * This <code>TestFailedException</code> will cause the test to fail.
 * </p>
 *
 * <p>
 * The <code>left should be === (right)</code> syntax works by calling <code>==</code>  on the <code>left</code>
 * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
 * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
 * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) == Array(1, 2) // yields false
 * </pre>
 *
 * <p>
 * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest compares
 * the two arrays structurally, taking into consideration the equality of the array's contents:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should be === (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
 * <code>be theSameInstanceAs</code> syntax, described below.
 * </p>
 *
 * <h2>Checking <code>Boolean</code> properties with <code>be</code></h2>
 * 
 * <p>
 * If an object has a method that takes no parameters and returns boolean, you can check
 * it by placing a <code>Symbol</code> (after <code>be</code>) that specifies the name
 * of the method (excluding an optional prefix of "<code>is</code>"). A symbol literal
 * in Scala begins with a tick mark and ends at the first non-identifier character. Thus,
 * <code>'empty</code> results in a <code>Symbol</code> object at runtime, as does
 * <code>'defined</code> and <code>'file</code>. Here's an example:
 * </p>
 * 
 * <pre class="stHighlight">
 * emptySet should be ('empty)
 * </pre>
 * 
 * Given this code, ScalaTest will use reflection to look on the object referenced from
 * <code>emptySet</code> for a method that takes no parameters and results in <code>Boolean</code>,
 * with either the name <code>empty</code> or <code>isEmpty</code>. If found, it will invoke
 * that method. If the method returns <code>true</code>, execution will continue. But if it returns
 * <code>false</code>, a <code>TestFailedException</code> will be thrown that will contain a detail message, such as:
 * 
 * <pre class="stHighlight">
 * Set(1, 2, 3) was not empty
 * </pre>
 * 
 * <p>
 * This <code>be</code> syntax can be used with any type.  If the object does
 * not have an appropriately named predicate method, you'll get a <code>TestFailedException</code>
 * at runtime with a detail message that explains the problem.
 * (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="Matchers$BeWord.html"><code>BeWord</code></a>.)
 * </p>
 * 
 * <p>
 * If you think it reads better, you can optionally put <code>a</code> or <code>an</code> after
 * <code>be</code>. For example, <code>java.io.File</code> has two predicate methods,
 * <code>isFile</code> and <code>isDirectory</code>. Thus with a <code>File</code> object
 * named <code>temp</code>, you could write:
 * </p>
 * 
 * <pre class="stHighlight">
 * temp should be a ('file)
 * </pre>
 * 
 * <p>
 * Or, given <code>java.awt.event.KeyEvent</code> has a method <code>isActionKey</code> that takes
 * no arguments and returns <code>Boolean</code>, you could assert that a <code>KeyEvent</code> is
 * an action key with:
 *</p>
 *
 * <pre class="stHighlight">
 * keyEvent should be an ('actionKey)
 * </pre>
 * 
 * <p>
 * If you prefer to check <code>Boolean</code> properties in a type-safe manner, you can use a <code>BePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="stHighlight">
 * emptySet should be (empty)
 * temp should be a (file)
 * keyEvent should be an (actionKey)
 * </pre>
 * 
 * <p>
 * These expressions would fail to compile if <code>should</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>BePropertyMatcher</code> being used. (For example, <code>file</code> in this example
 * would likely be of type <code>BePropertyMatcher[java.io.File]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the <code>Boolean</code> property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <h2>Using custom <code>BeMatchers</code></h2>
 *
 * If you want to create a new way of using <code>be</code>, which doesn't map to an actual property on the
 * type you care about, you can create a <code>BeMatcher</code>. You could use this, for example, to create <code>BeMatcher[Int]</code>
 * called <code>odd</code>, which would match any odd <code>Int</code>, and <code>even</code>, which would match
 * any even <code>Int</code>. 
 * Given this pair of <code>BeMatcher</code>s, you could check whether an <code>Int</code> was odd or even with expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * num should be (odd)
 * num should not be (even)
 * </pre>
 *
 * For more information, see the documentation for <a href="BeMatcher.html"><code>BeMatcher</code></a>.
 *
 * <h2>Checking object identity</h2>
 * 
 * <p>
 * If you need to check that two references refer to the exact same object, you can write:
 * </p>
 * 
 * <pre class="stHighlight">
 * ref1 should be theSameInstanceAs (ref2)
 * </pre>
 * 
 * <h2>Checking numbers against a range</h2>
 * 
 * <p>
 * To check whether a floating point number has a value that exactly matches another, you
 * can use <code>should equal</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * sevenDotOh should equal (7.0)
 * </pre>
 * 
 * <p>
 * Often, however, you may want to check whether a floating point number is within a
 * range. You can do that using <code>be</code> and <code>plusOrMinus</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * sevenDotOh should be (6.9 plusOrMinus 0.2)
 * </pre>
 * 
 * <p>
 * This expression will cause a <code>TestFailedException</code> to be thrown if the floating point
 * value, <code>sevenDotOh</code> is outside the range <code>6.7</code> to <code>7.1</code>.
 * You can also use <code>plusOrMinus</code> with integral types, for example:
 * </p>
 * 
 * <pre class="stHighlight">
 * seven should be (6 plusOrMinus 2)
 * </pre>
 * 
 * <h2>Traversables, iterables, sets, sequences, and maps</h2>
 * 
 * <p>
 * You can use some of the syntax shown previously with <code>Iterable</code> and its
 * subtypes. For example, you can check whether an <code>Iterable</code> is <code>empty</code>,
 * like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * iterable should be ('empty)
 * </pre>
 * 
 * <p>
 * You can check the length of an <code>Seq</code> (<code>Array</code>, <code>List</code>, etc.),
 * like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * array should have length (3)
 * list should have length (9)
 * </pre>
 * 
 * <p>
 * You can check the size of any <code>Traversable</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * map should have size (20)
 * set should have size (90)
 * </pre>
 * 
 * <p>
 * In addition, you can check whether an <code>Iterable</code> contains a particular
 * element, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * iterable should contain ("five")
 * </pre>
 * 
 * <p>
 * You can also check whether a <code>Map</code> contains a particular key, or value, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * map should contain key (1)
 * map should contain value ("Howdy")
 * </pre>
 * 
 * <h2>Java collections and maps</h2>
 * 
 * <p>
 * You can use similar syntax on Java collections (<code>java.util.Collection</code>) and maps (<code>java.util.Map</code>).
 * For example, you can check whether a Java <code>Collection</code> or <code>Map</code> is <code>empty</code>,
 * like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaCollection should be ('empty)
 * javaMap should be ('empty)
 * </pre>
 * 
 * <p>
 * Even though Java's <code>List</code> type doesn't actually have a <code>length</code> or <code>getLength</code> method,
 * you can nevertheless check the length of a Java <code>List</code> (<code>java.util.List</code>) like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaList should have length (9)
 * </pre>
 * 
 * <p>
 * You can check the size of any Java <code>Collection</code> or <code>Map</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaMap should have size (20)
 * javaSet should have size (90)
 * </pre>
 * 
 * <p>
 * In addition, you can check whether a Java <code>Collection</code> contains a particular
 * element, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaCollection should contain ("five")
 * </pre>
 * 
 * <p>
 * One difference to note between the syntax supported on Java collections and that of Scala
 * iterables is that you can't use <code>contain (...)</code> syntax with a Java <code>Map</code>.
 * Java differs from Scala in that its <code>Map</code> is not a subtype of its <code>Collection</code> type.
 * If you want to check that a Java <code>Map</code> contains a specific key/value pair, the best approach is
 * to invoke <code>entrySet</code> on the Java <code>Map</code> and check that entry set for the appropriate
 * element (a <code>java.util.Map.Entry</code>) using <code>contain (...)</code>.
 * </p>
 *
 * <p>
 * Despite this difference, the other (more commonly used) map matcher syntax works just fine on Java <code>Map</code>s.
 * You can, for example, check whether a Java <code>Map</code> contains a particular key, or value, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaMap should contain key (1)
 * javaMap should contain value ("Howdy")
 * </pre>
 * 
 * <h2>Be as an equality comparison</h2>
 * 
 * <p>
 * All uses of <code>be</code> other than those shown previously perform an equality comparison. In other words, they work
 * the same as <code>equals</code>. This redundance between <code>be</code> and <code>equals</code> exists because it enables syntax
 * that sometimes sounds more natural. For example, instead of writing: 
 * </p>
 * 
 * <pre class="stHighlight">
 * result should equal (null)
 * </pre>
 * 
 * <p>
 * You can write:
 * </p>
 * 
 * <pre class="stHighlight">
 * result should be (null)
 * </pre>
 * 
 * <p>
 * (Hopefully you won't write that too much given <code>null</code> is error prone, and <code>Option</code>
 * is usually a better, well, option.) 
 * Here are some other examples of <code>be</code> used for equality comparison:
 * </p>
 * 
 * <pre class="stHighlight">
 * sum should be (7.0)
 * boring should be (false)
 * fun should be (true)
 * list should be (Nil)
 * option should be (None)
 * option should be (Some(1))
 * </pre>
 * 
 * <p>
 * As with <code>equal</code>, using <code>be</code> on two arrays results in <code>deep</code> being called on both arrays prior to
 * calling <code>equal</code>. As a result,
 * the following expression would <em>not</em> throw a <code>TestFailedException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should be (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * Because <code>be</code> is used in several ways in ScalaTest matcher syntax, just as it is used in many ways in English, one
 * potential point of confusion in the event of a failure is determining whether <code>be</code> was being used as an equality comparison or
 * in some other way, such as a property assertion. To make it more obvious when <code>be</code> is being used for equality, the failure
 * messages generated for those equality checks will include the word <code>equal</code> in them. For example, if this expression fails with a
 * <code>TestFailedException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * option should be (Some(1))
 * </pre>
 *
 * <p>
 * The detail message in that <code>TestFailedException</code> will include the words <code>"equal to"</code> to signify <code>be</code>
 * was in this case being used for equality comparison:
 * </p>
 *
 * <pre class="stHighlight">
 * Some(2) was not equal to Some(1)
 * </pre>
 *
 * <h2>Being negative</h2>
 * 
 * <p>
 * If you wish to check the opposite of some condition, you can simply insert <code>not</code> in the expression.
 * Here are a few examples:
 * </p>
 * 
 * <pre class="stHighlight">
 * result should not be (null)
 * sum should not be <= (10)
 * mylist should not equal (yourList)
 * string should not startWith ("Hello")
 * </pre>
 * 
 * <h2>Logical expressions with <code>and</code> and <code>or</code></h2>
 * 
 * <p>
 * You can also combine matcher expressions with <code>and</code> and/or <code>or</code>, however,
 * you must place parentheses or curly braces around the <code>and</code> or <code>or</code> expression. For example, 
 * this <code>and</code>-expression would not compile, because the parentheses are missing:
 * </p>
 * 
 * <pre class="stHighlight">
 * map should contain key ("two") and not contain value (7) // ERROR, parentheses missing!
 * </pre>
 * 
 * <p>
 * Instead, you need to write:
 * </p>
 * 
 * <pre class="stHighlight">
 * map should (contain key ("two") and not contain value (7))
 * </pre>
 * 
 * <p>
 * Here are some more examples:
 * </p>
 * 
 * <pre class="stHighlight">
 * number should (be > (0) and be <= (10))
 * option should (equal (Some(List(1, 2, 3))) or be (None))
 * string should (
 *   equal ("fee") or
 *   equal ("fie") or
 *   equal ("foe") or
 *   equal ("fum")
 * )
 * </pre>
 * 
 * <p>
 * Two differences exist between expressions composed of these <code>and</code> and <code>or</code> operators and the expressions you can write
 * on regular <code>Boolean</code>s using its <code>&&</code> and <code>||</code> operators. First, expressions with <code>and</code>
 * and <code>or</code> do not short-circuit. The following contrived expression, for example, would print <code>"hello, world!"</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "yellow" should (equal ("blue") and equal { println("hello, world!"); "green" })
 * </pre>
 * 
 * <p>
 * In other words, the entire <code>and</code> or <code>or</code> expression is always evaluated, so you'll see any side effects
 * of the right-hand side even if evaluating
 * only the left-hand side is enough to determine the ultimate result of the larger expression. Failure messages produced by these
 * expressions will "short-circuit," however,
 * mentioning only the left-hand side if that's enough to determine the result of the entire expression. This "short-circuiting" behavior
 * of failure messages is intended
 * to make it easier and quicker for you to ascertain which part of the expression caused the failure. The failure message for the previous
 * expression, for example, would be:
 * </p>
 * 
 * <pre class="stHighlight">
 * "yellow" did not equal "blue"
 * </pre>
 * 
 * <p>
 * Most likely this lack of short-circuiting would rarely be noticeable, because evaluating the right hand side will usually not
 * involve a side effect. One situation where it might show up, however, is if you attempt to <code>and</code> a <code>null</code> check on a variable with an expression
 * that uses the variable, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * map should (not be (null) and contain key ("ouch"))
 * </pre>
 * 
 * <p>
 * If <code>map</code> is <code>null</code>, the test will indeed fail, but with a <code>NullPointerException</code>, not a
 * <code>TestFailedException</code>. Here, the <code>NullPointerException</code> is the visible right-hand side effect. To get a
 * <code>TestFailedException</code>, you would need to check each assertion separately:
 * </p>
 *
 * <pre class="stHighlight">
 * map should not be (null)
 * map should contain key ("ouch")
 * </pre>
 * 
 * <p>
 * If <code>map</code> is <code>null</code> in this case, the <code>null</code> check in the first expression will fail with
 * a <code>TestFailedException</code>, and the second expression will never be executed.
 * </p>
 *
 * <p>
 * The other difference with <code>Boolean</code> operators is that although <code>&&</code> has a higher precedence than <code>||</code>,
 * <code>and</code> and <code>or</code>
 * have the same precedence. Thus although the <code>Boolean</code> expression <code>(a || b && c)</code> will evaluate the <code>&&</code> expression
 * before the <code>||</code> expression, like <code>(a || (b && c))</code>, the following expression:
 * </p>
 * 
 * <pre class="stHighlight">
 * traversable should (contain (7) or contain (8) and have size (9))
 * </pre>
 * 
 * <p>
 * Will evaluate left to right, as:
 * </p>
 * 
 * <pre class="stHighlight">
 * traversable should ((contain (7) or contain (8)) and have size (9))
 * </pre>
 * 
 * <p>
 * If you really want the <code>and</code> part to be evaluated first, you'll need to put in parentheses, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * traversable should (contain (7) or (contain (8) and have size (9)))
 * </pre>
 * 
 * <h2>Working with <code>Option</code>s</h2>
 * 
 * <p>
 * ScalaTest matchers has no special support for <code>Option</code>s, but you can 
 * work with them quite easily using syntax shown previously. For example, if you wish to check
 * whether an option is <code>None</code>, you can write any of:
 * </p>
 * 
 * <pre class="stHighlight">
 * option should equal (None)
 * option should be (None)
 * option should not be ('defined)
 * option should be ('empty)
 * </pre>
 * 
 * <p>
 * If you wish to check an option is defined, and holds a specific value, you can write either of:
 * </p>
 * 
 * <pre class="stHighlight">
 * option should equal (Some("hi"))
 * option should be (Some("hi"))
 * </pre>
 * 
 * <p>
 * If you only wish to check that an option is defined, but don't care what it's value is, you can write:
 * </p>
 * 
 * <pre class="stHighlight">
 * option should be ('defined)
 * </pre>
 * 
 * <p>
 * If you mix in (or import the members of) <a href="../OptionValues.html"><code>OptionValues</code></a>,
 * you can write one statement that indicates you believe an option should be defined and then say something else about its value. Here's an example:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.OptionValues._
 * option.value should be &lt; (7)
 * </pre>
 * 
 * <h2>Checking arbitrary properties with <code>have</code></h2>
 * 
 * <p>
 * Using <code>have</code>, you can check properties of any type, where a <em>property</em> is an attribute of any
 * object that can be retrieved either by a public field, method, or JavaBean-style <code>get</code>
 * or <code>is</code> method, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * book should have (
 *   'title ("Programming in Scala"),
 *   'author (List("Odersky", "Spoon", "Venners")),
 *   'pubYear (2008)
 * )
 * </pre>
 * 
 * <p>
 * This expression will use reflection to ensure the <code>title</code>, <code>author</code>, and <code>pubYear</code> properties of object <code>book</code>
 * are equal to the specified values. For example, it will ensure that <code>book</code> has either a public Java field or method
 * named <code>title</code>, or a public method named <code>getTitle</code>, that when invoked (or accessed in the field case) results
 * in a the string <code>"Programming in Scala"</code>. If all specified properties exist and have their expected values, respectively,
 * execution will continue. If one or more of the properties either does not exist, or exists but results in an unexpected value,
 * a <code>TestFailedException</code> will be thrown that explains the problem. (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="Matchers$HavePropertyMatcherGenerator.html"><code>HavePropertyMatcherGenerator</code></a>.)
 * </p>
 * 
 * <p>
 * When you use this syntax, you must place one or more property values in parentheses after <code>have</code>, seperated by commas, where a <em>property
 * value</em> is a symbol indicating the name of the property followed by the expected value in parentheses. The only exceptions to this rule is the syntax
 * for checking size and length shown previously, which does not require parentheses. If you forget and put parentheses in, however, everything will
 * still work as you'd expect. Thus instead of writing:
 * </p>
 *
 * <pre class="stHighlight">
 * array should have length (3)
 * set should have size (90)
 * </pre>
 * 
 * <p>
 * You can alternatively, write:
 * </p>
 *
 * <pre class="stHighlight">
 * array should have (length (3))
 * set should have (size (90))
 * </pre>
 * 
 * <p>
 * If a property has a value different from the specified expected value, a <code>TestFailedError</code> will be thrown
 * with a detail message that explains the problem. For example, if you assert the following on
 * a <code>book</code> whose title is <code>Moby Dick</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have ('title ("A Tale of Two Cities"))
 * </pre>
 *
 * <p>
 * You'll get a <code>TestFailedException</code> with this detail message:
 * </p>
 *
 * <pre>
 * The title property had value "Moby Dick", instead of its expected value "A Tale of Two Cities",
 * on object Book("Moby Dick", "Melville", 1851)
 * </pre>
 * 
 * <p>
 * If you prefer to check properties in a type-safe manner, you can use a <code>HavePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have (
 *   title ("Programming in Scala"),
 *   author (List("Odersky", "Spoon", "Venners")),
 *   pubYear (2008)
 * )
 * </pre>
 * 
 * <p>
 * These expressions would fail to compile if <code>should</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>HavePropertyMatcher</code> being used. (For example, <code>title</code> in this example
 * might be of type <code>HavePropertyMatcher[org.publiclibrary.Book]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="HavePropertyMatcher.html"><code>HavePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <h2>Using custom matchers</h2>
 * 
 * <p>
 * If none of the built-in matcher syntax (or options shown so far for extending the syntax) satisfy a particular need you have, you can create
 * custom <code>Matcher</code>s that allow
 * you to place your own syntax directly after <code>should</code>. For example, class <code>java.io.File</code> has a method <code>exists</code>, which
 * indicates whether a file of a certain path and name exists. Because the <code>exists</code> method takes no parameters and returns <code>Boolean</code>,
 * you can call it using <code>be</code> with a symbol or <code>BePropertyMatcher</code>, yielding assertions like:
 * </p>
 * 
 * <pre class="stHighlight">
 * file should be ('exists)  // using a symbol
 * file should be (inExistance)   // using a BePropertyMatcher
 * </pre>
 * 
 * <p>
 * Although these expressions will achieve your goal of throwing a <code>TestFailedException</code> if the file does not exist, they don't produce
 * the most readable code because the English is either incorrect or awkward. In this case, you might want to create a
 * custom <code>Matcher[java.io.File]</code>
 * named <code>exist</code>, which you could then use to write expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * // using a plain-old Matcher
 * file should exist
 * file should not (exist)
 * file should (exist and have ('name ("temp.txt")))
 * </pre>
 * 
 * <p>
 * Note that when you use custom <code>Matcher</code>s, you will need to put parentheses around the custom matcher in more cases than with
 * the built-in syntax. For example you will often need the parentheses after <code>not</code>, as shown above. (There's no penalty for
 * always surrounding custom matchers with parentheses, and if you ever leave them off when they are needed, you'll get a compiler error.)
 * For more information about how to create custom <code>Matcher</code>s, please see the documentation for the <a href="Matcher.html"><code>Matcher</code></a> trait.
 * </p>
 *
 * <h2>Checking for expected exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. With <code>Matchers</code> mixed in, you can
 * check for an expected exception like this:
 * </p>
 *
 * <pre class="stHighlight">
 * evaluating { s.charAt(-1) } should produce [IndexOutOfBoundsException]
 * </pre>
 *
 * <p>
 * If <code>charAt</code> throws an instance of <code>StringIndexOutOfBoundsException</code>,
 * this expression will result in that exception. But if <code>charAt</code> completes normally, or throws a different
 * exception, this expression will complete abruptly with a <code>TestFailedException</code>.
 * This expression returns the caught exception so that you can inspect it further if you wish, for
 * example, to ensure that data contained inside the exception has the expected values. Here's an
 * example:
 * </p>
 *
 * <pre class="stHighlight">
 * val thrown = evaluating { s.charAt(-1) } should produce [IndexOutOfBoundsException]
 * thrown.getMessage should equal ("String index out of range: -1")
 * </pre>
 *
 * <h2>Those pesky parens</h2>
 * 
 * <p>
 * Perhaps the most tricky part of writing assertions using ScalaTest matchers is remembering
 * when you need or don't need parentheses, but bearing in mind a few simple rules <!-- PRESERVE -->should help.
 * It is also reassuring to know that if you ever leave off a set of parentheses when they are
 * required, your code will not compile. Thus the compiler will help you remember when you need the parens.
 * That said, the rules are:
 * </p>
 *
 * <p>
 * 1. Although you don't always need them, it is recommended style to always put parentheses
 * around right-hand values, such as the <code>7</code> in <code>num should equal (7)</code>:
 * </p>
 *
 * <pre>
 * result should equal <span class="stRed">(</span>4<span class="stRed">)</span>
 * array should have length <span class="stRed">(</span>3<span class="stRed">)</span>
 * book should have (
 *   'title <span class="stRed">(</span>"Programming in Scala"<span class="stRed">)</span>,
 *   'author <span class="stRed">(</span>List("Odersky", "Spoon", "Venners")<span class="stRed">)</span>,
 *   'pubYear <span class="stRed">(</span>2008<span class="stRed">)</span>
 * )
 * option should be <span class="stRed">(</span>'defined<span class="stRed">)</span>
 * catMap should (contain key <span class="stRed">(</span>9<span class="stRed">)</span> and contain value <span class="stRed">(</span>"lives"<span class="stRed">)</span>)</span>
 * keyEvent should be an <span class="stRed">(</span>'actionKey<span class="stRed">)</span>
 * javaSet should have size <span class="stRed">(</span>90<span class="stRed">)</span>
 * </pre>
 *
 * <p>
 * 2. Except for <code>length</code> and <code>size</code>, you must always put parentheses around
 * the list of one or more property values following a <code>have</code>:
 * </p>
 *
 * <pre>
 * file should (exist and have <span class="stRed">(</span>'name ("temp.txt")<span class="stRed">)</span>)
 * book should have <span class="stRed">(</span>
 *   title ("Programming in Scala"),
 *   author (List("Odersky", "Spoon", "Venners")),
 *   pubYear (2008)
 * <span class="stRed">)</span>
 * javaList should have length (9) // parens optional for length and size
 * </pre>
 *
 * <p>
 * 3. You must always put parentheses around <code>and</code> and <code>or</code> expressions, as in:
 * </p>
 *
 * <pre>
 * catMap should <span class="stRed">(</span>contain key (9) and contain value ("lives")<span class="stRed">)</span>
 * number should <span class="stRed">(</span>equal (2) or equal (4) or equal (8)<span class="stRed">)</span>
 * </pre>
 * 
 * <p>
 * 4. Although you don't always need them, it is recommended style to always put parentheses
 * around custom <code>Matcher</code>s when they appear directly after <code>not</code>:
 * </p>
 * 
 * <pre>
 * file should exist
 * file should not <span class="stRed">(</span>exist<span class="stRed">)</span>
 * file should (exist and have ('name ("temp.txt")))
 * file should (not <span class="stRed">(</span>exist<span class="stRed">)</span> and have ('name ("temp.txt"))
 * file should (have ('name ("temp.txt") or exist)
 * file should (have ('name ("temp.txt") or not <span class="stRed">(</span>exist<span class="stRed">)</span>)
 * </pre>
 *
 * <p>
 * That's it. With a bit of practice it <!-- PRESERVE -->should become natural to you, and the compiler will always be there to tell you if you
 * forget a set of needed parentheses.
 * </p>
 */
trait Matchers extends Assertions with Tolerance with ShouldVerb with AsAny with LoneElement { matchers =>

  private[scalatest] def newTestFailedException(message: String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 0): Throwable = {
    val temp = new RuntimeException
    val stackDepth = temp.getStackTrace.indexWhere(_.getFileName != "Matchers.scala")
    optionalCause match {
      case Some(cause) => new TestFailedException(message, cause, stackDepth + stackDepthAdjustment)
      case None => new TestFailedException(message, stackDepth + stackDepthAdjustment)
    }
  }

  private[scalatest] def matchSymbolToPredicateMethod[S <: AnyRef](left: S, right: Symbol, hasArticle: Boolean, articleIsA: Boolean): MatchResult = {

    // If 'empty passed, rightNoTick would be "empty"
    val propertyName = right.name

    accessProperty(left, right, true) match {

      case None =>

        // if propertyName is '>, mangledPropertyName would be "$greater"
        val mangledPropertyName = transformOperatorChars(propertyName)

        // methodNameToInvoke would also be "empty"
        val methodNameToInvoke = mangledPropertyName

        // methodNameToInvokeWithIs would be "isEmpty"
        val methodNameToInvokeWithIs = "is"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

        val firstChar = propertyName(0).toLower
        val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
          firstChar == 'o' || firstChar == 'u'

        throw newTestFailedException(
          FailureMessages(
            if (methodNameStartsWithVowel) "hasNeitherAnOrAnMethod" else "hasNeitherAOrAnMethod",
            left,
            UnquotedString(methodNameToInvoke),
            UnquotedString(methodNameToInvokeWithIs)
          )
        )

      case Some(result) =>

        val (wasNot, was) =
          if (hasArticle) {
            if (articleIsA) ("wasNotA", "wasA") else ("wasNotAn", "wasAn")
          }
          else ("wasNot", "was")

        MatchResult(
          result == true, // Right now I just leave the return value of accessProperty as Any
          FailureMessages(wasNot, left, UnquotedString(propertyName)),
          FailureMessages(was, left, UnquotedString(propertyName))
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class MatcherWrapper[T](leftMatcher: Matcher[T]) { matchersWrapper =>

// TODO: mention not short circuited, and the precendence is even between and and or

    /**
     * Returns a matcher whose <code>apply</code> method returns a <code>MatchResult</code>
     * that represents the logical-and of the results of the wrapped and the passed matcher applied to
     * the same value.
     *
     * <p>
     * The reason <code>and</code> has an upper bound on its type parameter is so that the <code>Matcher</code>
     * resulting from an invocation of <code>and</code> will have the correct type parameter. If you call
     * <code>and</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Valencia]</code>,
     * the result will have type <code>Matcher[Valencia]</code>. This is correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Valencia]</code> know how to match a
     * <code>Valencia</code> (but a <code>Matcher[Valencia]</code> doesn't know how to
     * match any old <code>Orange</code>).  If you call
     * <code>and</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Fruit]</code>,
     * the result will have type <code>Matcher[Orange]</code>. This is also correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Fruit]</code> know how to match an
     * <code>Orange</code> (but a <code>Matcher[Orange]</code> doesn't know how to
     * match any old <code>Fruit</code>).
     * </p>
     *
     * @param the matcher to logical-and with this matcher
     * @return a matcher that performs the logical-and of this and the passed matcher
     */
    def and[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          andMatchersAndApply(left, leftMatcher, rightMatcher)
/*
          val leftMatchResult = leftMatcher(left)
          val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
          if (!leftMatchResult.matches)
            MatchResult(
              false,
              leftMatchResult.failureMessage,
              leftMatchResult.negatedFailureMessage,
              leftMatchResult.midSentenceFailureMessage,
              leftMatchResult.midSentenceNegatedFailureMessage
            )
          else {
            MatchResult(
              rightMatchResult.matches,
              Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
              Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
            )
          }
*/
        }
      }

    def and[U <: T, TYPECLASS[_]](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
      new MatcherGen1[U, TYPECLASS] {
        def matcher[V <: U : TYPECLASS]: Matcher[V] = {
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              val rightMatcher = rightMatcherGen1.matcher
              andMatchersAndApply(left, leftMatcher, rightMatcher)
/*
              val leftMatchResult = leftMatcher(left)
              val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
              if (!leftMatchResult.matches)
                MatchResult(
                  false,
                  leftMatchResult.failureMessage,
                  leftMatchResult.negatedFailureMessage,
                  leftMatchResult.midSentenceFailureMessage,
                  leftMatchResult.midSentenceNegatedFailureMessage
                )
              else {
                MatchResult(
                  rightMatchResult.matches,
                  Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
                  Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
                  Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
                  Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
                )
              }
*/
            }
          }
        }
      }

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndHaveWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have length (2) and have length (3 - 1))
       *                                              ^
       * </pre>
       */
      def length(expectedLength: Long): Matcher[T with AnyRef] = and(have.length(expectedLength))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have size (2) and have size (3 - 1))
       *                                            ^ 
       * </pre>
       */
      def size(expectedSize: Long): Matcher[T with AnyRef] = and(have.size(expectedSize))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                   ^ 
     * </pre>
     */
    def and(haveWord: HaveWord): AndHaveWord = new AndHaveWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndContainWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (contain (2) and contain (3 - 1))
       *                                     ^
       * </pre>
       */
      def apply[U](expectedElement: U): Matcher[T with GenTraversable[U]] = matchersWrapper.and(matchers.contain(expectedElement))
      // def element[T](expectedElement: T) = matchersWrapper.and(matchers.contain.apply(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
       *                                                                     ^
       * </pre>
       */
      def key[U](expectedElement: U): Matcher[T with scala.collection.GenMap[U, Any]] = matchersWrapper.and(matchers.contain.key(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain value (2) and contain value (1))
       *                                                                   ^
       * </pre>
       */
      def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = matchersWrapper.and(matchers.contain.value(expectedValue))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
     *                                                         ^ 
     * </pre>
     */
    def and(containWord: ContainWord): AndContainWord = new AndContainWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndBeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isFileMock should (be a ('file) and be a ('file))
       *                                        ^
       * </pre>
       */
      def a(symbol: Symbol): Matcher[T with AnyRef] = and(be.a(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (be a (file) and be a (file))
       *                                        ^
       * </pre>
       */
      def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(be.a(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isAppleMock should (be an ('apple) and be an ('apple))
       *                                           ^
       * </pre>
       */
      def an(symbol: Symbol): Matcher[T with AnyRef] = and(be.an(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isAppleMock should (be an (apple) and be an (apple))
       *                                           ^
       * </pre>
       */
      def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(be.an(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
       *                                                  ^
       * </pre>
       */
      def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = and(be.theSameInstanceAs(anyRef))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a ('file) and be a ('file))
     *                                 ^
     * </pre>
     */
    def and(beWord: BeWord): AndBeWord = new AndBeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndFullyMatchWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
       *                                                         ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(fullyMatch.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
       *                                                              ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(fullyMatch.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
     *                                               ^
     * </pre>
     */
    def and(fullyMatchWord: FullyMatchWord): AndFullyMatchWord = new AndFullyMatchWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndIncludeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex (decimal) and include regex (decimal))
       *                                                   ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(include.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
       *                                                        ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(include.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "hello, world" should (include regex ("hel*o") and include regex ("wor.d"))
     *                                           ^
     * </pre>
     */
    def and(includeWord: IncludeWord): AndIncludeWord = new AndIncludeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndStartWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex (decimal) and startWith regex (decimal))
       *                                                       ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(startWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
       *                                                            ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(startWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.78" should (have length (4) and startWith regex ("1.7"))
     *                                ^
     * </pre>
     */
    def and(startWithWord: StartWithWord): AndStartWithWord = new AndStartWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndEndWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex (decimal) and endWith regex (decimal))
       *                                                   ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = and(endWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
       *                                                        ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = and(endWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
     *                                            ^
     * </pre>
     */
    def and(endWithWord: EndWithWord): AndEndWithWord = new AndEndWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class AndNotWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not equal (2) and not equal (3 - 1))
       *                                 ^
       * </pre>
       */
      def equal(any: Any): Matcher[T] =
        matchersWrapper.and(matchers.not.apply(matchers.legacyEqual(any)))

      /**
       * This method enables the following syntax, for the "primitive" numeric types:
       *
       * <pre class="stHighlight">
       * sevenDotOh should (not equal (17.0 plusOrMinus 0.2) and not equal (17.0 plusOrMinus 0.2))
       *                                                         ^
       * </pre>
       */
      def equal[U](interval: Interval[U]): Matcher[T with U] = matchersWrapper.and(matchers.not.equal(interval))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * aNullRef should (not equal ("hi") and not equal (null))
       *                                   ^
       * </pre>
       */
      def equal(o: Null): Matcher[T] = {
        matchersWrapper and {
          new Matcher[T] {
            def apply(left: T): MatchResult = {
              MatchResult(
                left != null,
                FailureMessages("equaledNull"),
                FailureMessages("didNotEqualNull", left),
                FailureMessages("midSentenceEqualedNull"),
                FailureMessages("didNotEqualNull", left)
              )
            }
          }
        }
      }

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not be (2) and not be (3 - 1))
       *                              ^
       * </pre>
       */
      def be(any: Any): Matcher[T] =
        matchersWrapper.and(matchers.not.apply(matchers.be(any)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have size (5) and not have length (3))
       *                                               ^
       * </pre>
       */
      def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.and(matchers.not.apply(matchers.have.length(resultOfLengthWordApplication.expectedLength)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have size (5) and not have size (3))
       *                                               ^
       * </pre>
       */
      def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.and(matchers.not.apply(matchers.have.size(resultOfSizeWordApplication.expectedSize)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * book should (not have (title ("Moby Dick")) and not have (author ("Melville")))
       *                                                     ^
       * </pre>
       */
      def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
        matchersWrapper.and(matchers.not.apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be < (2) and not be < (6))
       *                                ^
       * </pre>
       */
      def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfLessThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * map should (contain key (7) and not be (null))
       *                                     ^
       * </pre>
       */
      def be(o: Null): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(o))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 7 should (not be > (8) and not be > (6))
       *                                ^
       * </pre>
       */
      def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfGreaterThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be <= (1) and not be <= (2))
       *                                 ^
       * </pre>
       */
      def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfLessThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 7 should (not be >= (8) and not be >= (6))
       *                                 ^
       * </pre>
       */
      def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.and(matchers.not.be(resultOfGreaterThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be === (2) and not be === (6))
       *                                  ^
       * </pre>
       */
      def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
        matchersWrapper.and(matchers.not.be(tripleEqualsInvocation))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * notEmptyMock should (not be ('empty) and not be ('empty))
       *                                              ^
       * </pre>
       */
      def be(symbol: Symbol): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be (odd) and not be (odd))
       *                                ^
       * </pre>
       */
      def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = matchersWrapper.and(matchers.not.be(beMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be (directory) and not be (directory))
       *                                              ^
       * </pre>
       */
      def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = matchersWrapper.and(matchers.not.be(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isNotFileMock should (not be a ('file) and not be a ('file))
       *                                                ^
       * </pre>
       */
      def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be a (directory) and not be a (directory))
       *                                             ^
       * </pre>
       */
      def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = matchersWrapper.and(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isNotAppleMock should (not be an ('apple) and not be an ('apple)) 
       *                                                   ^
       * </pre>
       */
      def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be an (directory) and not be an (directory))
       *                                              ^
       * </pre>
       */
      def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]) = matchersWrapper.and(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (otherString))
       *                                                            ^
       * </pre>
       */
      def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = matchersWrapper.and(matchers.not.be(resultOfTheSameInstanceAsApplication))

      /**
       * This method enables the following syntax, for the "primitive" numeric types:
       *
       * <pre class="stHighlight">
       * sevenDotOh should (not be (17.0 plusOrMinus 0.2) and not be (17.0 plusOrMinus 0.2))
       *                                                          ^
       * </pre>
       */
      def be[U](interval: Interval[U]): Matcher[T with U] = matchersWrapper.and(matchers.not.be(interval))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimal))
       *                                                     ^
       * </pre>
       */
      def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.fullyMatch(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include regex ("bob") and not include regex (decimal))
       *                                                     ^
       * </pre>
       */
      def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.include(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include ("bob") and not include ("1.7"))
       *                                            ^
       * </pre>
       */
      def include(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.and(matchers.not.include(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith regex ("bob") and not startWith regex (decimal))
       *                                                    ^
       * </pre>
       */
      def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.startWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith ("red") and not startWith ("1.7"))
       *                                              ^
       * </pre>
       */
      def startWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.and(matchers.not.startWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith regex ("bob") and not endWith regex (decimal))
       *                                                  ^
       * </pre>
       */
      def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.and(matchers.not.endWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith ("fre") and not endWith ("1.7"))
       *                                            ^
       * </pre>
       */
      def endWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.and(matchers.not.endWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not contain (5) and not contain (3))
       *                                                     ^
       * </pre>
       */
      def contain[U](expectedElement: U): Matcher[T with GenTraversable[U]] =
        matchersWrapper.and(matchers.not.contain(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain key ("five") and not contain key ("three"))
       *                                                                      ^
       * </pre>
       */
      def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
        matchersWrapper.and(matchers.not.contain(resultOfKeyWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
       *                                                                   ^
       * </pre>
       */
      def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
        matchersWrapper.and(matchers.not.contain(resultOfValueWordApplication))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
     *                                                           ^
     * </pre>
     */
    def and(notWord: NotWord): AndNotWord = new AndNotWord

    /**
     * Returns a matcher whose <code>apply</code> method returns a <code>MatchResult</code>
     * that represents the logical-or of the results of this and the passed matcher applied to
     * the same value.
     *
     * <p>
     * The reason <code>or</code> has an upper bound on its type parameter is so that the <code>Matcher</code>
     * resulting from an invocation of <code>or</code> will have the correct type parameter. If you call
     * <code>or</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Valencia]</code>,
     * the result will have type <code>Matcher[Valencia]</code>. This is correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Valencia]</code> know how to match a
     * <code>Valencia</code> (but a <code>Matcher[Valencia]</code> doesn't know how to
     * match any old <code>Orange</code>).  If you call
     * <code>or</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Fruit]</code>,
     * the result will have type <code>Matcher[Orange]</code>. This is also correct because both a
     * <code>Matcher[Orange]</code> and a <code>Matcher[Fruit]</code> know how to match an
     * <code>Orange</code> (but a <code>Matcher[Orange]</code> doesn't know how to
     * match any old <code>Fruit</code>).
     * </p>
     *
     * @param the matcher to logical-or with this matcher
     * @return a matcher that performs the logical-or of this and the passed matcher
     */
    def or[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          orMatchersAndApply(left, leftMatcher, rightMatcher)
/*
          val leftMatchResult = leftMatcher(left)
          val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
          if (leftMatchResult.matches)
            MatchResult(
              true,
              leftMatchResult.negatedFailureMessage,
              leftMatchResult.failureMessage,
              leftMatchResult.midSentenceNegatedFailureMessage,
              leftMatchResult.midSentenceFailureMessage
            )
          else {
            MatchResult(
              rightMatchResult.matches,
              Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
              Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceFailureMessage),
              Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
            )
          }
*/
        }
      }

    def or[U <: T, TYPECLASS[_]](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
      new MatcherGen1[U, TYPECLASS] {
        def matcher[V <: U : TYPECLASS]: Matcher[V] = {
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              val rightMatcher = rightMatcherGen1.matcher
              orMatchersAndApply(left, leftMatcher, rightMatcher)
/*
              val leftMatchResult = leftMatcher(left)
              val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
              if (leftMatchResult.matches)
                MatchResult(
                  true,
                  leftMatchResult.negatedFailureMessage,
                  leftMatchResult.failureMessage,
                  leftMatchResult.midSentenceNegatedFailureMessage,
                  leftMatchResult.midSentenceFailureMessage
                )
              else {
                MatchResult(
                  rightMatchResult.matches,
                  Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceFailureMessage),
                  Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
                  Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceFailureMessage),
                  Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
                )
              }
*/
            }
          }
        }
      }

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrHaveWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have length (2) and have length (3 - 1))
       *                                              ^
       * </pre>
       */
      def length(expectedLength: Long): Matcher[T with AnyRef] = or(have.length(expectedLength))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (have size (2) and have size (3 - 1))
       *                                       ^
       * </pre>
       */
      def size(expectedSize: Long): Matcher[T with AnyRef] = or(have.size(expectedSize))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                   ^
     * </pre>
     */
    def or(haveWord: HaveWord): OrHaveWord = new OrHaveWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrContainWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (contain (2) or contain (3 - 1))
       *                                            ^
       * </pre>
       */
      def apply[U](expectedElement: U): Matcher[T with GenTraversable[U]] = matchersWrapper.or(matchers.contain(expectedElement))
      // def element[T](expectedElement: T) = matchersWrapper.or(matchers.contain.apply(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain key ("cat") or contain key ("one"))
       *                                                                    ^
       * </pre>
       */
      def key[U](expectedKey: U): Matcher[T with scala.collection.GenMap[U, Any]] = matchersWrapper.or(matchers.contain.key(expectedKey))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
       *                                                                  ^
       * </pre>
       */
      def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = matchersWrapper.or(matchers.contain.value(expectedValue))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
     *                                                       ^
     * </pre>
     */
    def or(containWord: ContainWord): OrContainWord = new OrContainWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrBeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isFileMock should (be a ('file) or be a ('directory))
       *                                       ^
       * </pre>
       */
      def a(symbol: Symbol): Matcher[T with AnyRef] = or(be.a(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isFileMock should (be a (file) or be a (directory))
       *                                      ^
       * </pre>
       */
      def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(be.a(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * appleMock should (be an ('orange) or be an ('apple))
       *                                         ^
       * </pre>
       */
      def an(symbol: Symbol): Matcher[T with AnyRef] = or(be.an(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * appleMock should (be an (orange) or be an (apple))
       *                                        ^
       * </pre>
       */
      def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(be.an(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (be theSameInstanceAs (string) or be theSameInstanceAs (otherString))
       *                                                 ^
       * </pre>
       */
      def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = or(be.theSameInstanceAs(anyRef))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a ('file) or be a ('directory))
     *                                 ^
     * </pre>
     */
    def or(beWord: BeWord): OrBeWord = new OrBeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrFullyMatchWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
       *                                                        ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(fullyMatch.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
       *                                                        ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(fullyMatch.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
     *                                          ^
     * </pre>
     */
    def or(fullyMatchWord: FullyMatchWord): OrFullyMatchWord = new OrFullyMatchWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrIncludeWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex ("hello") or include regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(include.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (include regex ("hello") or include regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(include.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "a1.7b" should (include regex ("1.7") or include regex ("1.7"))
     *                                          ^
     * </pre>
     */
    def or(includeWord: IncludeWord): OrIncludeWord = new OrIncludeWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrStartWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
       *                                                      ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(startWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
       *                                                      ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(startWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex ("hello") or startWith regex ("1.7"))
     *                                            ^
     * </pre>
     */
    def or(startWithWord: StartWithWord): OrStartWithWord = new OrStartWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrEndWithWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regexString: String): Matcher[T with String] = or(endWith.regex(regexString))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
       *                                                  ^
       * </pre>
       */
      def regex(regex: Regex): Matcher[T with String] = or(endWith.regex(regex))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7b" should (endWith regex ("hello") or endWith regex ("7b"))
     *                                           ^
     * </pre>
     */
    def or(endWithWord: EndWithWord): OrEndWithWord = new OrEndWithWord

    /**
     * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
     * the matchers DSL.
     *
     * @author Bill Venners
     */
    final class OrNotWord {

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not equal (1) or not equal (2))
       *                                ^
       * </pre>
       */
      def equal(any: Any): Matcher[T] =
        matchersWrapper.or(matchers.not.apply(matchers.legacyEqual(any)))

      /**
       * This method enables the following syntax for the "primitive" numeric types:
       *
       * <pre class="stHighlight">
       * sevenDotOh should (not equal (17.0 plusOrMinus 0.2) or not equal (17.0 plusOrMinus 0.2))
       *                                                        ^
       * </pre>
       */
      def equal[U](interval: Interval[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.equal(interval))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * aNullRef should (not equal (null) or not equal (null))
       *                                   ^
       * </pre>
       */
      def equal(o: Null): Matcher[T] = {
        matchersWrapper or {
          new Matcher[T] {
            def apply(left: T): MatchResult = {
              MatchResult(
                left != null,
                FailureMessages("equaledNull"),
                FailureMessages("didNotEqualNull", left),
                FailureMessages("midSentenceEqualedNull"),
                FailureMessages("didNotEqualNull", left)
              )
            }
          }
        }
      }

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 1 should (not be (1) or not be (2))
       *                             ^
       * </pre>
       */
      def be(any: Any): Matcher[T] =
        matchersWrapper.or(matchers.not.apply(matchers.be(any)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have length (2) or not have length (3))
       *                                                ^
       * </pre>
       */
      def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.or(matchers.not.apply(matchers.have.length(resultOfLengthWordApplication.expectedLength)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not have size (2) or not have size (3))
       *                                              ^
       * </pre>
       */
      def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[T with AnyRef] =
        matchersWrapper.or(matchers.not.apply(matchers.have.size(resultOfSizeWordApplication.expectedSize)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * book should (not have (title ("Moby Dick")) or not have (author ("Melville")))
       *                                                    ^
       * </pre>
       */
      def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
        matchersWrapper.or(matchers.not.apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*)))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * map should (contain key (7) or not be (null))
       *                                    ^
       * </pre>
       */
      def be(o: Null): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(o))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be < (7) or not be < (8))
       *                               ^
       * </pre>
       */
      def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfLessThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 7 should (not be > (5) or not be > (6))
       *                               ^
       * </pre>
       */
      def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfGreaterThanComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be <= (3) or not be <= (2))
       *                                ^
       * </pre>
       */
      def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfLessThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 8 should (not be >= (7) or not be >= (6))
       *                                ^
       * </pre>
       */
      def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
        matchersWrapper.or(matchers.not.be(resultOfGreaterThanOrEqualToComparison))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 5 should (not be === (7) or not be === (8))
       *                                 ^
       * </pre>
       */
      def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
        matchersWrapper.or(matchers.not.be(tripleEqualsInvocation))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * notEmptyMock should (not be ('full) or not be ('empty))
       *                                            ^
       * </pre>
       */
      def be(symbol: Symbol): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(symbol))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * 2 should (not be (even) or not be (odd))
       *                                ^
       * </pre>
       */
      def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(beMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be (directory) or not be (file))
       *                                          ^
       * </pre>
       */
      def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = matchersWrapper.or(matchers.not.be(bePropertyMatcher))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * isNotFileMock should (not be a ('directory) or not be a ('file))
       *                                                    ^
       * </pre>
       */
      def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be a (directory) or not be a (file))
       *                                            ^
       * </pre>
       */
      def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(resultOfAWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * notAppleMock should (not be an ('apple) or not be an ('apple))
       *                                                ^
       * </pre>
       */
      def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * myFile should (not be an (directory) or not be an (file))
       *                                             ^
       * </pre>
       */
      def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(resultOfAnWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * obj should (not be theSameInstanceAs (otherString) or not be theSameInstanceAs (string))
       *                                                           ^
       * </pre>
       */
      def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = matchersWrapper.or(matchers.not.be(resultOfTheSameInstanceAsApplication))

      /**
       * This method enables the following syntax for the "primitive" numeric types:
       *
       * <pre class="stHighlight">
       * sevenDotOh should (not be (17.0 plusOrMinus 0.2) or not be (17.0 plusOrMinus 0.2))
       *                                                         ^
       * </pre>
       */
      def be[U](interval: Interval[U]): Matcher[T with U] = matchersWrapper.or(matchers.not.be(interval))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimal))
       *                                                     ^
       * </pre>
       */
      def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.fullyMatch(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include regex ("fred") or not include regex (decimal))
       *                                                  ^
       * </pre>
       */
      def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.include(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not include ("bob") or not include ("1.7"))
       *                                           ^
       * </pre>
       */
      def include(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.or(matchers.not.include(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith regex ("bob") or not startWith regex (decimal))
       *                                                   ^
       * </pre>
       */
      def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.startWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not startWith ("fred") or not startWith ("1.7"))
       *                                              ^
       * </pre>
       */
      def startWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.or(matchers.not.startWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith regex ("bob") or not endWith regex (decimal))
       *                                                 ^
       * </pre>
       */
      def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
        matchersWrapper.or(matchers.not.endWith(resultOfRegexWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * "fred" should (not endWith ("fred") or not endWith ("1.7"))
       *                                            ^
       * </pre>
       */
      def endWith(expectedSubstring: String): Matcher[T with String] =
        matchersWrapper.or(matchers.not.endWith(expectedSubstring))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Array(1, 2) should (not contain (1) or not contain (3))
       *                                            ^
       * </pre>
       */
      def contain[U](expectedElement: U): Matcher[T with GenTraversable[U]] =
        matchersWrapper.or(matchers.not.contain(expectedElement))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("three"))
       *                                                                    ^
       * </pre>
       */
      def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
        matchersWrapper.or(matchers.not.contain(resultOfKeyWordApplication))

      /**
       * This method enables the following syntax:
       *
       * <pre class="stHighlight">
       * Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
       *                                                                  ^
       * </pre>
       */
      def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
        matchersWrapper.or(matchers.not.contain(resultOfValueWordApplication))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
     *                                                           ^
     * </pre>
     */
    def or(notWord: NotWord): OrNotWord = new OrNotWord
  }

  /**
   * This implicit conversion method enables ScalaTest matchers expressions that involve <code>and</code> and <code>or</code>.
   */
  implicit def convertToMatcherWrapper[T](leftMatcher: Matcher[T]): MatcherWrapper[T] = new MatcherWrapper(leftMatcher)

  //
  // This class is used as the return type of the overloaded should method (in MapShouldWrapper)
  // that takes a HaveWord. It's key method will be called in situations like this:
  //
  // map should have key 1
  //
  // This gets changed to :
  //
  // convertToMapShouldWrapper(map).should(have).key(1)
  //
  // Thus, the map is wrapped in a convertToMapShouldWrapper call via an implicit conversion, which results in
  // a MapShouldWrapper. This has a should method that takes a HaveWord. That method returns a
  // ResultOfHaveWordPassedToShould that remembers the map to the left of should. Then this class
  // ha a key method that takes a K type, they key type of the map. It does the assertion thing.
  // 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForMap[K, V](left: scala.collection.GenMap[K, V], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should contain key ("one")
     *                    ^
     * </pre>
     */
    def key(expectedKey: K) {
      if (left.exists(_._1 == expectedKey) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            expectedKey)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should contain value (1)
     *                    ^
     * </pre>
     */
    def value(expectedValue: V) {
      // if (left.values.contains(expectedValue) != shouldBeTrue) CHANGING FOR 2.8.0 RC1
      if (left.exists(expectedValue == _._2) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            expectedValue)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForJavaMap[K, V](left: java.util.Map[K, V], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain key ("two")
     *                        ^
     * </pre>
     */
    def key(expectedKey: K) {
      if (left.containsKey(expectedKey) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            expectedKey)
        )
    }

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain value ("2")
     *                        ^
     * </pre>
     */
    def value(expectedValue: V) {
      if (left.containsValue(expectedValue) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            expectedValue)
        )
    }
  }

  /** 
   * This implicit conversion method enables the following syntax (<code>javaColl</code> is a <code>java.util.Collection</code>):
   *
   * <pre class="stHighlight">
   * javaColl should contain ("two")
   * </pre>
   *
   * The <code>(contain ("two"))</code> expression will result in a <code>Matcher[GenTraversable[String]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[java.util.Collection[String]]</code>.
   */
  implicit def convertTraversableMatcherToJavaCollectionMatcher[T](traversableMatcher: Matcher[GenTraversable[T]]): Matcher[java.util.Collection[T]] =
    new Matcher[java.util.Collection[T]] {
      def apply(left: java.util.Collection[T]): MatchResult = {
        val traversable = new Traversable[T] {
          def foreach[U](f: (T) => U) {
            val javaIterator = left.iterator
            while (javaIterator.hasNext)
              f(javaIterator.next)
          }
          override def toString: String = left.toString
        }
        traversableMatcher.apply(traversable)
      }
    }

  /**
   * This implicit conversion method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain (3) and not contain (2))
   * </pre>
   *
   * The <code>(not contain ("two"))</code> expression will result in a <code>Matcher[GenTraversable[String]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[Array[String]]</code>.
  */
  implicit def convertTraversableMatcherToArrayMatcher[T](traversableMatcher: Matcher[GenTraversable[T]]): Matcher[Array[T]] =
    new Matcher[Array[T]] {
      def apply(left: Array[T]): MatchResult = {
        val traversable = new Traversable[T] {
          def foreach[U](f: (T) => U) {
            var index = 0
            while (index < left.length) {
              index += 1
              f(left(index - 1))
            }
          }
          // Need to prettify the array's toString, because by the time it gets to decorateToStringValue, the array
          // has been wrapped in this Traversable and so it won't get prettified anymore by FailureMessages.decorateToStringValue.
          override def toString: String = FailureMessages.prettifyArrays(left).toString
        }
        traversableMatcher.apply(traversable)
      }
    }

  /**
   * This implicit conversion method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
   *
   * <pre class="stHighlight">
   * javaMap should (contain key ("two"))
   * </pre>
   *
   * The <code>(contain key ("two"))</code> expression will result in a <code>Matcher[scala.collection.GenMap[String, Any]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[java.util.Map[String, Any]]</code>.
   */
  implicit def convertMapMatcherToJavaMapMatcher[K, V](mapMatcher: Matcher[scala.collection.GenMap[K, V]]): Matcher[java.util.Map[K, V]] =
    new Matcher[java.util.Map[K, V]] {
      def apply(left: java.util.Map[K, V]): MatchResult = {
        // Even though the java map is mutable I just wrap it it to a plain old Scala map, because
        // I have no intention of mutating it.
        class MapWrapper[Z](javaMap: java.util.Map[K, Z]) extends scala.collection.Map[K, Z] {
          override def size: Int = javaMap.size
          def get(key: K): Option[Z] =
            if (javaMap.containsKey(key)) Some(javaMap.get(key)) else None
          override def iterator: Iterator[(K, Z)] = new Iterator[(K, Z)] {
            private val javaIterator = javaMap.keySet.iterator
            def next: (K, Z) = {
              val nextKey = javaIterator.next
              (nextKey, javaMap.get(nextKey))
            }
            def hasNext: Boolean = javaIterator.hasNext
          }
          override def +[W >: Z] (kv: (K, W)): scala.collection.Map[K, W] = {
            val newJavaMap = new java.util.HashMap[K, W](javaMap)
            val (key, value) = kv
            newJavaMap.put(key, value)
            new MapWrapper[W](newJavaMap)
          }
          override def - (key: K): scala.collection.Map[K, Z] = {
            val newJavaMap = new java.util.HashMap[K, Z](javaMap)
            newJavaMap.remove(key)
            new MapWrapper[Z](newJavaMap)
          }
          override def toString: String = javaMap.toString
        }
        val scalaMap = new MapWrapper[V](left)
        mapMatcher.apply(scalaMap)
      }
    }

  // Ack. The above conversion doesn't apply to java.util.Maps, because java.util.Map is not a subinterface
  // of java.util.Collection. But right now Matcher[Traversable] supports only "contain" and "have size"
  // syntax, and thus that should work on Java maps too, why not. Well I'll tell you why not. It is too complicated.
  // Since java Map is not a java Collection, I'll say the contain syntax doesn't work on it. But you can say
  // have key.

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (contain (2) and contain (1))
     *                             ^
     * </pre>
     */
    def apply[T](expectedElement: T): Matcher[GenTraversable[T]] =
      new Matcher[GenTraversable[T]] {
        def apply(left: GenTraversable[T]): MatchResult =
          MatchResult(
            left.exists(_ == expectedElement), 
            FailureMessages("didNotContainExpectedElement", left, expectedElement),
            FailureMessages("containedExpectedElement", left, expectedElement)
          )
      }
    
    /**
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should contain (odd)
     *               ^
     * </pre>
     */
    def apply[T](right: ContainMatcher[T]) = right
    
    //
    // This key method is called when "contain" is used in a logical expression, such as:
    // map should { contain key 1 and equal (Map(1 -> "Howdy")) }. It results in a matcher
    // that remembers the key value. By making the value type Any, it causes overloaded shoulds
    // to work, because for example a Matcher[GenMap[Int, Any]] is a subtype of Matcher[GenMap[Int, String]],
    // given Map is covariant in its V (the value type stored in the map) parameter and Matcher is
    // contravariant in its lone type parameter. Thus, the type of the Matcher resulting from contain key 1
    // is a subtype of the map type that has a known value type parameter because its that of the map
    // to the left of should. This means the should method that takes a map will be selected by Scala's
    // method overloading rules.
    //
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should (contain key ("fifty five") or contain key ("twenty two"))
     *                     ^
     * </pre>
     *
     * The map's value type parameter cannot be inferred because only a key type is provided in
     * an expression like <code>(contain key ("fifty five"))</code>. The matcher returned
     * by this method matches <code>scala.collection.Map</code>s with the inferred key type and value type <code>Any</code>. Given
     * <code>Map</code> is covariant in its value type, and <code>Matcher</code> is contravariant in
     * its type parameter, a <code>Matcher[Map[Int, Any]]</code>, for example, is a subtype of <code>Matcher[Map[Int, String]]</code>.
     * This will enable the matcher returned by this method to be used against any <code>Map</code> that has
     * the inferred key type.
     */
    def key[K](expectedKey: K): Matcher[scala.collection.GenMap[K, Any]] =
      new Matcher[scala.collection.GenMap[K, Any]] {
        def apply(left: scala.collection.GenMap[K, Any]): MatchResult =
          MatchResult(
            left.exists(_._1 == expectedKey),
            FailureMessages("didNotContainKey", left, expectedKey),
            FailureMessages("containedKey", left, expectedKey)
          )
      }

    // Holy smokes I'm starting to scare myself. I fixed the problem of the compiler not being
    // able to infer the value type in contain value 1 and ... like expressions, because the
    // value type is there, with an existential type. Since I don't know what K is, I decided to
    // try just saying that with an existential type, and it compiled and ran. Pretty darned
    // amazing compiler. The problem could not be fixed like I fixed the key method above, because
    // Maps are nonvariant in their key type parameter, whereas they are covariant in their value
    // type parameter, so the same trick wouldn't work. But this existential type trick seems to
    // work like a charm.
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
     *                                                 ^
     * </pre>
     *
     * The map's key type parameter cannot be inferred because only a value type is provided in
     * an expression like <code>(contain value (5))</code>. The matcher returned
     * by this method matches <code>scala.collection.Map</code>s with the inferred value type and the existential key
     * type <code>[K] forSome { type K }</code>. Even though <code>Matcher</code> is contravariant in its type parameter, because
     * <code>Map</code> is nonvariant in its key type, 
     * a <code>Matcher[Map[Any, Int]]</code>, for example, is <em>not</em> a subtype of <code>Matcher[Map[String, Int]]</code>,
     * so the key type parameter of the <code>Map</code> returned by this method cannot be <code>Any</code>. By making it
     * an existential type, the Scala compiler will not infer it to anything more specific.
     * This will enable the matcher returned by this method to be used against any <code>Map</code> that has
     * the inferred value type.
     *
     */
    def value[V](expectedValue: V): Matcher[scala.collection.GenMap[K, V] forSome { type K }] =
      new Matcher[scala.collection.GenMap[K, V] forSome { type K }] {
        def apply(left: scala.collection.GenMap[K, V] forSome { type K }): MatchResult =
          MatchResult(
            // left.values.contains(expectedValue), CHANGING FOR 2.8.0 RC1
            left.exists(expectedValue == _._2),
            FailureMessages("didNotContainValue", left, expectedValue),
            FailureMessages("containedValue", left, expectedValue)
          )
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class IncludeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include ("1.7") and include ("1.8"))
     *                       ^
     * </pre>
     */
    def apply(expectedSubstring: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left.indexOf(expectedSubstring) >= 0, 
            FailureMessages("didNotIncludeSubstring", left, expectedSubstring),
            FailureMessages("includedSubstring", left, expectedSubstring)
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "a1.7b" should (include regex (decimal) and include regex (decimal))
     *                         ^
     * </pre>
     */
    def regex[T <: String](right: T): Matcher[T] = regex(right.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "a1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
     *                        ^
     * </pre>
     */
    def regex(expectedRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            expectedRegex.findFirstIn(left).isDefined,
            FailureMessages("didNotIncludeRegex", left, expectedRegex),
            FailureMessages("includedRegex", left, expectedRegex)
          )
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class StartWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7b" should (startWith ("1.7") and startWith ("1.7b"))
     *                          ^
     * </pre>
     */
    def apply(right: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left startsWith right,
            FailureMessages("didNotStartWith", left, right),
            FailureMessages("startedWith", left, right)
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "1.7b" should (startWith regex (decimal) and startWith regex (decimal))
     *                          ^
     * </pre>
     */
    def regex[T <: String](right: T): Matcher[T] = regex(right.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
     *                         ^
     * </pre>
     */
    def regex(rightRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            rightRegex.pattern.matcher(left).lookingAt,
            FailureMessages("didNotStartWithRegex", left, rightRegex),
            FailureMessages("startedWithRegex", left, rightRegex)
          )
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class EndWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7b" should (endWith ("1.7b") and endWith ("7b"))
     *                        ^
     * </pre>
     */
    def apply(right: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left endsWith right,
            FailureMessages("didNotEndWith", left, right),
            FailureMessages("endedWith", left, right)
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "b1.7" should (endWith regex (decimal) and endWith regex (decimal))
     *                        ^
     * </pre>
     */
    def regex[T <: String](right: T): Matcher[T] = regex(right.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "b1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
     *                        ^
     * </pre>
     */
    def regex(rightRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          val allMatches = rightRegex.findAllIn(left)
          MatchResult(
            allMatches.hasNext && (allMatches.end == left.length),
            FailureMessages("didNotEndWithRegex", left, rightRegex),
            FailureMessages("endedWithRegex", left, rightRegex)
          )
        }
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class FullyMatchWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimal = """(-)?(\d+)(\.\d*)?"""
     * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
     *                          ^
     * </pre>
     */
    def regex(rightRegexString: String): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            java.util.regex.Pattern.matches(rightRegexString, left),
            FailureMessages("didNotFullyMatchRegex", left, UnquotedString(rightRegexString)),
            FailureMessages("fullyMatchedRegex", left, UnquotedString(rightRegexString))
          )
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
     * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
     *                          ^
     * </pre>
     */
    def regex(rightRegex: Regex): Matcher[String] =
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            rightRegex.pattern.matcher(left).matches,
            FailureMessages("didNotFullyMatchRegex", left, rightRegex),
            FailureMessages("fullyMatchedRegex", left, rightRegex)
          )
      }
  }

// The getLength and getSize field conversions seem inconsistent with
// what I do in symbol HavePropertyMatchers. It isn't, though because the difference is here
// it's a Scala field and there a Java field: a val getLength is a 
// perfectly valid Scala way to get a JavaBean property Java method in the bytecodes.

  /**
   * Sealed supertrait for <code>Length</code> and <code>Size</code> type classes.
   *
   * <p>
   * This sealed trait has two subclasses, <code>Length[T]</code> and <code>Size[T]</code>.
   * Objects of type T for which an implicit <code>Length[T]</code> is available can be used
   * with the <code>should have length</code> syntax.
   * Similarly, objects of type T for which an implicit <code>Size[T]</code> is available can be used
   * with the <code>should have size</code> syntax.
   * By creating an appropriate type class, therefore, you can enable the size and length checking syntax with arbitrary objects.
   * As an example, consider <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
   * can't be used with ScalaTest's <code>have length</code> syntax. 
   * </p>
   *
   * <pre>
   * scala> import java.net.DatagramPacket
   * import java.net.DatagramPacket
   * 
   * scala> import org.scalatest.matchers.ShouldMatchers._
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * scala> val dp = new DatagramPacket(Array(0x0, 0x1, 0x2, 0x3), 4)
   * dp: java.net.DatagramPacket = java.net.DatagramPacket@54906181
   * 
   * scala> dp.getLength
   * res0: Int = 4
   *
   * scala> dp should have length 4
   * <console>:13: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.net.DatagramPacket]
   *          dp should have length 4
   *             ^
   *
   * scala> implicit val lengthOfDatagramPacket =
   *     |   new Length[DatagramPacket] {
   *     |     def extentOf(dp: DatagramPacket): Long = dp.getLength
   *     |   }
   * lengthOfDatagramPacket: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Length[java.net.DatagramPacket] = $anon$1@550c6b37
   *
   * scala> dp should have length 4
   *
   * scala> dp should have length 3
   * org.scalatest.exceptions.TestFailedException:  java.net.DatagramPacket@54906181 had length 4, not length 3
   * </pre>
   *
   * @author Bill Venners
   */
  sealed trait Extent[T] {
    def extentOf(o: T): Long
  }

  /**
   * Supertrait for <code>Length</code> type classes.
   *
   * <p>
   * Trait <code>Length</code> is a type class trait for objects that can be queried for length.
   * Objects of type T for which an implicit <code>Length[T]</code> is available can be used
   * with the <code>should have length</code> syntax.
   * In other words, this trait enables you to use the length checking
   * syntax with arbitrary objects. As an example, consider
   * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
   * can't be used with ScalaTest's <code>have length</code> syntax. 
   * </p>
   *
   * <pre>
   * scala> import java.net.DatagramPacket
   * import java.net.DatagramPacket
   * 
   * scala> import org.scalatest.matchers.ShouldMatchers._
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * scala> val dp = new DatagramPacket(Array(0x0, 0x1, 0x2, 0x3), 4)
   * dp: java.net.DatagramPacket = java.net.DatagramPacket@54906181
   * 
   * scala> dp.getLength
   * res0: Int = 4
   *
   * scala> dp should have length 4
   * <console>:13: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.net.DatagramPacket]
   *          dp should have length 4
   *             ^
   *
   * scala> implicit val lengthOfDatagramPacket =
   *     |   new Length[DatagramPacket] {
   *     |     def extentOf(dp: DatagramPacket): Long = dp.getLength
   *     |   }
   * lengthOfDatagramPacket: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Length[java.net.DatagramPacket] = $anon$1@550c6b37
   *
   * scala> dp should have length 4
   *
   * scala> dp should have length 3
   * org.scalatest.exceptions.TestFailedException:  java.net.DatagramPacket@54906181 had length 4, not length 3
   * </pre>
   *
   * @author Bill Venners
   */
  trait Length[T] extends Extent[T]

  /**
   * Supertrait for <code>Size</code> type classes.
   *
   * <p>
   * Trait <code>Size</code> is a type class trait for objects that can be queried for size.
   * Objects of type T for which an implicit <code>Size[T]</code> is available can be used
   * with the <code>should have size</code> syntax.
   * In other words, this trait enables you to use the size checking
   * syntax with arbitrary objects. As an example, consider
   * <code>java.net.DatagramPacket</code>, which has a <code>getLength</code> method. By default, this
   * can't be used with ScalaTest's <code>have length</code> syntax. 
   * </p>
   *
   * <pre>
   * scala> import java.awt.image.DataBufferByte
   * import java.awt.image.DataBufferByte
   * 
   * scala> import org.scalatest.matchers.ShouldMatchers._
   * import org.scalatest.matchers.ShouldMatchers._
   *
   * scala> val db = new DataBufferByte(4)
   * db: java.awt.image.DataBufferByte = java.awt.image.DataBufferByte@33d5e94f
   * 
   * scala> db.getSize
   * res0: Int = 4
   *
   * scala> db should have size 4
   * <console>:17: error: could not find implicit value for parameter ev: org.scalatest.matchers.ShouldMatchers.Extent[java.awt.image.DataBufferByte]
   *               db should have size 4
   *                  ^
   * scala> implicit val sizeOfDataBufferByte =
   *      |   new Size[DataBufferByte] {
   *      |     def extentOf(db: DataBufferByte): Long = db.getSize
   *      |   }
   * sizeOfDataBufferByte: java.lang.Object with org.scalatest.matchers.ShouldMatchers.Size[java.awt.image.DataBufferByte] = $anon$1@4c69bdf8
   *
   * scala> db should have size 4
   *
   * scala> db should have size 3
   * org.scalatest.exceptions.TestFailedException:  java.awt.image.DataBufferByte@33d5e94f had size 4, not size 3
   * </pre>
   *
   * @author Bill Venners
   */
  trait Size[T] extends Extent[T]

  // This guy is generally done through an implicit conversion from a symbol. It takes that symbol, and 
  // then represents an object with an apply method. So it gives an apply method to symbols.
  // book should have ('author ("Gibson"))
  //                   ^ // Basically this 'author symbol gets converted into this class, and its apply  method takes "Gibson"
  // TODO, put the documentation of the details of the algo for selecting a method or field to use here.
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used as the result of an implicit conversion from class <code>Symbol</code>, to enable symbols to be
   * used in <code>have ('author ("Dickens"))</code> syntax. The name of the implicit conversion method is
   * <code>convertSymbolToHavePropertyMatcherGenerator</code>.
   * </p>
   *
   * <p>
   * Class <code>HavePropertyMatcherGenerator</code>'s primary constructor takes a <code>Symbol</code>. The 
   * <code>apply</code> method uses reflection to find and access a property that has the name specified by the
   * <code>Symbol</code> passed to the constructor, so it can determine if the property has the expected value
   * passed to <code>apply</code>.
   * If the symbol passed is <code>'title</code>, for example, the <code>apply</code> method
   * will use reflection to look for a public Java field named
   * "title", a public method named "title", or a public method named "getTitle". 
   * If a method, it must take no parameters. If multiple candidates are found,
   * the <code>apply</code> method will select based on the following algorithm:
   * </p>
   * 
   * <table class="stTable">
   * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"get" Method</th><th class="stHeadingCell">Result</th></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>getTitle()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>title()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>title()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>title()</code></td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>title()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>title</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>getTitle()</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell"><code>title()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>title()</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell"><code>title()</code></td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>title()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * </table>
   * 
   *
   * @author Bill Venners
   */
  final class HavePropertyMatcherGenerator(symbol: Symbol) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have ('title ("A Tale of Two Cities"))
     *                          ^
     * </pre>
     * 
     * <p>
     * This class has an <code>apply</code> method that will produce a <code>HavePropertyMatcher[AnyRef, Any]</code>.
     * The implicit conversion method, <code>convertSymbolToHavePropertyMatcherGenerator</code>, will cause the 
     * above line of code to be eventually transformed into:
     * </p>
     * 
     * <pre class="stHighlight">
     * book should have (convertSymbolToHavePropertyMatcherGenerator('title).apply("A Tale of Two Cities"))
     * </pre>
     */
    def apply(expectedValue: Any): HavePropertyMatcher[AnyRef, Any] =
      new HavePropertyMatcher[AnyRef, Any] {

        /**
         * This method enables the following syntax:
         *
         * <pre class="stHighlight">
         * book should have ('title ("A Tale of Two Cities"))
         * </pre>
         * 
         * <p>
         * This method uses reflection to discover a field or method with a name that indicates it represents
         * the value of the property with the name contained in the <code>Symbol</code> passed to the 
         * <code>HavePropertyMatcherGenerator</code>'s constructor. The field or method must be public. To be a
         * candidate, a field must have the name <code>symbol.name</code>, so if <code>symbol</code> is <code>'title</code>,
         * the field name sought will be <code>"title"</code>. To be a candidate, a method must either have the name
         * <code>symbol.name</code>, or have a JavaBean-style <code>get</code> or <code>is</code>. If the type of the
         * passed <code>expectedValue</code> is <code>Boolean</code>, <code>"is"</code> is prepended, else <code>"get"</code>
         * is prepended. Thus if <code>'title</code> is passed as <code>symbol</code>, and the type of the <code>expectedValue</code> is
         * <code>String</code>, a method named <code>getTitle</code> will be considered a candidate (the return type
         * of <code>getTitle</code> will not be checked, so it need not be <code>String</code>. By contrast, if <code>'defined</code>
         * is passed as <code>symbol</code>, and the type of the <code>expectedValue</code> is <code>Boolean</code>, a method
         * named <code>isTitle</code> will be considered a candidate so long as its return type is <code>Boolean</code>.
         * </p>
         * TODO continue the story
         */
        def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Any] = {

          // If 'empty passed, propertyName would be "empty"
          val propertyName = symbol.name

          val isBooleanProperty =
            expectedValue match {
              case o: Boolean => true
              case _ => false
            }

          accessProperty(objectWithProperty, symbol, isBooleanProperty) match {

            case None =>

              // if propertyName is '>, mangledPropertyName would be "$greater"
              val mangledPropertyName = transformOperatorChars(propertyName)

              // methodNameToInvoke would also be "title"
              val methodNameToInvoke = mangledPropertyName

              // methodNameToInvokeWithGet would be "getTitle"
              val methodNameToInvokeWithGet = "get"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

              throw newTestFailedException(Resources("propertyNotFound", methodNameToInvoke, expectedValue.toString, methodNameToInvokeWithGet))

            case Some(result) =>

              new HavePropertyMatchResult[Any](
                result == expectedValue,
                propertyName,
                expectedValue,
                result
              )
          }
        }
      }
  }

  /**
   * This implicit conversion method converts a <code>Symbol</code> to a
   * <code>HavePropertyMatcherGenerator</code>, to enable the symbol to be used with the <code>have ('author ("Dickens"))</code> syntax.
   */
  implicit def convertSymbolToHavePropertyMatcherGenerator(symbol: Symbol): HavePropertyMatcherGenerator = new HavePropertyMatcherGenerator(symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class HaveWord {

    // TODO: How about returning a Matcher[Gazornimplatz] and then providing implicit conversion
    // methods from Matcher[Gazornimplatz] to Matcher[Seq], Matcher[String], Matcher[java.util.List], and
    // Matcher[the structural length methods]. This is similar to the technique I used with "contain (7)"
    // to get it to work with java.util.Collection.
    // I couldn't figure out how to combine view bounds with existential types. May or may not
    // be possible, but going dynamic for now at least.
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have length (9)
     *                  ^
     * </pre>
     *
     * <p>
     * Currently (as of ScalaTest 0.9.5), this method will produce a <code>Matcher[AnyRef]</code>, and if the
     * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>length</code> property
     * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem. The one exception is that it will work on
     * <code>java.util.List</code>, even though that type has no <code>length</code> structure (its <code>size</code> property
     * will be used instead.) In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
     * </p>
     */
    def length(expectedLength: Long): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          left match {
            case leftArray: Array[_] =>
              MatchResult(
                leftArray.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftSeq: GenSeq[_] =>
              MatchResult(
                leftSeq.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftString: String =>
              MatchResult(
                leftString.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftJavaList: java.util.List[_] =>
              MatchResult(
                leftJavaList.size == expectedLength,
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case _ =>

              accessProperty(left, 'length, false) match {

                case None =>

                  throw newTestFailedException(Resources("noLengthStructure", expectedLength.toString))

                case Some(result) =>

                  MatchResult(
                    result == expectedLength,
                    FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                    FailureMessages("hadExpectedLength", left, expectedLength)
                  )
              }
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have size (9)
     *                  ^
     * </pre>
     *
     * <p>
     * Currently, this method will produce a <code>Matcher[AnyRef]</code>, and if the
     * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>size</code> property
     * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem.
     * In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
     * </p>
     */
    def size(expectedSize: Long): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          left match {
            case leftArray: Array[_] =>
              MatchResult(
                leftArray.length == expectedSize, 
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case leftTrav: GenTraversable[_] =>
              MatchResult(
                leftTrav.size == expectedSize, 
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case leftJavaList: java.util.List[_] =>
              MatchResult(
                leftJavaList.size == expectedSize,
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case _ =>

              accessProperty(left, 'size, false) match {

                case None =>

                  throw newTestFailedException(Resources("noSizeStructure", expectedSize.toString))

                case Some(result) =>

                  MatchResult(
                    result == expectedSize,
                    FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                    FailureMessages("hadExpectedSize", left, expectedSize)
                  )
              }
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have (title ("A Tale of Two Cities"))
     *                  ^
     * </pre>
     */
    def apply[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =

      new Matcher[T] {

        def apply(left: T): MatchResult = {

          val results =
            for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
              propertyVerifier(left)

          val firstFailureOption = results.find(pv => !pv.matches)

          val justOneProperty = propertyMatchers.length == 0

          firstFailureOption match {

            case Some(firstFailure) =>

              val failedVerification = firstFailure
              val failureMessage =
                FailureMessages(
                  "propertyDidNotHaveExpectedValue",
                  UnquotedString(failedVerification.propertyName),
                  failedVerification.expectedValue,
                  failedVerification.actualValue,
                  left
                )
              val midSentenceFailureMessage =
                FailureMessages(
                  "midSentencePropertyDidNotHaveExpectedValue",
                  UnquotedString(failedVerification.propertyName),
                  failedVerification.expectedValue,
                  failedVerification.actualValue,
                  left
                )

              MatchResult(false, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)

            case None =>

              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "propertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left
                  )
                }
                else FailureMessages("allPropertiesHadExpectedValues", left)

              val midSentenceFailureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "midSentencePropertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left
                  )
                }
                else FailureMessages("midSentenceAllPropertiesHadExpectedValues", left)

              MatchResult(true, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)
          }
        }
      }
  }

  //
  // This class is used as the return type of the overloaded should method (in TraversableShouldWrapper) 
  // that takes a HaveWord. It's size method will be called in situations like this:
  //
  // list should have size 1
  //
  // This gets changed to :
  //
  // convertToTraversableShouldWrapper(list).should(have).size(1)
  //
  // Thus, the list is wrapped in a convertToTraversableShouldWrapper call via an implicit conversion, which results in
  // a TraversableShouldWrapper. This has a should method that takes a HaveWord. That method returns a
  // ResultOfHaveWordForTraverablePassedToShould that remembers the map to the left of should. Then this class
  // has a size method that takes a T type, type parameter of the Traversable. It does the assertion thing.
  // 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfHaveWordForTraversable[T](left: GenTraversable[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should have size (10)
     *                        ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfHaveWordForJavaCollection[E, L[_] <: java.util.Collection[_]](left: L[E], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should have size (10)
     *                       ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForJavaMap(left: java.util.Map[_, _], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should have size (10)
     *                     ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForSeq[T](left: GenSeq[T], shouldBeTrue: Boolean) extends ResultOfHaveWordForTraversable[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seq should have length (20)
     *                 ^
     * </pre>
     */
    def length(expectedLength: Int) {
      if ((left.length == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><cod
e>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
class ResultOfHaveWordForArray[T](left: Array[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * array should have size (10)
     *                   ^
     * </pre>
     */
    def size(expectedSize: Int) {
      if ((left.size == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * array should have length (20)
     *                   ^
     * </pre>
     */
    def length(expectedLength: Int) {
      if ((left.length == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForTraversable[E, T[_] <: GenTraversable[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * iterable should not contain ("one")
     *                     ^
     * </pre>
     */
    def contain(expectedElement: E) {
      val right = expectedElement
      if ((left.exists(_ == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not contain containMatcher
     *                       ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      val result = right(left.asInstanceOf[scala.collection.GenTraversable[E]])
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
          None, 
          3
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not have size (3)
     *                       ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForJavaCollection[E, T[_] <: java.util.Collection[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should not have size (3)
     *                           ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should not contain ("elephant")
     *                           ^
     * </pre>
     */
    def contain(expectedElement: E) {
      val right = expectedElement
      if ((left.contains(right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForMap[K, V, L[_, _] <: scala.collection.GenMap[_, _]](left: L[K, V], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not contain key ("three")
     *                ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      val right = resultOfKeyWordApplication.expectedKey
      if ((left.asInstanceOf[GenMap[K, V]].exists(_._1 == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should not contain value (3)
     *                                        ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      val right = resultOfValueWordApplication.expectedValue
      if ((left.asInstanceOf[GenMap[K, V]].exists(_._2 == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
              left,
              right
            )
          )
      }
    }

    // TODO: Had to pull these methods out of ReusltOfNotWordForTraversable, because can't exent
    // it without losing precision on the inferred types. Map[String, Int] becomes GenIterable[(Any, Any)]
    // So the wrong Equality type class was chosen. By going around ResultOfNotWordForTraversable, I can
    // get the precise Map type up to ResultOfNotWord's equal method, which requires the Equality type class.

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * iterable should not contain ("one")
     *                     ^
     * </pre>
     */
    def contain(expectedElement: (K, V)) {
      val right = expectedElement
      if ((left.exists(_ == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not contain containMatcher
     *                       ^
     * </pre>
     */
    def contain(right: ContainMatcher[(K, V)]) {
      val result = right(left.asInstanceOf[scala.collection.GenTraversable[(K, V)]])
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not have size (3)
     *                       ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForJavaMap[K, V, L[_, _] <: java.util.Map[_, _]](left: L[K, V], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should not contain key ("three")
     *                    ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      val right = resultOfKeyWordApplication.expectedKey
      if ((left.containsKey(right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should not contain value (3)
     *                            ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      val right = resultOfValueWordApplication.expectedValue
      if ((left.containsValue(right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForSeq[E, T[_] <: GenSeq[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForTraversable[E, T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * List(1, 2) should not have length (12)
     *                       ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html">
<code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForArray[E](left: Array[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array("two", "three") should not contain ("one")
     *                                  ^
     * </pre>
     */
    def contain(expectedElement: E) {
      val right = expectedElement
      if ((left.exists(_ == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should not have size (3)
     *                        ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should not have length (12)
     *                        ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForJavaList[E, L[_] <: java.util.List[_]](left: L[E], shouldBeTrue: Boolean) extends ResultOfHaveWordForJavaCollection[E, L](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaList should have length (12)
     *                      ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def length(expectedLength: Int) {
      if ((left.size == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForJavaList[E, T[_] <: java.util.List[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForJavaCollection[E, T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaList should not have length (12)
     *                     ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.size == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfBeWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      if ((left eq right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            right
          )
        )
    }

    /* *
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be a [String]
     *                  ^
     * </pre>
    def a[EXPECTED : ClassManifest] {
      val clazz = implicitly[ClassManifest[EXPECTED]].erasure.asInstanceOf[Class[EXPECTED]]
      if (clazz.isAssignableFrom(left.getClass)) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAnInstanceOf", left, UnquotedString(clazz.getName))
          else
            FailureMessages("wasAnInstanceOf")
        )
      }
    }
     */

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fileMock should be a ('file)
     *                    ^
     * </pre>
     */
    def a(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should be a (goodRead)
     *                   ^
     * </pre>
     */
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }

    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fruit should be an ('orange)
     *                 ^
     * </pre>
     */
    def an(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should be an (excellentRead)
     *                ^
     * </pre>
     */
    def an(beTrueMatcher: BePropertyMatcher[T]) {
      val beTrueMatchResult = beTrueMatcher(left)
      if (beTrueMatchResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(beTrueMatchResult.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(beTrueMatchResult.propertyName))
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWord[T](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not equal (7)
     *                   ^
     * </pre>
     */
    def equal(right: Any)(implicit equality: Equality[T]) {
      if (equality.areEqual(left, right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
           if (shouldBeTrue) "didNotEqual" else "equaled",
            left,
            right
          )
        )
    }

// TODO: Why isn't there an equal that takes a tolerance? (and later one that takes a null?)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be (7)
     *                   ^
     * </pre>
     */
    def be(right: Any) {
      if ((left == right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
           if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be <= (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be >= (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be < (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be > (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^
     * </pre>
     */
    def be(comparison: TripleEqualsInvocation[_]) {
      if ((left == comparison.right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to
     * a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * 2 should not be (odd)
     *              ^
     * </pre>
     */
    def be(beMatcher: BeMatcher[T]) {
      val result = beMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            result.failureMessage
          else
            result.negatedFailureMessage
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWord[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not be (null)
     *                ^
     * </pre>
     */
    def be(o: Null) {
      if ((left == null) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotNull", left) 
          else
            FailureMessages("wasNull")
        )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * stack should not be ('empty)
     *                  ^
     * </pre>
     */
    def be(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, false, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
     * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
     *
     * <pre class="stHighlight">
     * stack should not be (empty)
     *                      ^
     * </pre>
     */
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNot", left, UnquotedString(result.propertyName))
          else
            FailureMessages("was", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notFileMock should not be a ('file)
     *                        ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      val matcherResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * notFileMock should not be a (file)
     *                        ^
     * </pre>
     */
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
      val result = resultOfAWordApplication.bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * keyEvent should not be an ('actionKey)
     *                     ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      val matcherResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * keyEvent should not be an (actionKey)
     *                     ^
     * </pre>
     */
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
      val result = resultOfAnWordApplication.bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * otherString should not be theSameInstanceAs (string)
     *                        ^
     * </pre>
     */
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
      if ((resultOfSameInstanceAsApplication.right eq left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            resultOfSameInstanceAsApplication.right
          )
        )
      }
    }

    // TODO: Explain this matrix somewhere
    // The type parameter U has T as its lower bound, which means that U must be T or a supertype of T. Left is T, oh, because
    // HavePropertyMatcher is contravariant in its type parameter T, and that nmakes sense, because a HavePropertyMatcher of Any should
    // be able to match on a String.
    // <code>not have (a (1), b (2))</code> must mean the opposite of <code>have (a (1), b (2))</code>, which means that 
    // <code>not have (a (1), b (2))</code> will be true if either <code>(a (1)).matches</code> or <code>(b (1)).matches</code> is false.
    // Only if both <code>(a (1)).matches</code> or <code>(b (1)).matches</code> are true will <code>not have (a (1), b (2))</code> be false.
    // title/author matches | have | have not
    // 0 0 | 0 | 1 
    // 0 1 | 0 | 1
    // 1 0 | 0 | 1
    // 1 1 | 1 | 0
    // 
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should not have (title ("One Hundred Years of Solitude"))
     *                 ^
     * </pre>
     */
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {

      val results =
        for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
          propertyVerifier(left)

      val firstFailureOption = results.find(pv => !pv.matches)

      val justOneProperty = propertyMatchers.length == 0

      // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
      // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
      if (firstFailureOption.isDefined == shouldBeTrue) {
        firstFailureOption match {
          case Some(firstFailure) =>
            // This is one of these cases, thus will only get here if shouldBeTrue is true
            // 0 0 | 0 | 1
            // 0 1 | 0 | 1
            // 1 0 | 0 | 1
            throw newTestFailedException(
              FailureMessages(
                "propertyDidNotHaveExpectedValue",
                 UnquotedString(firstFailure.propertyName),
                 firstFailure.expectedValue,
                 firstFailure.actualValue,
                 left
              )
            )
          case None =>
            // This is this cases, thus will only get here if shouldBeTrue is false
            // 1 1 | 1 | 0
            val failureMessage =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                FailureMessages(
                  "propertyHadExpectedValue",
                  UnquotedString(firstPropertyResult.propertyName),
                  firstPropertyResult.expectedValue,
                  left
                )
              }
              else FailureMessages("allPropertiesHadExpectedValues", left)

            throw newTestFailedException(failureMessage)
        } 
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForString(left: String, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef[String](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should not have length (12)
     *                   ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                   ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
      if (rightRegex.pattern.matcher(left).matches != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
            left,
            rightRegex
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not include regex ("wo.ld")
     *                   ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
      if (rightRegex.findFirstIn(left).isDefined != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
            left,
            rightRegex
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not include ("world")
     *                   ^
     * </pre>
     */
    def include(expectedSubstring: String) {
      if ((left.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeSubstring" else "includedSubstring",
            left,
            expectedSubstring
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not startWith regex ("Hel*o")
     *                   ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
      if (rightRegex.pattern.matcher(left).lookingAt != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
            left,
            rightRegex
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not startWith ("1.7")
     *                    ^
     * </pre>
     */
    def startWith(expectedSubstring: String) {
      if ((left.indexOf(expectedSubstring) == 0) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWith" else "startedWith",
            left,
            expectedSubstring
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * greeting should not endWith regex ("wor.d")
     *                     ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
      val allMatches = rightRegex.findAllIn(left)
      if (allMatches.hasNext && (allMatches.end == left.length) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
            left,
            rightRegex
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not endWith ("1.7")
     *                    ^
     * </pre>
     */
    def endWith(expectedSubstring: String) {
      if ((left endsWith expectedSubstring) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWith" else "endedWith",
            left,
            expectedSubstring
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForNumeric[T : Numeric](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWord[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should not be (6.5 +- 0.2)
     *                       ^
     * </pre>
     */
    def be(interval: Interval[T]) {
      if (interval.isWithin(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            interval.pivot,
            interval.tolerance
          )
        )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should not equal (6.5 +- 0.2)
     *                       ^
     * </pre>
     */
    def equal(interval: Interval[T]) {
      if (interval.isWithin(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
            left,
            interval.pivot,
            interval.tolerance
          )
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class RegexWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
     *                                     ^
     * </pre>
     */
    def apply(regexString: String): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regexString)

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                                     ^
     * </pre>
     */
    def apply(regex: Regex): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regex)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * The primary constructor enables the following syntax (with a passed <code>scala.util.matching.Regex</code>): 
   * </p>
   *
   * <pre class="stHighlight">
   * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
   *                               ^
   * </pre>
   *
   * @author Bill Venners
   */
  final class ResultOfRegexWordApplication(val regex: Regex) {

    /**
     * This auxiliary constructor enables the following syntax (with a passed <code>java.lang.String</code>): 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                               ^
     * </pre>
     */
    def this(regexString: String) = this(new Regex(regexString))
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should have length (12)
     *                    ^
     * </pre>
     */
    def length(expectedLength: Int) {
      if ((left.length == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength
          )
        )
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfIncludeWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("world")
     *                       ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("wo.ld".r)
     *                       ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.findFirstIn(left).isDefined != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
            left,
            rightRegex
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfStartWithWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o")
     *                         ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o".r)
     *                         ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.pattern.matcher(left).lookingAt != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
            left,
            rightRegex
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfEndWithWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d")
     *                       ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d".r)
     *                       ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      val allMatches = rightRegex.findAllIn(left)
      if ((allMatches.hasNext && (allMatches.end == left.length)) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
            left,
            rightRegex
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfFullyMatchWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullMatch regex ("Hel*o world")
     *                         ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullymatch regex ("Hel*o world".r)
     *                          ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.pattern.matcher(left).matches != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
            left,
            rightRegex
          )
        )
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should equal (7)
   *               ^
   * </pre>
   *
   * <p>
   * The <code>left should equal (right)</code> syntax works by calling <code>==</code> on the <code>left</code>
   * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
   * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
   * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
   * </p>
   * 
   * <pre class="stHighlight">
   * Array(1, 2) == Array(1, 2) // yields false
   * </pre>
   *
   * <p>
   * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest will compare
   * the two arrays structurally, taking into consideration the equality of the array's contents:
   * </p>
   *
   * <pre class="stHighlight">
   * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
   * </pre>
   *
   * <p>
   * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
   * <code>be theSameInstanceAs</code> syntax.
   * </p>
   *
   */
  def equal(right: Any): MatcherGen1[Any, Equality] =
    new MatcherGen1[Any, Equality] {
      def matcher[T <: Any : Equality]: Matcher[T] = {
        val equality = implicitly[Equality[T]]
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
            MatchResult(
              equality.areEqual(left, right),
              FailureMessages("didNotEqual", leftee, rightee),
              FailureMessages("equaled", left, right)
            )
          }
        }
      }
    }

  // Going back to original, legacy one to get to a good place to check in.
/*
  def equal(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("didNotEqual", leftee, rightee),
            FailureMessages("equaled", left, right)
          )
        }
      }
*/

  def legacyEqual(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("didNotEqual", leftee, rightee),
            FailureMessages("equaled", left, right)
          )
        }
      }

  private def andMatchersAndApply[T](left: T, leftMatcher: Matcher[T], rightMatcher: Matcher[T]): MatchResult = {
    val leftMatchResult = leftMatcher(left)
    val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
    if (!leftMatchResult.matches)
      MatchResult(
        false,
        leftMatchResult.failureMessage,
        leftMatchResult.negatedFailureMessage,
        leftMatchResult.midSentenceFailureMessage,
        leftMatchResult.midSentenceNegatedFailureMessage
      )
    else {
      MatchResult(
        rightMatchResult.matches,
        Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
        Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
      )
    }
  }

  private def orMatchersAndApply[T](left: T, leftMatcher: Matcher[T], rightMatcher: Matcher[T]): MatchResult = {
    val leftMatchResult = leftMatcher(left)
    val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
    if (leftMatchResult.matches)
      MatchResult(
        true,
        leftMatchResult.negatedFailureMessage,
        leftMatchResult.failureMessage,
        leftMatchResult.midSentenceNegatedFailureMessage,
        leftMatchResult.midSentenceFailureMessage
      )
    else {
      MatchResult(
        rightMatchResult.matches,
        Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
        Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
      )
    }
  }

  abstract class MatcherGen1[-SUPERCLASS, TYPECLASS[_]] { thisMatcherGen1 =>

    def matcher[T <: SUPERCLASS : TYPECLASS]: Matcher[T]

    // (equal (7) and ...)
    def and[U <: SUPERCLASS](rightMatcher: Matcher[U]): MatcherGen1[U, TYPECLASS] =
      new MatcherGen1[U, TYPECLASS] {
        def matcher[V <: U : TYPECLASS]: Matcher[V] = {
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              val leftMatcher = thisMatcherGen1.matcher
              andMatchersAndApply(left, leftMatcher, rightMatcher)
/*
              val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
              if (!leftMatchResult.matches)
                MatchResult(
                  false,
                  leftMatchResult.failureMessage,
                  leftMatchResult.negatedFailureMessage,
                  leftMatchResult.midSentenceFailureMessage,
                  leftMatchResult.midSentenceNegatedFailureMessage
                )
              else {
                MatchResult(
                  rightMatchResult.matches,
                  Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
                  Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
                  Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
                  Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
                )
              }
*/
            }
          }
        }
      }

    // (equal (7) or ...)
    def or[U <: SUPERCLASS](rightMatcher: Matcher[U]): MatcherGen1[U, TYPECLASS] =
      new MatcherGen1[U, TYPECLASS] {
        def matcher[V <: U : TYPECLASS]: Matcher[V] = {
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              val leftMatcher = thisMatcherGen1.matcher
              orMatchersAndApply(left, leftMatcher, rightMatcher)
            }
          }
        }
      }

// Need one for the same typeclass and one for a different typeclass, yes, and can overload because
// one returns a MatcherGen1 the other a MatcherGen2.
     // "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})
    def or[U <: SUPERCLASS](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
      new MatcherGen1[U, TYPECLASS] {
        def matcher[V <: U : TYPECLASS]: Matcher[V] = {
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              val leftMatcher = thisMatcherGen1.matcher
              val rightMatcher = rightMatcherGen1.matcher
              orMatchersAndApply(left, leftMatcher, rightMatcher)
            }
          }
        }
      }

    // "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
    def and[U <: SUPERCLASS](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
      new MatcherGen1[U, TYPECLASS] {
        def matcher[V <: U : TYPECLASS]: Matcher[V] = {
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              val leftMatcher = thisMatcherGen1.matcher
              val rightMatcher = rightMatcherGen1.matcher
              andMatchersAndApply(left, leftMatcher, rightMatcher)
            }
          }
        }
      }
  }

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal (100 +- 1)
   *        ^
   * </pre>
   */
  def equal[T](interval: Interval[T]): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        MatchResult(
          interval.isWithin(left),
          FailureMessages("didNotEqualPlusOrMinus", left, interval.pivot, interval.tolerance),
          FailureMessages("equaledPlusOrMinus", left, interval.pivot, interval.tolerance)
        )
      }
    }
  }

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal (null)
   *        ^
   * </pre>
   */
  def equal(o: Null): Matcher[AnyRef] = 
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left == null,
          FailureMessages("didNotEqualNull", left),
          FailureMessages("equaledNull"),
          FailureMessages("didNotEqualNull", left),
          FailureMessages("midSentenceEqualedNull")
        )
      }
    }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
/*
  final class TreatedAsOrderedWrapper {
    def <[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left < right,
            FailureMessages("wasNotLessThan", left, right),
            FailureMessages("wasLessThan", left, right)
          )
      }
    def >[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left > right,
            FailureMessages("wasNotGreaterThan", left, right),
            FailureMessages("wasGreaterThan", left, right)
          )
      }
    def <=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left <= right,
            FailureMessages("wasNotLessThanOrEqualTo", left, right),
            FailureMessages("wasLessThanOrEqualTo", left, right)
          )
      }
    def >=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T) =
          MatchResult(
            left >= right,
            FailureMessages("wasNotGreaterThanOrEqualTo", left, right),
            FailureMessages("wasGreaterThanOrEqualTo", left, right)
          )
      }
  }

  // This one is for one should be < (7)
  implicit def convertBeWordToForOrdered(beWord: BeWord): TreatedAsOrderedWrapper = new TreatedAsOrderedWrapper
*/

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * Class <code>BeWord</code> contains an <code>apply</code> method that takes a <code>Symbol</code>, which uses reflection
   * to find and access a <code>Boolean</code> property and determine if it is <code>true</code>.
   * If the symbol passed is <code>'empty</code>, for example, the <code>apply</code> method
   * will use reflection to look for a public Java field named
   * "empty", a public method named "empty", or a public method named "isEmpty". If a field, it must be of type <code>Boolean</code>.
   * If a method, it must take no parameters and return <code>Boolean</code>. If multiple candidates are found,
   * the <code>apply</code> method will select based on the following algorithm:
   * </p>
   * 
   * <table class="stTable">
   * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"is" Method</th><th class="stHeadingCell">Result</th></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>empty</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * </table>
   * 
   * @author Bill Venners
   */
  final class BeWord {


    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &lt; (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the less than operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the less than operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &lt; (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &lt; (7))
     *                       ^
     * </pre>
     */
    def <[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left < right,
            FailureMessages("wasNotLessThan", left, right),
            FailureMessages("wasLessThan", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &gt; (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the greater than operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the greater than operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &gt; (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &gt; (7))
     *                       ^
     * </pre>
     */
    def >[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left > right,
            FailureMessages("wasNotGreaterThan", left, right),
            FailureMessages("wasGreaterThan", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &lt;= (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the less than or equal to operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the less than or equal to operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &lt;= (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &lt;= (7))
     *                       ^
     * </pre>
     */
    def <=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left <= right,
            FailureMessages("wasNotLessThanOrEqualTo", left, right),
            FailureMessages("wasLessThanOrEqualTo", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &gt;= (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the greater than or equal to operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the greater than or equal to operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &gt;= (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &gt;= (7))
     *                       ^
     * </pre>
     */
    def >=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left >= right,
            FailureMessages("wasNotGreaterThanOrEqualTo", left, right),
            FailureMessages("wasGreaterThanOrEqualTo", left, right)
          )
      }


    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be === (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the === operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the ===n operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be === (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be === (7))
     *                       ^
     * </pre>
     */
    def ===(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("wasNotEqualTo", leftee, rightee),
            FailureMessages("wasEqualTo", left, right)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * fileMock should not { be a ('file) }
     *                          ^
     * </pre>
     */
    def a[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, true)
      }

    /**
     * This method enables the following syntax, where <code>fileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * fileMock should not { be a (file) }
     *                          ^
     * </pre>
     */
    def a[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName)), 
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * animal should not { be an ('elephant) }
     *                        ^
     * </pre>
     */
    def an[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, false)
      }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * keyEvent should not { be an (actionKey) }
     *                          ^
     * </pre>
     */
    def an[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName)),
            FailureMessages("wasAn", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables the following syntax for the "primitive" numeric types: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (7.1 plusOrMinus 0.2)
     *                      ^
     * </pre>
     */
    def apply[U](interval: Interval[U]): Matcher[U] =
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          MatchResult(
            interval.isWithin(left),
            // left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, interval.pivot, interval.tolerance),
            FailureMessages("wasPlusOrMinus", left, interval.pivot, interval.tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be theSameInstancreAs (anotherObject)
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          MatchResult(
            left eq right,
            FailureMessages("wasNotSameInstanceAs", left, right),
            FailureMessages("wasSameInstanceAs", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be (true)
     *                  ^
     * </pre>
     */
    def apply(right: Boolean): Matcher[Boolean] = 
      new Matcher[Boolean] {
        def apply(left: Boolean): MatchResult =
          MatchResult(
            left == right,
            FailureMessages("wasNot", left, right),
            FailureMessages("was", left, right)
          )
      }

/* Well heck if I don't need this one
      [fsc] both method apply in class BeWord of type [T](org.scalatest.BePropertyMatcher[T])org.scalatest.Matcher[T]
      [fsc] and  method apply in class BeWord of type [T](org.scalatest.BeMatcher[T])org.scalatest.Matcher[T]
      [fsc] match argument types (Null)
      [fsc]         o should be (null)
      [fsc]                  ^
*/

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be (null)
     *                  ^
     * </pre>
     */
    def apply(o: Null): Matcher[AnyRef] = 
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left == null,
            FailureMessages("wasNotNull", left),
            FailureMessages("wasNull"),
            FailureMessages("wasNotNull", left),
            FailureMessages("midSentenceWasNull")
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should be ('empty)
     *               ^
     * </pre>
     */
    def apply[T](right: AType[T]): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = 
          MatchResult(
            right.isAssignableFromClassOf(left),
            FailureMessages("wasNotAnInstanceOf", left, UnquotedString(right.className)),
            FailureMessages("wasAnInstanceOf"), // TODO, missing the left, right.className here. Write a test and fix it.
            FailureMessages("wasNotAnInstanceOf", left, UnquotedString(right.className)),
            FailureMessages("wasAnInstanceOf")
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should be ('empty)
     *               ^
     * </pre>
     */
    def apply[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, false, false)
      }

    /**
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should be (odd)
     *               ^
     * </pre>
     */
    def apply[T](right: BeMatcher[T]): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult = right(left)
      }

    /**
     * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
     *
     * <pre class="stHighlight">
     * door should be (open)
     *                ^
     * </pre>
     */
    def apply[T](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNot", left, UnquotedString(result.propertyName)), 
            FailureMessages("was", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables <code>be</code> to be used for equality comparison. Here are some examples: 
     *
     * <pre class="stHighlight">
     * result should be (None)
     *                  ^
     * result should be (Some(1))
     *                  ^
     * result should be (true)
     *                  ^
     * result should be (false)
     *                  ^
     * sum should be (19)
     *               ^
     * </pre>
     */
    def apply(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult =
          left match {
            case null =>
              MatchResult(
                right == null,
                FailureMessages("wasNotNull", right),
                FailureMessages("wasNull"),
                FailureMessages("wasNotNull", right),
                FailureMessages("midSentenceWasNull")
              )
            case _ => {
              val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
              MatchResult(
                areEqualComparingArraysStructurally(left, right),
                FailureMessages("wasNotEqualTo", leftee, rightee),
                FailureMessages("wasEqualTo", left, right)
              )
            }
        }
      }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class NotWord {

    /**
     * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>exist</code> is a <code>Matcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * tempFile should not (exist)
     *                     ^
     * </pre>
     */
    def apply[S <: Any](matcher: Matcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult =
          matcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * hasNoSize should not { have size (2) and equal (hasNoSize) }
     *                      ^
     * </pre>
     */
    def apply[S <: Any, TYPECLASS[_]](matcherGen1: MatcherGen1[S, TYPECLASS]): MatcherGen1[S, TYPECLASS] = {
      new MatcherGen1[S, TYPECLASS] {
        def matcher[V <: S : TYPECLASS]: Matcher[V] = {
          val innerMatcher: Matcher[V] = matcherGen1.matcher
          new Matcher[V] {
            def apply(left: V): MatchResult = {
              innerMatcher(left) match {
                case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
              }
            }
          }
        }
      }
    }

    /**
     * This method enables any <code>BeMatcher</code> to be negated by passing it to <code>not</code>. 
     * For example, if you have a <code>BeMatcher[Int]</code> called <code>odd</code>, which matches
     * <code>Int</code>s that are odd, you can negate it to get a <code>BeMatcher[Int]</code> that matches
     * even <code>Int</code>s, like this:
     *
     * <pre class="stHighlight">
     * val even = not (odd)
     *                ^
     * </pre>
     *
     * <p>
     * In addition, this method enables you to negate a <code>BeMatcher</code> at its point of use, like this:
     * </p>
     *
     * <pre class="stHighlight">
     * num should be (not (odd))
     * </pre>
     *
     * <p>
     * Nevertheless, in such as case it would be more idiomatic to write:
     * </p>
     *
     * <pre class="stHighlight">
     * num should not be (odd)
     * </pre>
     */
    def apply[S <: Any](beMatcher: BeMatcher[S]): BeMatcher[S] =
      new BeMatcher[S] {
        def apply(left: S): MatchResult =
          beMatcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * num should (not equal (7) and be < (9))
     *                 ^
     * </pre>
     */
    def equal(right: Any): Matcher[Any] = apply(matchers.legacyEqual(right))

    /**
     * This method enables the following syntax for the "primitive" numeric types: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should ((not equal (17.1 plusOrMinus 0.2)) and (not equal (27.1 plusOrMinus 0.2)))
     *                         ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): Matcher[U] = {
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          MatchResult(
            !(interval.isWithin(left)),
            FailureMessages("equaledPlusOrMinus", left, interval.pivot, interval.tolerance),
            FailureMessages("didNotEqualPlusOrMinus", left, interval.pivot, interval.tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should (not equal (null))
     *                 ^
     * </pre>
     */
    def equal(o: Null): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left != null,
            FailureMessages("equaledNull"),
            FailureMessages("didNotEqualNull", left),
            FailureMessages("midSentenceEqualedNull"),
            FailureMessages("didNotEqualNull", left)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have length (5) and not have length (3))
     *                         ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[AnyRef] =
      apply(matchers.have.length(resultOfLengthWordApplication.expectedLength))

    // This looks similar to the AndNotWord one, but not quite the same because no and
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have size (3))
     *                         ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[AnyRef] =
      apply(matchers.have.size(resultOfSizeWordApplication.expectedSize))

    /**
     * This method enables the following syntax, where, for example, <code>book</code> is of type <code>Book</code> and <code>title</code> and <code>author</code>
     * are both of type <code>HavePropertyMatcher[Book, String]</code>:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) and (not have (author ("Melville"))))
     *                  ^
     * </pre>
     */
    def have[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =
      apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*))

    /**
     * This method enables the following syntax, where, for example, <code>num</code> is an <code>Int</code> and <code>odd</code>
     * of type <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should (not be (odd) and be <= (8))
     *                 ^
     * </pre>
     */
    def be[T](beMatcher: BeMatcher[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          beMatcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should (not be (null))
     *                 ^
     * </pre>
     */
    def be(o: Null): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left != null,
            FailureMessages("wasNull"),
            FailureMessages("wasNotNull", left),
            FailureMessages("midSentenceWasNull"),
            FailureMessages("wasNotNull", left)
          )
        }
      }

    // These next four are for things like not be </>/<=/>=:
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be < (7) and not be > (10))
     *                 ^
     * </pre>
     */
    def be[T](resultOfLessThanComparison: ResultOfLessThanComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfLessThanComparison(left),
            FailureMessages("wasLessThan", left, resultOfLessThanComparison.right),
            FailureMessages("wasNotLessThan", left, resultOfLessThanComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be > (10) and not be < (7))
     *                 ^
     * </pre>
     */
    def be[T](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfGreaterThanComparison(left),
            FailureMessages("wasGreaterThan", left, resultOfGreaterThanComparison.right),
            FailureMessages("wasNotGreaterThan", left, resultOfGreaterThanComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be <= (7) and not be > (10))
     *                 ^
     * </pre>
     */
    def be[T](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfLessThanOrEqualToComparison(left),
            FailureMessages("wasLessThanOrEqualTo", left, resultOfLessThanOrEqualToComparison.right),
            FailureMessages("wasNotLessThanOrEqualTo", left, resultOfLessThanOrEqualToComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be >= (10) and not be < (7))
     *                 ^
     * </pre>
     */
    def be[T](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfGreaterThanOrEqualToComparison(left),
            FailureMessages("wasGreaterThanOrEqualTo", left, resultOfGreaterThanOrEqualToComparison.right),
            FailureMessages("wasNotGreaterThanOrEqualTo", left, resultOfGreaterThanOrEqualToComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * num should (not be === (7) and not be === (10))
     *                 ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[Any] = {
      new Matcher[Any] {
        def apply(left: Any): MatchResult =
          MatchResult(
            !(left == tripleEqualsInvocation.right),
            FailureMessages("wasEqualTo", left, tripleEqualsInvocation.right),
            FailureMessages("wasNotEqualTo", left, tripleEqualsInvocation.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFile should (not be ('hidden) and have (name ("temp.txt")))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](symbol: Symbol): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, symbol, false, false)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>hidden</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * tempFile should (not be (hidden) and have ('name ("temp.txt")))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("was", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNot", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('file) and have ('name ("temp.txt"))))
     *                           ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>notSoSecretFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>directory</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * notSoSecretFile should (not be a (directory) and have ('name ("passwords.txt")))
     *                             ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = resultOfAWordApplication.bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("wasA", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * isNotAppleMock should (not be an ('apple) and not be ('rotten))
     *                            ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) and not be an (directory))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = resultOfAnWordApplication.bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("wasAn", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFish should (not be theSameInstanceAs (redFish) and not be theSameInstanceAs (blueFish))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          MatchResult(
            resultOfTheSameInstanceAsApplication.right ne left,
            FailureMessages("wasSameInstanceAs", left, resultOfTheSameInstanceAsApplication.right),
            FailureMessages("wasNotSameInstanceAs", left, resultOfTheSameInstanceAsApplication.right)
          )
        }
      }
    }

    /**
     * This method enables the following syntax for the "primitive" numeric types: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should ((not be (17.1 plusOrMinus 0.2)) and (not be (27.1 plusOrMinus 0.2)))
     *                         ^
     * </pre>
     */
    def be[U](interval: Interval[U]): Matcher[U] = {
      new Matcher[U] {
        def apply(left: U): MatchResult = {
          MatchResult(
            // !(left <= right + tolerance && left >= right - tolerance),
            !(interval.isWithin(left)),
            FailureMessages("wasPlusOrMinus", left, interval.pivot, interval.tolerance),
            FailureMessages("wasNotPlusOrMinus", left, interval.pivot, interval.tolerance)
          )
        }
      }
    }

    /**
     * This method enables <code>be</code> to be used for inequality comparison. Here are some examples:
     *
     * <pre class="stHighlight">
     * result should not be (None)
     *                      ^
     * result should not be (Some(1))
     *                      ^
     * result should not be (true)
     *                      ^
     * result should not be (false)
     *                      ^
     * sum should not be (19)
     *                   ^
     * </pre>
     */
    def be(right: Any): Matcher[Any] = {
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          left match {
            case null =>
              MatchResult(
                right != null, 
                FailureMessages("wasNull"),
                FailureMessages("wasNotNull", right),
                FailureMessages("midSentenceWasNull"),
                FailureMessages("wasNotNull", right)
              )
            case _ => 
              MatchResult(
                !areEqualComparingArraysStructurally(left, right),
                FailureMessages("wasEqualTo", left, right),
                FailureMessages("wasNotEqualTo", left, right)
              )
          }
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not fullyMatch regex ("Hel*o") and not include ("orld"))
     *                    ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegexString = resultOfRegexWordApplication.regex.toString
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !java.util.regex.Pattern.matches(rightRegexString, left),
            FailureMessages("fullyMatchedRegex", left, UnquotedString(rightRegexString)),
            FailureMessages("didNotFullyMatchRegex", left, UnquotedString(rightRegexString))
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not include regex ("Hel.o") and not include regex ("""(-)?(\d+)(\.\d*)?"""))
     *                    ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !rightRegex.findFirstIn(left).isDefined,
            FailureMessages("includedRegex", left, rightRegex),
            FailureMessages("didNotIncludeRegex", left, rightRegex)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not include ("cat") and not include ("1.7"))
     *                    ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !(left.indexOf(expectedSubstring) >= 0), 
            FailureMessages("includedSubstring", left, expectedSubstring),
            FailureMessages("didNotIncludeSubstring", left, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not startWith regex ("hel*o") and not endWith regex ("wor.d"))
     *                    ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !rightRegex.pattern.matcher(left).lookingAt,
            FailureMessages("startedWithRegex", left, rightRegex),
            FailureMessages("didNotStartWithRegex", left, rightRegex)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should ((not startWith ("red")) and (not startWith ("1.7")))
     *                     ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left.indexOf(expectedSubstring) != 0,
            FailureMessages("startedWith", left, expectedSubstring),
            FailureMessages("didNotStartWith", left, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not endWith regex ("wor.d") and not startWith regex ("Hel*o"))
     *                    ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          val allMatches = rightRegex.findAllIn(left)
          MatchResult(
            !(allMatches.hasNext && (allMatches.end == left.length)),
            FailureMessages("endedWithRegex", left, rightRegex),
            FailureMessages("didNotEndWithRegex", left, rightRegex)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not endWith ("blue") and not endWith ("1.7"))
     *                    ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          MatchResult(
            !(left endsWith expectedSubstring),
            FailureMessages("endedWith", left, expectedSubstring),
            FailureMessages("didNotEndWith", left, expectedSubstring)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (5) and not contain (3))
     *                         ^
     * </pre>
     */
    def contain[T](expectedElement: T): Matcher[GenTraversable[T]] = {
      new Matcher[GenTraversable[T]] {
        def apply(left: GenTraversable[T]): MatchResult = {
          MatchResult(
            !(left.exists(_ == expectedElement)),
            FailureMessages("containedExpectedElement", left, expectedElement),
            FailureMessages("didNotContainExpectedElement", left, expectedElement)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain key ("three"))
     *                                         ^
     * </pre>
     */
    def contain[K](resultOfKeyWordApplication: ResultOfKeyWordApplication[K]): Matcher[scala.collection.GenMap[K, Any]] = {
      val expectedKey = resultOfKeyWordApplication.expectedKey
      new Matcher[scala.collection.GenMap[K, Any]] {
        def apply(left: scala.collection.GenMap[K, Any]): MatchResult = {
          MatchResult(
            !(left.exists(_._1 == expectedKey)),
            FailureMessages("containedKey", left, expectedKey),
            FailureMessages("didNotContainKey", left, expectedKey)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (3))
     *                                         ^
     * </pre>
     */
    def contain[K, V](resultOfValueWordApplication: ResultOfValueWordApplication[V]): Matcher[scala.collection.GenMap[K, V] forSome { type K }] = {
      val expectedValue = resultOfValueWordApplication.expectedValue
      new Matcher[scala.collection.GenMap[K, V] forSome { type K }] {
        def apply(left: scala.collection.GenMap[K, V] forSome { type K }): MatchResult = {
          MatchResult(
            !(left.exists(_._2 == expectedValue)),
            FailureMessages("containedValue", left, expectedValue),
            FailureMessages("didNotContainValue", left, expectedValue)
          )
        }
      }
    }
  }

  /**
   * This field enables syntax like the following: 
   *
   * <pre class="stHighlight">
   * myFile should (not be an (directory) and not have ('name ("foo.bar")))
   *                ^
   * </pre>
   */
  val not = new NotWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
   *             ^
   * </pre>
   */
  val be = new BeWord

/*
    In HaveWord's methods key, value, length, and size, I can give type parameters.
    The type HaveWord can contain a key method that takes a S or what not, and returns a matcher, which
    stores the key value in a val and whose apply method checks the passed map for the remembered key. This one would be used in things like:

    map should { have key 9 and have value "bob" }

    There's an overloaded should method on Shouldifier that takes a HaveWord. This method results in
    a different type that also has a key method that takes an S. So when you say:

    map should have key 9

    what happens is that this alternate should method gets invoked. The result is this other class that
    has a key method, and its constructor takes the map and stores it in a val. So this time when key is
    invoked, it checks to make sure the passed key is in the remembered map, and does the assertion.

    length and size can probably use structural types, because I want to use length on string and array for
    starters, and other people may create classes that have length methods. Would be nice to be able to use them.
  */

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (have length (3) and not contain ('a'))
   *              ^
   * </pre>
   */
  val have = new HaveWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (contain ('a') and have length (7))
   *              ^
   * </pre>
   */
  val contain = new ContainWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (include ("hope") and not startWith ("no"))
   *                ^
   * </pre>
   */
  val include = new IncludeWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (fullyMatch regex ("Hel*o, wor.d") and not have length (99))
   *                ^
   * </pre>
   */
  val fullyMatch = new FullyMatchWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (startWith ("Four") and include ("year"))
   *                ^
   * </pre>
   */
  val startWith = new StartWithWord

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (endWith ("ago") and include ("score"))
   *                ^
   * </pre>
   */
  val endWith = new EndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLengthWordApplication(val expectedLength: Long) extends HavePropertyMatcher[AnyRef, Long] {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "hi" should not have (length (3))
     *                      ^
     * </pre>
     *
     * <p>
     * This reason <code>ResultOfLengthWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
     * so that you don't have to remember whether <code>length</code> needs to be surrounded by parentheses when following
     * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
     * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
     * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>length</code>
     * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
     * will compile, but at will produce a <code>TestFailedException</code> at runtime.
     * </p>
     */
    def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

      accessProperty(objectWithProperty, 'length, false) match {

        case None =>

          throw newTestFailedException(Resources("propertyNotFound", "length", expectedLength.toString, "getLength"))

        case Some(result) =>

          new HavePropertyMatchResult[Long](
            result == expectedLength,
            "length",
            expectedLength,
            result match {
              case value: Byte => value.toLong
              case value: Short => value.toLong
              case value: Int => value.toLong
              case value: Long => value
              case _ => throw newTestFailedException(Resources("lengthPropertyNotAnInteger"))
            }
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class LengthWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "hi" should not have length (3)
     *                             ^
     * </pre>
     */
    def apply(expectedLength: Long): ResultOfLengthWordApplication = new ResultOfLengthWordApplication(expectedLength)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "hi" should not have length (3)
   *                      ^
   * </pre>
   */
  val length = new LengthWord
 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfSizeWordApplication(val expectedSize: Long) extends HavePropertyMatcher[AnyRef, Long] {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should not have (size (3))
     *                     ^
     * </pre>
     *
     * <p>
     * This reason <code>ResultOfSizeWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
     * so that you don't have to remember whether <code>size</code> needs to be surrounded by parentheses when following
     * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
     * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
     * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>size</code>
     * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
     * will compile, but at will produce a <code>TestFailedException</code> at runtime.
     * </p>
     */
    def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

      accessProperty(objectWithProperty, 'size, false) match {

        case None =>

          throw newTestFailedException(Resources("propertyNotFound", "size", expectedSize.toString, "getSize"))

        case Some(result) =>

          new HavePropertyMatchResult[Long](
            result == expectedSize,
            "size",
            expectedSize,
            result match {
              case value: Byte => value.toLong
              case value: Short => value.toLong
              case value: Int => value.toLong
              case value: Long => value
              case _ => throw newTestFailedException(Resources("sizePropertyNotAnInteger"))
            }
          )
      }
    }
  }


  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class SizeWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should not have size (3)
     *                          ^
     * </pre>
     */
    def apply(expectedSize: Long): ResultOfSizeWordApplication = new ResultOfSizeWordApplication(expectedSize)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should not have size (3)
   *                     ^
   * </pre>
   */
  val size = new SizeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfElementWordApplication[T](val expectedElement: T)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfKeyWordApplication[T](val expectedKey: T)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class KeyWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should not contain key (10)
     *                            ^
     * </pre>
     */
    def apply[T](expectedKey: T): ResultOfKeyWordApplication[T] = new ResultOfKeyWordApplication(expectedKey)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * map should not contain key (10)
   *                        ^
   * </pre>
   */
  val key = new KeyWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfValueWordApplication[T](val expectedValue: T)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ValueWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should not contain value (10)
     *                              ^
     * </pre>
     */
    def apply[T](expectedValue: T): ResultOfValueWordApplication[T] = new ResultOfValueWordApplication(expectedValue)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * map should not contain value (10)
   *                        ^
   * </pre>
   */
  val value = new ValueWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAWordToSymbolApplication(val symbol: Symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAWordToBePropertyMatcherApplication[T](val bePropertyMatcher: BePropertyMatcher[T])

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * badBook should not be a ('goodRead)
     *                         ^
     * </pre>
     */
    def apply(symbol: Symbol): ResultOfAWordToSymbolApplication = new ResultOfAWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>goodRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be a (goodRead)
     *                         ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAWordToBePropertyMatcherApplication[T] = new ResultOfAWordToBePropertyMatcherApplication(beTrueMatcher)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * badBook should not be a ('goodRead)
   *                       ^
   * </pre>
   */
  val a = new AWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAnWordToSymbolApplication(val symbol: Symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAnWordToBePropertyMatcherApplication[T](val bePropertyMatcher: BePropertyMatcher[T])

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AnWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * badBook should not be an ('excellentRead)
     *                          ^
     * </pre>
     */
    def apply(symbol: Symbol): ResultOfAnWordToSymbolApplication = new ResultOfAnWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>excellentRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be an (excellentRead)
     *                          ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAnWordToBePropertyMatcherApplication[T] = new ResultOfAnWordToBePropertyMatcherApplication(beTrueMatcher)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * badBook should not be an (excellentRead)
   *                       ^
   * </pre>
   */
  val an = new AnWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfTheSameInstanceAsApplication(val right: AnyRef)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class TheSameInstanceAsPhrase {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * oneString should not be theSameInstanceAs (anotherString)
     *                                           ^
     * </pre>
     */
    def apply(anyRef: AnyRef): ResultOfTheSameInstanceAsApplication = new ResultOfTheSameInstanceAsApplication(anyRef)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * oneString should not be theSameInstanceAs (anotherString)
   *                         ^
   * </pre>
   */
  val theSameInstanceAs: TheSameInstanceAsPhrase = new TheSameInstanceAsPhrase

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
   *                               ^
   * </pre>
   */
  val regex = new RegexWord

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not include substring ("seven")
   *                            ^
   * </pre>
  val substring = new SubstringWord
   */

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForSize[A <: AnyRef : Size](left: A, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

/*  I just added this whole thing in here for completeness when doing SizeShouldWrapper. Write some tests to prove it is needed.
// TODO: This should be for "sizey should not have size (12)" Try that test.
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForLength[A <: AnyRef : Length](left: A, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

/* TODO What's going on? Why can I drop this and still get a compile
// TODO: This should be for "lengthy should not have length (12)" Try that test.
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      if ((left.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
             if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              left,
              right
            )
          )
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForExtent[A : Extent](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Int)(implicit len: Length[A]) {
      // val len = implicitly[Length[A]]
      // if ((len.extentOf(left.asInstanceOf[A]) == expectedLength) != shouldBeTrue)
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Long)(implicit len: Length[A]) {
      // val len = implicitly[Length[A]]
      // if ((len.extentOf(left.asInstanceOf[A]) == expectedLength) != shouldBeTrue)
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Int)(implicit sz: Size[A]) {
      // val sz = implicitly[Size[T]]
      // if ((sz.extentOf(left.asInstanceOf[T]) == expectedSize) != shouldBeTrue)
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2L)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Long)(implicit sz: Size[A]) {
      // val sz = implicitly[Size[T]]
      // if ((sz.extentOf(left.asInstanceOf[T]) == expectedSize) != shouldBeTrue)
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }

/*
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForLength[A : Length](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Int) {
      val len = implicitly[Length[A]]
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Long) {
      val len = implicitly[Length[A]]
      if ((len.extentOf(left) == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
            left,
            expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForSize[A : Size](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Int) {
      val sz = implicitly[Size[A]]
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2L)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Long) {
      val sz = implicitly[Size[A]]
      if ((sz.extentOf(left) == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
            left,
            expectedSize)
        )
    }
  }
*/

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLessThanComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be < (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be < (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left < right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfGreaterThanComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be > (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be > (10) and not be < (7))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left > right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLessThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be <= (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be <= (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left <= right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfGreaterThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be >= (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be >= (10) and not be < (7))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left >= right
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
/* TODEL
  final class ResultOfTripleEqualsApplication(val right: Any) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be === (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: Any): Boolean = left == right
  }
*/

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be < (10) and not be > (17))
   *                    ^
   * </pre>
   */
  def <[T <% Ordered[T]] (right: T): ResultOfLessThanComparison[T] =
    new ResultOfLessThanComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be > (10) and not be < (7))
   *                    ^
   * </pre>
   */
  def >[T <% Ordered[T]] (right: T): ResultOfGreaterThanComparison[T] =
    new ResultOfGreaterThanComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be <= (10) and not be > (17))
   *                    ^
   * </pre>
   */
  def <=[T <% Ordered[T]] (right: T): ResultOfLessThanOrEqualToComparison[T] =
    new ResultOfLessThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be >= (10) and not be < (7))
   *                    ^
   * </pre>
   */
  def >=[T <% Ordered[T]] (right: T): ResultOfGreaterThanOrEqualToComparison[T] =
    new ResultOfGreaterThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should not be === (10)
   *                   ^
   * </pre>
   */
/* TODEL
  def === (right: Any): ResultOfTripleEqualsApplication =
    new ResultOfTripleEqualsApplication(right)
*/

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfEvaluatingApplication(val fun: () => Any)

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  def evaluating(fun: => Any): ResultOfEvaluatingApplication =
    new ResultOfEvaluatingApplication(fun _)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfProduceInvocation[T](val clazz: Class[T])

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  def produce[T](implicit manifest: Manifest[T]): ResultOfProduceInvocation[T] =
    new ResultOfProduceInvocation(manifest.erasure.asInstanceOf[Class[T]])

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForTraversable[T](left: GenTraversable[T], shouldBeTrue: Boolean = true) {
    
    private[scalatest] def apply(containMatcher: ContainMatcher[T]) {
      val result = containMatcher(left)
      if (result.matches != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
          None, 
          3
        )
    }
  
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain theSameElementsAs anotherTraversable
     *                            ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[T]) {
      apply(new TheSameElementsAsContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain theSameIteratedElementsAs anotherTraversable
     *                            ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[T]) {
      apply(new TheSameIteratedElementsAsContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain allOf (1, 2)
     *                            ^
     * </pre>
     */
    def allOf(right: T*) {
      apply(new AllOfContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain inOrder (1, 2)
     *                            ^
     * </pre>
     */
    def inOrder(right: T*) {
      apply(new InOrderContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain oneOf (1, 2)
     *                            ^
     * </pre>
     */
    def oneOf(right: T*) {
      apply(new OneOfContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain only (1, 2)
     *                            ^
     * </pre>
     */
    def only(right: T*) {
      apply(new OnlyContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain inOrderOnly (1, 2)
     *                            ^
     * </pre>
     */
    def inOrderOnly(right: T*) {
      apply(new InOrderOnlyContainMatcher(right))
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain noneOf (1, 2)
     *                            ^
     * </pre>
     */
    def noneOf(right: T*) {
      apply(new NoneOfContainMatcher(right))
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class TheSameElementsAsContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    @tailrec
    private def checkEqual(left: Iterator[T], right: Iterator[T], remains: IndexedSeq[T]): Boolean = {
      if (left.hasNext) {
        val nextLeft = left.next
        // Let's look from the remains first
        val idx = remains.indexOf(nextLeft)
        if (idx >= 0) {
          // Found in remains, let's remove it from remains and continue
          val (first, second) = remains.splitAt(idx)
          checkEqual(left, right, first ++: second.tail)
        }
        else {
          // Not found in remains, let's try right iterator
          if (right.isEmpty) // right is empty, so the element not found
            false
          else {
            val newRemains = right.takeWhile(_ != nextLeft)
            checkEqual(left, right, remains ++: newRemains.toIndexedSeq)
          }
        }
      }
      else
        right.isEmpty && remains.isEmpty
    }
    
    /**
     * This method contains the matching code for theSameElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = 
      MatchResult(
        checkEqual(left.toIterator, right.toIterator, IndexedSeq.empty), 
        FailureMessages("didNotContainSameElements", left, right), 
        FailureMessages("containedSameElements", left, right)
      )
    
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * traversable should contain (theSameElementsAs(anotherTraversable))
   *                             ^
   * </pre>
   */
  def theSameElementsAs[T](xs: GenTraversable[T]) = 
    new TheSameElementsAsContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class TheSameIteratedElementsAsContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    @tailrec
    private def checkEqual(left: Iterator[T], right: Iterator[T]): Boolean = {
      if (left.hasNext && right.hasNext) {
        val nextLeft = left.next
        val nextRight = right.next
        if (nextLeft != nextRight)
          false
        else
          checkEqual(left, right)
      }
      else
        left.isEmpty && right.isEmpty
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = 
      MatchResult(
        checkEqual(left.toIterator, right.toIterator), 
        FailureMessages("didNotContainSameIteratedElements", left, right), 
        FailureMessages("containedSameIteratedElements", left, right)
      )
    
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * traversable should contain (theSameIteratedElementsAs(anotherTraversable))
   *                             ^
   * </pre>
   */
  def theSameIteratedElementsAs[T](xs: GenTraversable[T]) = 
    new TheSameIteratedElementsAsContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class AllOfContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    @tailrec
    private def checkEqual(left: GenTraversable[T], rightItr: Iterator[T], processedSet: Set[T]): Boolean = {
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.contains(nextRight))
          throw new IllegalArgumentException(FailureMessages("allOfDuplicate", nextRight))
        if (left.exists(_ == nextRight)) 
          checkEqual(left, rightItr, processedSet + nextRight)
        else
          false // Element not found, let's fail early
      }
      else // No more element in right, left contains all of right.
        true
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = 
      MatchResult(
        checkEqual(left, right.toIterator, Set.empty), 
        FailureMessages("didNotContainAllOfElements", left, UnquotedString(right.mkString(", "))),
        FailureMessages("containedAllOfElements", left, UnquotedString(right.mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (allOf(1, 2))
   *                               ^
   * </pre>
   */
  def allOf[T](xs: T*) = 
    new AllOfContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class InOrderContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    
    @tailrec
    private def lastIndexOf(itr: Iterator[T], element: T, idx: Option[Int], i: Int): Option[Int] = {
      if (itr.hasNext) {
        val next = itr.next
        if (next == element)
          lastIndexOf(itr, element, Some(i), i + 1)
        else
          lastIndexOf(itr, element, idx, i + 1)
      }
      else
        idx
    }
    
    @tailrec
    private def checkEqual(left: GenTraversable[T], rightItr: Iterator[T], processedSet: Set[T]): Boolean = {
      
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.contains(nextRight))
            throw new IllegalArgumentException(FailureMessages("inOrderDuplicate", nextRight))
        lastIndexOf(left.toIterator, nextRight, None, 0) match {
          case Some(idx) => 
            checkEqual(left.drop(idx).tail, rightItr, processedSet + nextRight)
          case None => 
            false // Element not found, let's fail early
        }
      }
      else // No more element in right, left contains all of right.
        true
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = 
      MatchResult(
        checkEqual(left, right.toIterator, Set.empty), 
        FailureMessages("didNotContainAllOfElementsInOrder", left, UnquotedString(right.mkString(", "))),
        FailureMessages("containedAllOfElementsInOrder", left, UnquotedString(right.mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrder(1, 2))
   *                               ^
   * </pre>
   */
  def inOrder[T](xs: T*) = 
    new InOrderContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class OneOfContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    
    @tailrec
    private def checkEqual(left: GenTraversable[T], rightItr: Iterator[T], processedSet: Set[T]): Boolean = {
      
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.contains(nextRight))
            throw new IllegalArgumentException(FailureMessages("oneOfDuplicate", nextRight))
        if (left.exists(_ == nextRight)) // Found one of right in left, can succeed early
          true
        else
          checkEqual(left, rightItr, processedSet + nextRight)
      }
      else // No more element in right, left does not contain one of right.
        false
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = 
      MatchResult(
        checkEqual(left, right.toIterator, Set.empty), 
        FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.mkString(", "))),
        FailureMessages("containedOneOfElements", left, UnquotedString(right.mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (oneOf(1, 2))
   *                               ^
   * </pre>
   */
  def oneOf[T](xs: T*) = 
    new OneOfContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class OnlyContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    
    @tailrec
    private def findNext(value: T, rightItr: Iterator[T], processedSet: Set[T]): Set[T] = 
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.contains(nextRight))
            throw new IllegalArgumentException(FailureMessages("onlyDuplicate", nextRight))
        if (nextRight == value)
          processedSet + nextRight
        else
          findNext(value, rightItr, processedSet + nextRight)
      }
      else
        processedSet
     
    @tailrec
    private def checkEqual(leftItr: Iterator[T], rightItr: Iterator[T], processedSet: Set[T]): Boolean = {
      if (leftItr.hasNext) {
        val nextLeft = leftItr.next
        if (processedSet.contains(nextLeft)) // The nextLeft is contained in right, let's continue next
          checkEqual(leftItr, rightItr, processedSet)
        else {
          val newProcessedSet = findNext(nextLeft, rightItr, processedSet)
          if (newProcessedSet.contains(nextLeft)) // The nextLeft is contained in right, let's continue next
            checkEqual(leftItr, rightItr, newProcessedSet)
          else // The nextLeft is not in right, let's fail early
            false
        }
      }
      else // No more element in left, left contains only elements of right.
        true
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = 
      MatchResult(
        checkEqual(left.toIterator, right.toIterator, Set.empty), 
        FailureMessages("didNotContainOnlyElements", left, UnquotedString(right.mkString(", "))),
        FailureMessages("containedOnlyElements", left, UnquotedString(right.mkString(", ")))
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (only(1, 2))
   *                               ^
   * </pre>
   */
  def only[T](xs: T*) = 
    new OnlyContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class InOrderOnlyContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    
    @tailrec
    private def findNext(value: T, rightItr: Iterator[T], processedList: IndexedSeq[T]): IndexedSeq[T] = 
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedList.contains(nextRight))
            throw new IllegalArgumentException(FailureMessages("inOrderOnlyDuplicate", nextRight))
        if (nextRight == value)
          processedList :+ nextRight
        else
          findNext(value, rightItr, processedList :+ nextRight)
      }
      else
        processedList
    
    @tailrec
    private def checkEqual(leftItr: Iterator[T], rightItr: Iterator[T], currentRight: T, processedList: IndexedSeq[T]): Boolean = {
      
      if (leftItr.hasNext) {
        val nextLeft = leftItr.next
        if (nextLeft == currentRight) // The nextLeft is contained in right, let's continue next
          checkEqual(leftItr, rightItr, currentRight, processedList)
        else {
          val newProcessedList = findNext(nextLeft, rightItr, processedList)
          if (newProcessedList.last == nextLeft) // The nextLeft is contained in right, let's continue next
            checkEqual(leftItr, rightItr, nextLeft, newProcessedList) // nextLeft will be the new currentRight
          else // The nextLeft is not in right, let's fail early
            false
        }
      }
      else // No more element in left, left contains only elements of right.
        true
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = {
      val rightItr = right.toIterator
      val rightFirst = rightItr.next
      MatchResult(
        if (rightItr.hasNext) checkEqual(left.toIterator, rightItr, rightFirst, IndexedSeq(rightFirst)) else left.isEmpty, 
        FailureMessages("didNotContainInOrderOnlyElements", left, UnquotedString(right.mkString(", "))),
        FailureMessages("containedInOrderOnlyElements", left, UnquotedString(right.mkString(", ")))
      )
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrderOnly(1, 2))
   *                               ^
   * </pre>
   */
  def inOrderOnly[T](xs: T*) = 
    new InOrderOnlyContainMatcher(xs)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class NoneOfContainMatcher[T](right: GenTraversable[T]) extends ContainMatcher[T] {
    
    @tailrec
    private def findNext(value: T, rightItr: Iterator[T], processedSet: Set[T]): Set[T] = 
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.contains(nextRight))
            throw new IllegalArgumentException(FailureMessages("noneOfDuplicate", nextRight))
        if (nextRight == value)
          processedSet + nextRight
        else
          findNext(value, rightItr, processedSet + nextRight)
      }
      else
        processedSet
    
    @tailrec
    private def checkEqual(leftItr: Iterator[T], rightItr: Iterator[T], processedSet: Set[T]): Boolean = {
      
      if (leftItr.hasNext) {
        val nextLeft = leftItr.next
        if (processedSet.contains(nextLeft)) // nextLeft is found in right, let's fail early
          false
        else {
          val newProcessedSet = findNext(nextLeft, rightItr, processedSet)
          if (newProcessedSet.contains(nextLeft)) // nextLeft is found in right, let's fail early
            false
          else // nextLeft not found in right, let's continue to next element in left
            checkEqual(leftItr, rightItr, newProcessedSet)
        }
      }
      else // No more element in left, left contains only elements of right.
        true
    }
    
    /**
     * This method contains the matching code for theSameIteratedElementsAs.
     */
    def apply(left: GenTraversable[T]): MatchResult = {
      MatchResult(
        checkEqual(left.toIterator, right.toIterator, Set.empty), 
        FailureMessages("containedOneOfElements", left, UnquotedString(right.mkString(", "))),
        FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.mkString(", ")))
      )
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (noneOf(1, 2))
   *                               ^
   * </pre>
   */
  def noneOf[T](xs: T*) = 
    new NoneOfContainMatcher(xs)
  
  // For safe keeping
  private implicit def nodeToCanonical(node: scala.xml.Node) = new Canonicalizer(node)

  private class Canonicalizer(node: scala.xml.Node) {

    def toCanonical: scala.xml.Node = {
      node match {
        case elem: scala.xml.Elem =>
          val canonicalizedChildren =
            for (child <- node.child if !child.toString.trim.isEmpty) yield {
              child match {
                case elem: scala.xml.Elem => elem.toCanonical
                case other => other
              }
            }
          new scala.xml.Elem(elem.prefix, elem.label, elem.attributes, elem.scope, canonicalizedChildren: _*)
        case other => other
      }
    }
  }

  class AType[T : ClassManifest] {

    private val clazz = implicitly[ClassManifest[T]].erasure.asInstanceOf[Class[T]]

    def isAssignableFromClassOf(o: Any): Boolean = clazz.isAssignableFrom(o.getClass)

    def className: String = clazz.getName
  }

  def a[T : ClassManifest]: AType[T] = new AType[T]

  // This is where InspectorShorthands started

  import org.scalatest.InspectorsHelper._
  
  private sealed trait Collected
  private case object AllCollected extends Collected
  private case object EveryCollected extends Collected
  private case class BetweenCollected(from: Int, to: Int) extends Collected
  private case class AtLeastCollected(num: Int) extends Collected
  private case class AtMostCollected(num: Int) extends Collected
  private case object NoCollected extends Collected
  private case class ExactlyCollected(num: Int) extends Collected
  
  def doCollected[T](collected: Collected, xs: GenTraversable[T], methodName: String, stackDepth: Int)(fun: T => Unit) {
    collected match {
      case AllCollected =>
        forAll(xs, "Matchers.scala", methodName, stackDepth) { e => 
          fun(e)
        }
      case AtLeastCollected(num) => 
        forAtLeast(num, xs, "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case EveryCollected => 
        forEvery(xs, "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case ExactlyCollected(num) => 
        forExactly(num, xs, "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case NoCollected =>
        forNo(xs, "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case BetweenCollected(from, to) =>
        forBetween(from, to, xs, "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case AtMostCollected(num) =>
        forAtMost(num, xs, "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedAny[T](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) {

    import org.scalatest.InspectorsHelper._
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (7)
     *                    ^
     * </pre>
     */
    def equal(right: Any) {
      doCollected(collected, xs, "equal", 1) { e =>
        if ((e == right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEqual" else "equaled",
              e,
              right
            ), 
            None, 
            11
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (7)
     *                    ^
     * </pre>
     */
    def be(right: Any) {
      doCollected(collected, xs, "be", 1) { e =>
        if ((e == right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
              e,
              right
            ), 
            None, 
            11
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be <= (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            11
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be >= (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            11
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be < (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
              e,
              comparison.right
            ), 
            None, 
            11
          ) 
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be > (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
              e,
              comparison.right
            ), 
            None, 
            11
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be === (7)
     *                    ^
     * </pre>
     */
    def be(comparison: TripleEqualsInvocation[_]) {
      doCollected(collected, xs, "be", 1) { e => 
        if ((e == comparison.right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
              e,
              comparison.right
            ), 
            None, 
            11
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to
     * a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (odd)
     *                    ^
     * </pre>
     */
    def be(beMatcher: BeMatcher[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = beMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              result.failureMessage
            else
              result.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
     * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (empty)
     *                    ^
     * </pre>
     */
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNot", e, UnquotedString(result.propertyName))
            else
              FailureMessages("was", e, UnquotedString(result.propertyName)), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be a (file)
     *                    ^
     * </pre>
     */
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = resultOfAWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotA", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasA", e, UnquotedString(result.propertyName)), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * all(keyEvents) should not be an (actionKey)
     *                           ^
     * </pre>
     */
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
      doCollected(collected, xs, "be", 1) { e => 
        val result = resultOfAnWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotAn", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasAn", e, UnquotedString(result.propertyName)), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be theSameInstanceAs (string)
     *                    ^
     * </pre>
     */
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        e match {
          case ref: AnyRef =>
            if ((resultOfSameInstanceAsApplication.right eq ref) != shouldBeTrue) {
              throw newTestFailedException(
                FailureMessages(
                  if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
                  e,
                  resultOfSameInstanceAsApplication.right
                ), 
                None, 
                11
              )
            }
          case _ => 
            throw new IllegalArgumentException("theSameInstanceAs should only be used for AnyRef")
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should not have (title ("One Hundred Years of Solitude"))
     *                       ^
     * </pre>
     */
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {
      doCollected(collected, xs, "have", 1) { e => 
      
        val results =
          for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
            propertyVerifier(e)

        val firstFailureOption = results.find(pv => !pv.matches)

        val justOneProperty = propertyMatchers.length == 0

        // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
        // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
        if (firstFailureOption.isDefined == shouldBeTrue) {
          firstFailureOption match {
            case Some(firstFailure) =>
              // This is one of these cases, thus will only get here if shouldBeTrue is true
              // 0 0 | 0 | 1
              // 0 1 | 0 | 1
              // 1 0 | 0 | 1
              throw newTestFailedException(
                FailureMessages(
                  "propertyDidNotHaveExpectedValue",
                  UnquotedString(firstFailure.propertyName),
                  firstFailure.expectedValue,
                  firstFailure.actualValue,
                  e
                ), 
                None, 
                11
              )
            case None =>
              // This is this cases, thus will only get here if shouldBeTrue is false
              // 1 1 | 1 | 0
              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "propertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    e
                  )
                }
                else FailureMessages("allPropertiesHadExpectedValues", e)

              throw newTestFailedException(failureMessage, None, 11)
          } 
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
    extends ResultOfNotWordForCollectedAny(collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (null)
     *                    ^
     * </pre>
     */
    def be(o: Null) {
      doCollected(collected, xs, "be", 1) { e => 
        if ((e == null) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotNull", e) 
            else
              FailureMessages("wasNull"), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be ('empty)
     *                    ^
     * </pre>
     */
    def be(symbol: Symbol) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, false, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be a ('file)
     *                    ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, resultOfAWordApplication.symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be an ('actionKey)
     *                    ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              11
            )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[String](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not have length (12)
     *                        ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not have size (12)
     *                        ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not startWith ("1.7")
     *                        ^
     * </pre>
     */
    def startWith(right: String) {
      doCollected(collected, xs, "startWith", 1) { e =>
        if ((e.indexOf(right) == 0) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWith" else "startedWith",
              e,
              right
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not startWith regex ("Hel*o")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "startWith", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.pattern.matcher(e).lookingAt != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not endWith ("1.7")
     *                        ^
     * </pre>
     */
    def endWith(expectedSubstring: String) {
      doCollected(collected, xs, "endWith", 1) { e =>
        if ((e endsWith expectedSubstring) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWith" else "endedWith",
              e,
              expectedSubstring
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not endWith regex ("wor.d")
     *                        ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "endWith", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        val allMatches = rightRegex.findAllIn(e)
        if (allMatches.hasNext && (allMatches.end == e.length) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not include regex ("wo.ld")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "include", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.findFirstIn(e).isDefined != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not include ("world")
     *                        ^
     * </pre>
     */
    def include(expectedSubstring: String) {
      doCollected(collected, xs, "include", 1) { e =>
        if ((e.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeSubstring" else "includedSubstring",
              e,
              expectedSubstring
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "fullyMatch", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.pattern.matcher(e).matches != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
              e,
              rightRegex
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedGenTraversable[E, T <: GenTraversable[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not have size (12)
     *                                          ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not have length (12)
     *                                          ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not contain ("one")
     *                                          ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.exists(_ == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedGenSeq[E, T <: GenSeq[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedGenTraversable[E, T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(seqOfSeq) should not have length (12)
     *                          ^
     * </pre>
     */
    override def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.length == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedArray[E, T <: Array[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be ('empty)
     *                            ^
     * </pre>
     */
    override def be(symbol: Symbol) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, symbol, false, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be a ('file)
     *                            ^
     * </pre>
     */
    override def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, resultOfAWordApplication.symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be an ('actionKey)
     *                            ^
     * </pre>
     */
    override def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              10
            )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfArray) should not have size (12)
     *                                    ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfArray) should not contain ("one")
     *                                    ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.exists(_ == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(seqOfArray) should not have length (12)
     *                            ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedGenMap[K, V, T <: GenMap[K, V]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedGenTraversable[(K, V), T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain key ("three")
     *                          ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfKeyWordApplication.expectedKey
        if ((e.exists(_._1 == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain value (3)
     *                          ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfValueWordApplication.expectedValue
        if ((e.exists(_._2 == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfNotWordForCollectedJavaCollection[E, T <: java.util.Collection[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not have size (3)
     *                                     ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not have length (12)
     *                                     ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not contain ("elephant")
     *                                     ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.contains(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedJavaMap[K, V, T <: java.util.Map[K, V]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue){
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have size (3)
     *                              ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have length (12)
     *                              ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        if ((e.size == right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not contain key ("three")
     *                              ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfKeyWordApplication.expectedKey
        if ((e.containsKey(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not contain value (3)
     *                              ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfValueWordApplication.expectedValue
        if ((e.containsValue(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              right
            ), 
            None, 
            11
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfBeWordForCollectedAny[T](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfBeWordForCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
    extends ResultOfBeWordForCollectedAny(collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      doCollected(collected, xs, "theSameInstanceAs", 1) { e =>
        if ((e eq right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
              e,
              right
            ),
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be a ('file)
     *                   ^
     * </pre>
     */
    def a(symbol: Symbol) {
      doCollected(collected, xs, "a", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            11
          )
        }
      }
    }
    
    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be an ('orange)
     *                   ^
     * </pre>
     */
    def an(symbol: Symbol) {
      doCollected(collected, xs, "an", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            11
          )
        }
      }
    }
    
    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be a (goodRead)
     *                      ^
     * </pre>
     */
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "a", 1) { e =>
        val result = bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotA", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasA", e, UnquotedString(result.propertyName)), 
            None, 
            11
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be an (excellentRead)
     *                      ^
     * </pre>
     */
    def an(beTrueMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "an", 1) { e =>
        val beTrueMatchResult = beTrueMatcher(e)
        if (beTrueMatchResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotAn", e, UnquotedString(beTrueMatchResult.propertyName))
            else
              FailureMessages("wasAn", e, UnquotedString(beTrueMatchResult.propertyName)), 
            None, 
            11
          )
        }
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfBeWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) 
    extends ResultOfBeWordForCollectedAnyRef(collected, xs, shouldBeTrue) {
  
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should be ('empty)
     *                           ^
     * </pre>
     */
    def apply(right: Symbol): Matcher[Array[T]] =
      new Matcher[Array[T]] {
        def apply(left: Array[T]): MatchResult = matchSymbolToPredicateMethod(left.deep, right, false, false)
      }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) {
  
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should contain (element)
     *                        ^
     * </pre>
     */
    def apply(expectedElement: T): Matcher[Array[T]] = 
      new Matcher[Array[T]] {
        def apply(left: Array[T]): MatchResult =
          MatchResult(
            left.exists(_ == expectedElement), 
            FailureMessages("didNotContainExpectedElement", left, expectedElement),
            FailureMessages("containedExpectedElement", left, expectedElement)
          )
      }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfCollectedAny[T](collected: Collected, xs: GenTraversable[T]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be (3)
     *         ^
     * </pre>
     */
    def should(rightMatcher: Matcher[T]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[T, Equality])(implicit equality: Equality[T]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAny[T](collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedAny[T] = 
      new ResultOfNotWordForCollectedAny(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be (3)
     *         ^
     * </pre>
     */
    def should(rightMatcher: Matcher[T]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[T, Equality])(implicit equality: Equality[T]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef[T](collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedAnyRef[T] = 
      new ResultOfNotWordForCollectedAnyRef(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedString(collected: Collected, xs: GenTraversable[String]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should be ("hi")
     *             ^
     * </pre>
     */
    def should(rightMatcher: Matcher[String]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[String, Equality])(implicit equality: Equality[String]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should be theSameInstanceAs anotherObject
     *             ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should have length (3)
     *             ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedString = 
      new ResultOfHaveWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should not have length (3)
     *             ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedString = 
      new ResultOfNotWordForCollectedString(collected, xs, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *             ^
     * </pre>
     */
    def should(startWithWord: StartWithWord): ResultOfStartWithWordForCollectedString = 
      new ResultOfStartWithWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(endWithWord: EndWithWord): ResultOfEndWithWordForCollectedString = 
      new ResultOfEndWithWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(includeWord: IncludeWord): ResultOfIncludeWordForCollectedString = 
      new ResultOfIncludeWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForCollectedString = 
      new ResultOfFullyMatchWordForCollectedString(collected, xs, true)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should have length (12)
     *                         ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.length == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should have size (12)
     *                         ^
     * </pre>
     */
    def size(expectedSize: Int) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfStartWithWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *                              ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o".r)
     *                              ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.pattern.matcher(e).lookingAt != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            12
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfIncludeWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("world")
     *                            ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.findFirstIn(e).isDefined != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
              e,
              rightRegex
            ), 
            None, 
            12
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfEndWithWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d")
     *                            ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        val allMatches = rightRegex.findAllIn(e)
        if ((allMatches.hasNext && (allMatches.end == e.length)) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            12
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfFullyMatchWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("Hel*o world")
     *                              ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullymatch regex ("Hel*o world".r)
     *                               ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.pattern.matcher(e).matches != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
              e,
              rightRegex
            ), 
            None, 
            12
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have size (3)
     *                       ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedGenTraversable[T] = 
      new ResultOfHaveWordForCollectedGenTraversable(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be (Set(1, 2, 3))
     *                       ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenTraversable[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should equal (3)
     *                       ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[GenTraversable[T], Equality])(implicit equality: Equality[GenTraversable[T]]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be theSameInstanceAs anotherObject
     *                       ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should not have size (3)
     *                       ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenTraversable[T, GenTraversable[T]] = 
      new ResultOfNotWordForCollectedGenTraversable(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have size (12)
     *                                   ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have length (12)
     *                                   ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.size == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedGenSeq[T](collected: Collected, xs: GenTraversable[GenSeq[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have length (3)
     *               ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedGenSeq[T] = 
      new ResultOfHaveWordForCollectedGenSeq(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should be (List(1, 2, 3))
     *               ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenSeq[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[GenSeq[T], Equality])(implicit equality: Equality[GenSeq[T]]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should be theSameInstanceAs anotherObject
     *               ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should not have length (3)
     *               ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenSeq[T, GenSeq[T]] = 
      new ResultOfNotWordForCollectedGenSeq(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedGenSeq[T](collected: Collected, xs: GenTraversable[GenSeq[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have length (12)
     *                           ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.length == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have size (12)
     *                           ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have size (3)
     *                 ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedArray[T] = 
      new ResultOfHaveWordForCollectedArray(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be (Set(1, 2, 3))
     *                       ^
     * </pre>
     */
    def should[T](rightMatcher: Matcher[GenTraversable[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e.deep.asInstanceOf[IndexedSeq[T]]) match {  // TODO: Ugly but safe cast here because e is Array[T]
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should equal (3)
     *                       ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[GenTraversable[T], Equality])(implicit equality: Equality[GenTraversable[T]]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e.deep.asInstanceOf[IndexedSeq[T]]) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should be theSameInstanceAs anotherObject
     *                 ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedArray(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not have size (3)
     *                 ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedArray[T, Array[T]] = 
      new ResultOfNotWordForCollectedArray(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have size (12)
     *                             ^
     * </pre>
     */
    def size(expectedSize: Int) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize
            ), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have length (12)
     *                             ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.length == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength
            ), 
            None, 
            11
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedGenMap[K, V](collected: Collected, xs: GenTraversable[GenMap[K, V]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key (10)
     *               ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedGenMap[K, V] = 
      new ResultOfContainWordForCollectedGenMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should be (Map(1 -> "one", 2 -> "two"))
     *               ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenMap[K, V]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should equal (3)
     *               ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[GenMap[K, V], Equality])(implicit equality: Equality[GenMap[K, V]]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should be theSameInstanceAs (anotherMap)
     *               ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not have size (3)
     *               ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenMap[K, V, GenMap[K, V]] = 
      new ResultOfNotWordForCollectedGenMap(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedGenMap[K, V](collected: Collected, xs: GenTraversable[GenMap[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key ("one")
     *                              ^
     * </pre>
     */
    def key(expectedKey: K) {
      doCollected(collected, xs, "key", 1) { e =>
        if (e.exists(_._1 == expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              expectedKey), 
              None, 
              11
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain value (1)
     *                              ^
     * </pre>
     */
    def value(expectedValue: V) {
      doCollected(collected, xs, "value", 1) { e =>
        if (e.exists(expectedValue == _._2) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              expectedValue), 
            None, 
            11
          )
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have size (3)
     *                   ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedJavaCollection[T] = 
      new ResultOfHaveWordForCollectedJavaCollection(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should be (aJavaSet)
     *                   ^
     * </pre>
     */
    def should(rightMatcher: Matcher[java.util.Collection[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should equal (3)
     *                   ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[java.util.Collection[T], Equality])(implicit equality: Equality[java.util.Collection[T]]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should not have size (3)
     *                   ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedJavaCollection[T, java.util.Collection[T]] = 
      new ResultOfNotWordForCollectedJavaCollection(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have size (10)
     *                   ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize), 
            None, 
            11
          )
       }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have length (12)
     *                   ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.size == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength), 
            None, 
            11
          )
        }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have size (3)
     *                   ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedJavaMap[K, V] = 
      new ResultOfHaveWordForCollectedJavaMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain value (3)
     *                   ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedJavaMap[K, V] = 
      new ResultOfContainWordForCollectedJavaMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should be (someJavaMap)
     *                   ^
     * </pre>
     */
    def should(rightMatcher: Matcher[java.util.Map[K, V]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 11)
          case _ => ()
        }
      }
    }
    
    // TODO: To implement it correctly using Equality
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should equal (3)
     *                   ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[java.util.Map[K, V], Equality])(implicit equality: Equality[java.util.Map[K, V]]) {
      val rightMatcher = rightMatcherGen1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have length (3)
     *                   ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedJavaMap[K, V, java.util.Map[K, V]] = 
      new ResultOfNotWordForCollectedJavaMap(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have size (10)
     *                               ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        if ((e.size == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedSize" else "hadExpectedSize",
              e,
              expectedSize), 
            None, 
            11
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have length (10)
     *                               ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        if ((e.size == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotHaveExpectedLength" else "hadExpectedLength",
              e,
              expectedLength), 
            None, 
            11
          )
      }
    }
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain key ("two")
     *                        ^
     * </pre>
     */
    def key(expectedKey: K) {
      doCollected(collected, xs, "key", 1) { e =>
        if (e.containsKey(expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              expectedKey), 
            None, 
            11
          )
      }
    }

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain value ("2")
     *                        ^
     * </pre>
     */
    def value(expectedValue: V) {
      doCollected(collected, xs, "value", 1) { e =>
        if (e.containsValue(expectedValue) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              expectedValue), 
              None, 
              11
          )
      }
    }
    
  }

  def all[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(AllCollected, xs)
  
  def all(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(AllCollected, xs)
  
  def all(xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(AllCollected, xs)
  
  def all[T](xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(AllCollected, xs)
  
  def all[T](xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(AllCollected, xs)
  
  def all[T](xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(AllCollected, xs)
  
  def all[K, V](xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(AllCollected, xs)
  
  def all[T](xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(AllCollected, xs)
  
  def all[K, V](xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(AllCollected, xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(AtLeastCollected(num), xs)
  
  def atLeast(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(AtLeastCollected(num), xs)
  
  def atLeast(num: Int, xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(AtLeastCollected(num), xs)
  
  def atLeast[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(AtLeastCollected(num), xs)
  
  def atLeast[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(AtLeastCollected(num), xs)
  
  def every[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(EveryCollected, xs)
  
  def every(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(EveryCollected, xs)
  
  def every(xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(EveryCollected, xs)
  
  def every[K, V](xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(EveryCollected, xs)
  
  def every[K, V](xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(EveryCollected, xs)
  
  def exactly[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(ExactlyCollected(num), xs)
  
  def exactly(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(ExactlyCollected(num), xs)
  
  def exactly(num: Int, xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(ExactlyCollected(num), xs)
  
  def exactly[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(ExactlyCollected(num), xs)
  
  def exactly[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(ExactlyCollected(num), xs)

  def no[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(NoCollected, xs)

  def no(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(NoCollected, xs)

  def no(xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(NoCollected, xs)

  def no[T](xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(NoCollected, xs)

  def no[T](xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(NoCollected, xs)

  def no[T](xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(NoCollected, xs)

  def no[K, V](xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(NoCollected, xs)

  def no[T](xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(NoCollected, xs)

  def no[K, V](xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(NoCollected, xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), xs)

  def between(from: Int, upTo:Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(BetweenCollected(from, upTo), xs)

  def between(from: Int, upTo:Int, xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(BetweenCollected(from, upTo), xs)

  def between[K, V](from: Int, upTo:Int, xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(BetweenCollected(from, upTo), xs)

  def between[K, V](from: Int, upTo:Int, xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(BetweenCollected(from, upTo), xs)

  def atMost[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(AtMostCollected(num), xs)

  def atMost(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(AtMostCollected(num), xs)

  def atMost(num: Int, xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(AtMostCollected(num), xs)

  def atMost[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(AtMostCollected(num), xs)

  def atMost[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(AtMostCollected(num), xs)
  // This is where ShouldMatchers.scala started 

  // Turn off this implicit conversion, becase asAny method is added via AnyShouldWrapper
  override def convertToAsAnyWrapper(o: Any): AsAnyWrapper = new AsAnyWrapper(o)

  private object ShouldMethodHelper {
    def shouldMatcher[T](left: T, rightMatcher: Matcher[T], stackDepthAdjustment: Int = 0) {
      rightMatcher(left) match {
        case MatchResult(false, failureMessage, _, _, _) => throw newTestFailedException(failureMessage, None, 4 + stackDepthAdjustment)
        case _ => ()
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>Any</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class AnyShouldWrapper[T](left: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be (3)
     *        ^
     * </pre>
     */
    def should(rightMatcherX1: Matcher[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX1)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should equal (3)
     *        ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[T, Equality])(implicit equality: Equality[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * () shouldEqual ()
     *    ^
     * </pre>
     */
    def shouldEqual(right: Any)(implicit equality: Equality[T]) {
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should not equal (3)
     *        ^
     * </pre>
     */
    def should(notWord: NotWord) = new ResultOfNotWord[T](left, false)

    /* * Turns out all the tests compile without this one
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * () should === (()) // In 2.10, will work with AnyVals. TODO: Also, Need to ensure Char works
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[T, U]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }

    // TODO: Scaladoc
    def asAny: Any = left
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on <code>String</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  final class StringShouldWrapper(left: String) extends StringShouldWrapperForVerb(left) {

    /* *
     * This method enables syntax such as the following in a <code>FlatSpec</code>:
     *
     * <pre class="stHighlight">
     * "A Stack (when empty)" should "be empty" in {
     *   assert(emptyStack.empty)
     * }
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> passes in a function via the implicit parameter that takes
     * three strings and results in a <code>ResultOfStringPassedToVerb</code>. This method
     * simply invokes this function, passing in left, right, and the verb string
     * <code>"should"</code>.
     * </p>
     *
    def should(right: String)(implicit fun: (String, String, String) => ResultOfStringPassedToVerb): ResultOfStringPassedToVerb = {
      fun(left, right, "should")
    }

    def should(right: => Unit)(implicit fun: (String, () => Unit, String) => Unit) {
      fun(left, right _, "should")
    }     */

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should be ("hi")
     *        ^
     * </pre>
     */
    def should(rightMatcherX2: Matcher[String]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX2)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should equal ("hi")
     *        ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[String, Equality])(implicit equality: Equality[String]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldEqual "hi"
     *        ^
     * </pre>
     */
    def shouldEqual(right: Any)(implicit equality: Equality[String]) {
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[String] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should have length (3)
     *        ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForString = {
      new ResultOfHaveWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should include regex ("hi")
     *        ^
     * </pre>
     */
    def should(includeWord: IncludeWord): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("hello")
     *        ^
     * </pre>
     */
    def should(startWithWord: StartWithWord): ResultOfStartWithWordForString = {
      new ResultOfStartWithWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("world")
     *        ^
     * </pre>
     */
    def should(endWithWord: EndWithWord): ResultOfEndWithWordForString = {
      new ResultOfEndWithWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
      new ResultOfFullyMatchWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should not have length (3)
     *        ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForString = {
      new ResultOfNotWordForString(left, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * s should === ("hi") 
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[String, U]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on <code>Double</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  final class NumericShouldWrapper[T : Numeric](left: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble should be (8.8)
     *         ^
     * </pre>
     */
    def should(rightMatcherX3: Matcher[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX3)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble should equal (8.8)
     *         ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[T, Equality])(implicit equality: Equality[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldEqual 8.8
     *         ^
     * </pre>
     */
    def shouldEqual(right: T)(implicit equality: Equality[T]) {
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual 7.1 +- 0.2
     *        ^
     * </pre>
     */
    def shouldEqual(interval: Interval[T]) {
      if (!interval.isWithin(left)) {
        throw newTestFailedException(FailureMessages("didNotEqualPlusOrMinus", left, interval.pivot, interval.tolerance))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should not equal (8.8)
     *        ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForNumeric[T] = {
      new ResultOfNotWordForNumeric[T](left, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe 8.8
     *         ^
     * </pre>
     */
    def shouldBe(right: T) {
      if (left != right) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("wasNotEqualTo", leftee, rightee))
      }
    }

    def shouldBe(beMatcher: BeMatcher[T]) {
      beMatcher.apply(left).matches
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (3)
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[T, U]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (100 +- 1)
     *        ^
     * </pre>
     */
    def should(inv: TripleEqualsInvocationOnInterval[T]) {
      if ((inv.interval.isWithin(left)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
            if (inv.expectingEqual) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
            left,
            inv.interval.pivot,
            inv.interval.tolerance
          )
        )
    }
  }

// TODO: Am I doing conversions on immutable.GenTraversable and immutable.GenSeq? If so, write a test that fails and make it general.
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>scala.collection.GenMap[K, V]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class MapShouldWrapper[K, V, L[_, _] <: scala.collection.GenMap[_, _]](left: L[K, V]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should be (Map(1 -> "one", 2 -> "two"))
     *     ^
     * </pre>
     */
    def should(rightMatcherX4: Matcher[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX4)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should equal (Map(1 -> "one", 2 -> "two"))
     *     ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[L[K, V], Equality])(implicit equality: Equality[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map shouldEqual Map(1 -> "one", 2 -> "two")
     *     ^
     * </pre>
     */
    def shouldEqual(right: L[K, V])(implicit equality: Equality[L[K, V]]) {
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should be theSameInstanceAs (anotherMap)
     *     ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[scala.collection.GenMap[K, V]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[GenMap[K, V]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should have size (3)
     *     ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForTraversable[(K, V)] = {
      new ResultOfHaveWordForTraversable(left.asInstanceOf[GenMap[K,V]], true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should contain key (10)
     *     ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForMap[K, V] = {
      new ResultOfContainWordForMap(left.asInstanceOf[GenMap[K, V]], true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should not have size (3)
     *     ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForMap[K, V, L] = {
      new ResultOfNotWordForMap(left.asInstanceOf[L[K, V]], false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (Map("I" -&gt; 1, "II" -&gt; 2))
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[K, V], R]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on <code>AnyRef</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  final class AnyRefShouldWrapper[T <: AnyRef](left: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * anyRef should be (anotherObject)
     *        ^
     * </pre>
     */
    def should(rightMatcherX5: Matcher[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX5)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * anyRef should equal (anotherObject)
     *        ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[T, Equality])(implicit equality: Equality[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * anyRef shouldEqual (anotherObject)
     *        ^
     * </pre>
     */
    def shouldEqual(right: T)(implicit equality: Equality[T]) {
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should not have length (3)
     *        ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForAnyRef[T] =
      new ResultOfNotWordForAnyRef(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[T] = new ResultOfBeWordForAnyRef(left, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should have length (3)
     *        ^
     * result should have size (3)
     *        ^
     * </pre>
     */
    def should(haveWord: HaveWord)(implicit ev: Extent[T]): ResultOfHaveWordForExtent[T] =
      new ResultOfHaveWordForExtent(left, true)

    def shouldBe = new ResultOfBeWordForAnyRef(left, true)
    
    def shouldBe[U](right: Null) {
      if (left != null) {
        throw newTestFailedException(FailureMessages("wasNotNull", left))
      }
    }

    def shouldBe[U](right: AType[U]) {
      if (!right.isAssignableFromClassOf(left)) {
        throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", left, UnquotedString(right.className)))
      }
    }

    def shouldBe(right: AnyRef) {

      def shouldBeEqual(right: AnyRef): Boolean = {
        if (right.isInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]) {
          // need to put in if because NoSuchMethodError when pattern match ResultOfAWordToBePropertyMatcherApplication
          val app = right.asInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]
          app.bePropertyMatcher.apply(left).matches
        }
        else if (right.isInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]) {
          val app = right.asInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]
          app.bePropertyMatcher.apply(left).matches
        }
        else {
          val beWord = new BeWord
          right match {
            case rightSymbol: ResultOfAWordToSymbolApplication => 
              beWord.a[AnyRef](rightSymbol.symbol)(left).matches
            case rightSymbol: ResultOfAnWordToSymbolApplication => 
              beWord.an[AnyRef](rightSymbol.symbol)(left).matches
            case beMatcher: BeMatcher[AnyRef] => 
              beMatcher.apply(left).matches
            case bePropertyMatcher: BePropertyMatcher[AnyRef] => 
              bePropertyMatcher.apply(left).matches
            case _ => 
              left == right
          }
        }
      }

      if (!shouldBeEqual(right)) {
        val (resourceName, leftee, rightee) = 
          if (right.isInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]) {
            val app = right.asInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, UnquotedString(app.bePropertyMatcher.apply(left).propertyName))
            ("wasNotA", leftee, rightee)
          }
          else if (right.isInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]) {
            val app = right.asInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, UnquotedString(app.bePropertyMatcher.apply(left).propertyName))
            ("wasNotAn", leftee, rightee)
          }
          else {
            right match {
              case bePropertyMatcher: BePropertyMatcher[AnyRef] => 
                val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, UnquotedString(bePropertyMatcher.apply(left).propertyName))
                ("wasNot", leftee, rightee)
              case _ => 
                val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
                ("wasNotEqualTo", leftee, rightee)
            }
          }
        throw newTestFailedException(FailureMessages(resourceName, leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (new Person("Abe", "Lincoln"))
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[T, U]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>scala.Collection[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class TraversableShouldWrapper[E, L[_] <: GenTraversable[_]](left: L[E]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should be (Set(1, 2, 3))
     *             ^
     * </pre>
     */
    def should(rightMatcherX6: Matcher[GenTraversable[E]]) {
      ShouldMethodHelper.shouldMatcher(left.asInstanceOf[GenTraversable[E]], rightMatcherX6)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should equal (Set(1, 2, 3))
     *             ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[L[E], Equality])(implicit equality: Equality[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should have size (3)
     *             ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForTraversable[E] = 
      new ResultOfHaveWordForTraversable(left.asInstanceOf[GenTraversable[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should contain theSameElementsAs anotherTraversable
     *             ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForTraversable(left.asInstanceOf[GenTraversable[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should contain customContainMatcher
     *             ^
     * </pre>
     */
    def should(containMatcher: ContainMatcher[E]) {
      new ResultOfContainWordForTraversable(left.asInstanceOf[GenTraversable[E]], true).apply(containMatcher)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should be theSameInstanceAs anotherObject
     *             ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[GenTraversable[E]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[GenTraversable[E]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should not have size (3)
     *             ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForTraversable[E, L] =
      new ResultOfNotWordForTraversable(left, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (Set(1, 2, 3))
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[E], R]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * xs.loneElement should be > 9
     *    ^
     * </pre>
     */
    def loneElement: E = {
      if (left.size == 1)
        left.head.asInstanceOf[E] // Why do I need to cast?
      else
        throw newTestFailedException(
          FailureMessages(
            "notLoneElement",
            left,
            left.size), 
          None, 
          2
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>java.util.Collection[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  // final class JavaCollectionShouldWrapper[T](left: java.util.Collection[T]) {
  final class JavaCollectionShouldWrapper[E, L[_] <: java.util.Collection[_]](left: L[E]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should be (aJavaSet)
     *                ^
     * </pre>
     */
    def should(rightMatcherX7: Matcher[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX7)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should equal (aJavaSet)
     *                ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[L[E], Equality])(implicit equality: Equality[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should have size (3)
     *                ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForJavaCollection[E, L] =
      new ResultOfHaveWordForJavaCollection(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should be theSameInstanceAs anotherObject
     *                ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Collection[E]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[java.util.Collection[E]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should not have size (3)
     *                ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForJavaCollection[E, L] =
      new ResultOfNotWordForJavaCollection(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (jSet)
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[E], R]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>java.util.Map[K, V]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class JavaMapShouldWrapper[K, V, L[_, _] <: java.util.Map[_, _]](left: L[K, V]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should be (someJavaMap)
     *         ^
     * </pre>
     */
    def should(rightMatcherX8: Matcher[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX8)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should equal (someJavaMap)
     *         ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[L[K, V], Equality])(implicit equality: Equality[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should contain value (3)
     *         ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForJavaMap[K, V] = {
      new ResultOfContainWordForJavaMap(left.asInstanceOf[java.util.Map[K, V]], true)
    }
 
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should have size (3)
     *         ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForJavaMap = {
      new ResultOfHaveWordForJavaMap(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should not have length (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForJavaMap[K, V, L] = {
      new ResultOfNotWordForJavaMap[K, V, L](left, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Map[K, V]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[java.util.Map[K, V]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (javaMap)
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[K, V], R]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>GenSeq[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class SeqShouldWrapper[E, L[_] <: GenSeq[_]](left: L[E]) {
 
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should be (List(1, 2, 3))
     *     ^
     * </pre>
     */
    def should(rightMatcherX9: Matcher[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX9)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should equal (List(1, 2, 3))
     *     ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[L[E], Equality])(implicit equality: Equality[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should have length (3)
     *     ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForSeq[E] =
      new ResultOfHaveWordForSeq(left.asInstanceOf[GenSeq[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should contain theSameElementsAs anotherSeq
     *     ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForTraversable(left.asInstanceOf[GenTraversable[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should contain customContainMatcher
     *             ^
     * </pre>
     */
    def should(containMatcher: ContainMatcher[E]) {
      new ResultOfContainWordForTraversable(left.asInstanceOf[GenTraversable[E]], true).apply(containMatcher)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should not have length (3)
     *     ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForSeq[E, L] =
      new ResultOfNotWordForSeq(left, false)
    // def should(notWord: NotWord): ResultOfNotWordForAnyRef[GenSeq[E]] =
      // new ResultOfNotWordForAnyRef(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should be theSameInstanceAs anotherObject
     *     ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[L[E]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (List(1, 2, 3))
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[E], R]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>scala.Array[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class ArrayShouldWrapper[T](left: Array[T]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * array should be (Array("one", "two"))
     *       ^
     * </pre>
     */
    def should(rightMatcherX10: Matcher[Array[T]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX10)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * array should equal (Array("one", "two"))
     *       ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[Array[T], Equality])(implicit equality: Equality[Array[T]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * array should have length (3)
     *       ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForSeq[T] = {
      new ResultOfHaveWordForSeq(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * array should not have length (3)
     *       ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForArray[T] =
      new ResultOfNotWordForArray(left, false)
    
    def shouldBe(right: Array[T]) {
      if (!left.deep.equals(right.deep)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("wasNotEqualTo", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (Array(1, 2, 3))
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[Array[T], U]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }
  // Note, no should(beWord) is needed here because a different implicit conversion will be used
  // on "array shoudl be ..." because this one doesn't solve the type error.

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>java.util.List[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class JavaListShouldWrapper[E, L[_] <: java.util.List[_]](left: L[E]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should be (someOtherJavaList)
     *          ^
     * </pre>
     */
    def should(rightMatcherX12: Matcher[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX12)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should equal (someOtherJavaList)
     *          ^
     * </pre>
     */
    def should(rightMatcherGen1: MatcherGen1[L[E], Equality])(implicit equality: Equality[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherGen1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should have length (3)
     *          ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForJavaList[E, L] = {
      new ResultOfHaveWordForJavaList(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should not have length (3)
     *          ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForJavaList[E, L] = {
      new ResultOfNotWordForJavaList(left, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (jList)
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[L[E], U]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable a <code>should</code> method to
   * be invoked on objects that result of <code>evaulating { ... }</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class EvaluatingApplicationShouldWrapper(left: ResultOfEvaluatingApplication) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
     *                                ^
     * </pre>
     */
     def should[T](resultOfProduceApplication: ResultOfProduceInvocation[T]): T =  {
       val clazz = resultOfProduceApplication.clazz
       val caught = try {
         left.fun()
         None
       }
       catch {
         case u: Throwable => {
           if (!clazz.isAssignableFrom(u.getClass)) {
             val s = Resources("wrongException", clazz.getName, u.getClass.getName)
             throw newTestFailedException(s, Some(u))
             // throw new TestFailedException(s, u, 3) 
           }
           else {
             Some(u)
           }
         }
       }
       caught match {
         case None =>
           val message = Resources("exceptionExpected", clazz.getName)
           throw newTestFailedException(message)
           // throw new TestFailedException(message, 3)
         case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase isAssignableFrom succeeded above
       }
     }
  }

  /**
   * Implicitly converts an object of type <code>T</code> to a <code>EvaluatingApplicationShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToEvaluatingApplicationShouldWrapper(o: ResultOfEvaluatingApplication): EvaluatingApplicationShouldWrapper = new EvaluatingApplicationShouldWrapper(o)

  /**
   * Implicitly converts an object of type <code>T</code> to a <code>AnyShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToAnyShouldWrapper[T](o: T): AnyShouldWrapper[T] = new AnyShouldWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Double</code> to a <code>DoubleShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToNumericShouldWrapperForDouble(o: Double): NumericShouldWrapper[Double] = new NumericShouldWrapper[Double](o)

  /**
   * Implicitly converts an object of type <code>scala.Float</code> to a <code>NumericShouldWrapper[Float]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToNumericShouldWrapperForFloat(o: Float): NumericShouldWrapper[Float] = new NumericShouldWrapper[Float](o)

  /**
   * Implicitly converts an object of type <code>scala.Long</code> to a <code>NumericShouldWrapper[Long]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToNumericShouldWrapperForLong(o: Long): NumericShouldWrapper[Long] = new NumericShouldWrapper[Long](o)

  /**
   * Implicitly converts an object of type <code>scala.Int</code> to a <code>NumericShouldWrapper[Int]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToNumericShouldWrapperForInt(o: Int): NumericShouldWrapper[Int] = new NumericShouldWrapper[Int](o)

  /**
   * Implicitly converts an object of type <code>scala.Short</code> to a <code>NumericShouldWrapper[Short]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToNumericShouldWrapperForShort(o: Short): NumericShouldWrapper[Short] = new NumericShouldWrapper[Short](o)

  /**
   * Implicitly converts an object of type <code>scala.Byte</code> to a <code>NumericShouldWrapper[Byte]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToByteShouldWrapper(o: Byte): NumericShouldWrapper[Byte] = new NumericShouldWrapper[Byte](o)

  /**
   * Implicitly converts a <code>scala.AnyRef</code> of type <code>T</code> to an <code>AnyRefShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToAnyRefShouldWrapper[T <: AnyRef](o: T): AnyRefShouldWrapper[T] = new AnyRefShouldWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.Collection[T]</code> to a <code>CollectionShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToTraversableShouldWrapper[E, L[_] <: GenTraversable[_]](o: L[E]): TraversableShouldWrapper[E, L] = new TraversableShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>GenSeq[T]</code> to a <code>SeqShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToSeqShouldWrapper[E, L[_] <: GenSeq[_]](o: L[E]): SeqShouldWrapper[E, L] = new SeqShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>scala.Array[T]</code> to a <code>ArrayShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToArrayShouldWrapper[T](o: Array[T]): ArrayShouldWrapper[T] = new ArrayShouldWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.collection.GenMap[K, V]</code> to a <code>MapShouldWrapper[K, V]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToMapShouldWrapper[K, V, L[_, _] <: scala.collection.GenMap[_, _]](o: L[K, V]): MapShouldWrapper[K, V, L] = new MapShouldWrapper[K, V, L](o)

  /**
   * Implicitly converts an object of type <code>java.lang.String</code> to a <code>StringShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit override def convertToStringShouldWrapper(o: String): StringShouldWrapper = new StringShouldWrapper(o)

  /**
   * Implicitly converts an object of type <code>java.util.Collection[T]</code> to a <code>JavaCollectionShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToJavaCollectionShouldWrapper[E, L[_] <: java.util.Collection[_]](o: L[E]): JavaCollectionShouldWrapper[E, L] = new JavaCollectionShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>java.util.List[T]</code> to a <code>JavaListShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object. This conversion is necessary to enable
   * <code>length</code> to be used on Java <code>List</code>s.
   */
  // implicit def convertToJavaListShouldWrapper[T](o: java.util.List[T]): JavaListShouldWrapper[T] = new JavaListShouldWrapper[T](o)
  implicit def convertToJavaListShouldWrapper[E, L[_] <: java.util.List[_]](o: L[E]): JavaListShouldWrapper[E, L] = new JavaListShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>java.util.Map[K, V]</code> to a <code>JavaMapShouldWrapper[K, V]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToJavaMapShouldWrapper[K, V, L[_, _] <: java.util.Map[_, _]](o: L[K, V]): JavaMapShouldWrapper[K, V, L] = new JavaMapShouldWrapper[K, V, L](o)
  
  /**
   * Turn off implicit conversion of LoneElement, so that if user accidentally mixin LoneElement it does conflict with convertToTraversableShouldWrapper
   */
  override def convertToTraversableLoneElementWrapper[T](xs: GenTraversable[T]): LoneElementTraversableWrapper[T] = new LoneElementTraversableWrapper[T](xs)
}

/**
 * Companion object that facilitates the importing of <code>Matchers</code> members as 
 * an alternative to mixing it the trait. One use case is to import <code>Matchers</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.7.3.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala> import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
 * 
 * scala> 1 should equal (2)
 * org.scalatest.TestFailedException: 1 did not equal 2
 * 	at org.scalatest.matchers.Helper$.newTestFailedException(Matchers.template:40)
 * 	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:826)
 * 	at org.scalatest.matchers.ShouldMatchers$IntShouldWrapper.should(ShouldMatchers.scala:1123)
 * 	at .<init>(<console>:9)
 * 	at .<clinit>(<console>)
 * 	at RequestR...
 *
 * scala> "hello, world" should startWith ("hello")
 * 
 * scala> 7 should (be >= (3) and not be <= (7))
 * org.scalatest.TestFailedException: 7 was greater than or equal to 3, but 7 was less than or equal to 7
 * 	at org.scalatest.matchers.Helper$.newTestFailedException(Matchers.template:40)
 * 	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:826)
 * 	at org.scalatest.matchers.ShouldMatchers$IntShouldWrapper.should(ShouldMatchers.scala:1123)
 * 	at .<init>(...
 * </pre>
 *
 * @author Bill Venners
 */
object Matchers extends Matchers
