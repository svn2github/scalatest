#!/bin/sh
exec scala "$0" "$@"
!#

/*
 * This script converts java source files that define tag classes into
 * scala files that can be used to generate scaladocs.
 *
 * The resulting scala files won't work for actually running code, but
 * they're close enough to get into the scaladocs.
 *
 * The script rewrites five files in src/main/java/org/scalatest:
 *  
 *   DoNotDiscover.java
 *   Ignore.java
 *   Finders.java
 *   TagAnnotation.java
 *   WrapWith.java
 *
 * It copies them into target/docsrc/org/scalatest, removing java annotations
 * and converting them into similar scala code and preserving their header
 * comments so those make it into the scaladocs.
 *
 * This script aborts if any unexpected files are found in the java
 * directory, in order to make it obvious that something changed that
 * needs to be looked at.  If it's a new tag file, try adding it to the
 * filenames set and see if it works.  Otherwise, modify this script as
 * needed to get things working again.
 */

import java.io.File
import java.io.PrintWriter
import java.util.regex.Pattern
import scala.io.Source

val srcDir    = "src/main/java/org/scalatest"
val docsrcDir = "target/docsrc/org/scalatest"

//
// Splits java file's contents into two pieces: a top and body. 
// The top contains everything up through the declared class's name, and
// the body contains the following curly braces and their contents.
//
def parseContents(className: String, text: String): (String, String) = {

  val pat = Pattern.compile("""(?sm)(.*? @interface """ + className +
                            """) *(\{.*\})""")
  val matcher = pat.matcher(text)

  matcher.find()
  (matcher.group(1), matcher.group(2))
}

//
// Constructs a modified class body where the java declaration of the value()
// method, where present, is replaced by a scala version.
//
def genNewBody(body: String): String = {
  val matcher =
    Pattern.compile("""(?m)^\s*(.*?) *value\(\);""").matcher(body)

  if (matcher.find()) {
    val valueType = matcher.group(1)

    val newValueType =
      valueType match {
        case "Class<? extends Suite>" => "Class[_ <: Suite]"
        case "String"                 => "String"
        case "String[]"                 => "Array[String]"
        case _ =>
          throw new RuntimeException("unexpected valueType [" +
                                     valueType + "]")
    }

    val buf = new StringBuffer
    matcher.appendReplacement(buf, " def value(): "+ newValueType)
    matcher.appendTail(buf)

    buf.toString
  }
  else ""
}

def main() {
  println("docjavatags.scala: porting java tag files to scala")

  val filenames = Set("DoNotDiscover.java",
                      "Ignore.java",
                      "Finders.java",
                      "TagAnnotation.java",
                      "WrapWith.java")

  for (file <- (new File(srcDir)).list; if file.endsWith(".java")) {
    if (!filenames.contains(file))
      throw new RuntimeException("found unexpected java file [" + file + "]")

    val contents  = Source.fromFile(srcDir +"/"+ file).mkString
    val className = file.replaceFirst("""\.java$""", "")

    val (top, body) = parseContents(className, contents)

    val newBody = genNewBody(body)

    val newContents =
      top
        .replaceAll("""@Retention\(.*?\)""",  "")
        .replaceAll("""@Target\(.*?\)""",     "")
        .replaceAll("""@TagAnnotation""",     "")
        .replaceAll("""@Inherited""",         "")
        .replaceAll("""public *@interface""", "")
        .replaceAll("""(?m)^import.*$""",     "")
        .replaceAll(className + "$",
                    "trait "+ className +
                    " extends java.lang.annotation.Annotation "+ newBody +"\n")

     val newFile = new PrintWriter(docsrcDir +"/"+ className +".scala")
     newFile.print(newContents)
     newFile.close()
  }
}

main()
