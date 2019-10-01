name := "joern"
organization := "io.shiftleft"
ThisBuild/scalaVersion := "2.12.8"

val cpgVersion = "0.10.30"
val fuzzyc2cpgVersion = "0.1.101"
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"

scmInfo := Some(ScmInfo(url("https://github.com/ShiftLeftSecurity/joern"),
                     "scm:git@github.com:ShiftLeftSecurity/joern.git"
                     ))
homepage := Some(url("https://joern.io/"))
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val joerncli = Projects.joerncli
lazy val joernserver = Projects.joernserver
