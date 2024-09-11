name := "tap-2024-base"

version := "0.1"

scalaVersion := "3.3.1"

scalacOptions ++= Seq("-source:future", "-indent", "-rewrite")

// XML
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.2.0"
// ScalaTest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
// ScalaCheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"

wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Equals, Wart.Nothing,
  Wart.Overloading, Wart.Recursion, Wart.StringPlusAny,
  Wart.ToString, Wart.TripleQuestionMark)
