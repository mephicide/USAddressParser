organization  := "org.vickery"
name := "address-parser"

version := "2.1"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.7"

exportJars := true

credentials += Credentials("Sonatype Nexus Repository Manager",
  "172.31.20.143",
  "deployment",
  "w7A-6zw-shy-hfA")

publishMavenStyle := true
//isSnapshot := true
publishTo := Some("Sonatype Nexus Repository Manager" at "http://172.31.20.143:8081/nexus/content/repositories/releases")
    