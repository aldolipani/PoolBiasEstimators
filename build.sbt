name := "PoolBiasEstimators"

version := "1.0"

scalaVersion := "2.11.11"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"

libraryDependencies += "com.github.cb372" %% "scalacache-guava" % "0.9.1"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.22"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.5.1"

libraryDependencies += "io.spray" % "spray-can_2.11" % "1.3.4"

libraryDependencies += "io.spray" % "spray-http_2.11" % "1.3.4"

libraryDependencies += "io.spray" % "spray-routing-shapeless23_2.11" % "1.3.4"

libraryDependencies += "io.spray" % "spray-json_2.11" % "1.3.3"

resolvers += Resolver.sonatypeRepo("public")
