
name := "scala-fuzzy-logic"

version := "0.1"

scalaVersion := "2.12.8"

resolvers ++= Seq(
  "jfreechart" at "https://mvnrepository.com/artifact/org.jfree/jfreechart",
  "jzy3d" at  "http://maven.jzy3d.org/releases"
)

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.20",
  "org.jfree" % "jfreechart" % "1.0.19",
  "org.jzy3d" % "jzy3d-api" % "1.0.2",
  "org.jzy3d" % "jzy3d-javafx" % "1.0.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
