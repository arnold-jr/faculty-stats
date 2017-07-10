name := "faculty-stats"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "net.liftweb" % "lift-json_2.11" % "2.6.3",
  "org.apache.spark" % "spark-mllib_2.11" % "2.0.01",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.51",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.11"
)