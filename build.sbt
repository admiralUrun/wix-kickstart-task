name := "Kickstart testing example"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.7.0" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")
