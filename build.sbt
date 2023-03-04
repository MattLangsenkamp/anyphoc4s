ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "anyphoc4s",
    libraryDependencies ++= Seq(
      "org.locationtech.jts" % "jts" % "1.19.0",
      "org.locationtech.jts" % "jts-core" % "1.19.0",
      "org.locationtech.jts" % "jts-io" % "1.19.0" pomOnly(),
      "org.locationtech.jts" % "jts-modules" % "1.19.0" pomOnly(),
      "org.locationtech.jts.io" % "jts-io-common" % "1.19.0",
      "org.apache.pdfbox" % "pdfbox" % "2.0.27",
      "com.github.geirolz" %% "advxml-core" % "2.5.1",
    )
  )