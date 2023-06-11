ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "anyphoc4s",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "org.scalameta" %% "munit" % "1.0.0-M6",
      "org.locationtech.jts" % "jts" % "1.19.0",
      "org.locationtech.jts" % "jts-core" % "1.19.0",
      "org.locationtech.jts" % "jts-io" % "1.19.0" pomOnly(),
      "org.locationtech.jts" % "jts-modules" % "1.19.0" pomOnly(),
      "org.locationtech.jts.io" % "jts-io-common" % "1.19.0",
      "org.apache.pdfbox" % "pdfbox" % "2.0.27",
      "org.jsoup" % "jsoup" % "1.15.4",
      "com.github.geirolz" %% "advxml-core" % "2.5.1",
      "com.github.plokhotnyuk.rtree2d" %% "rtree2d-core" % "0.11.12"
    )
  )