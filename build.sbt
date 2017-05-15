val commonSettings = Seq(
  scalaVersion := "2.12.1",
  libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.9" % "test"),
  scalacOptions in Test ++= Seq("-Yrangepos")
)



lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )


//scalaVersion := "2.12.1"
//libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.9" % "test")
//scalacOptions in Test ++= Seq("-Yrangepos")
