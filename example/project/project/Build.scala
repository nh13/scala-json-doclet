object Build extends sbt.Build{
  import sbt._

  lazy val root = Project(
    "example_meta",
    file(".")
  )
    .dependsOn(file("../vendor/sbt-custom-doclet/"))
    .dependsOn(file("../../"))
}

