object Build extends sbt.Build{
  import sbt._

  lazy val root = Project(
    "example",
    file(".")
  )
}
