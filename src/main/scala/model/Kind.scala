package com.todesking.scala_models

sealed abstract class Kind[A <: Entity](val name:String) {
  type EntityType = A
  override def toString = name
}
object Kind {
  abstract class ContainerKind[A <: Entity](name:String) extends Kind[A](name)
  object Package   extends ContainerKind [Package]   ("package")
  object Type      extends ContainerKind [Type]      ("type")
  object TypeAlias extends Kind          [TypeAlias] ("type_alias")
  object Method    extends Kind          [Method]    ("method")
  object Objekt    extends ContainerKind [Objekt]    ("objekt")
  // TODO: Really need it?
  object Unknown   extends Kind          [Nothing]   ("unknown")
}
