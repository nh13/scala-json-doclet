package com.todesking.doclet

sealed abstract class Kind[A](val name:String) {
  type EntityType = A
  override def toString = name
}
object Kind {
  abstract class ContainerKind[A](name:String) extends Kind[A](name)
  object Package   extends ContainerKind [Package]   ("package")
  object Type      extends ContainerKind [Type]      ("type")
  object TypeAlias extends Kind          [TypeAlias] ("type_alias")
  object Method    extends Kind          [Method]    ("method")
  object Objekt    extends ContainerKind [Objekt]    ("objekt")
}
