package com.todesking.doclet

abstract sealed class Entity {
  def kind:Kind[_]
  def name:String
  def id:BoundEntityId[_]
  def representation:String = ??? // TODO: be abstract
}

/** Entity that contains types, methods, type aliases, or objects */
abstract class ContainerEntity extends Entity {
  // TODO: be abstract
  def memberTypes(implicit repo:Repository):Seq[Type] = ???
  // TODO: be abstract
  def memberMethods:Seq[Method] = ???
}

abstract class Package extends ContainerEntity {
  override def kind = Kind.Package
  override def id:EntityId.bound.Package
  def moduleId:ModuleId
  def subPackages:Seq[SubPackage] = ???
}

case class SubPackage(parentId:EntityId.bound.Package, override val name:String) extends Package {
  override def moduleId = parentId.moduleId
  override def id:EntityId.bound.Package = parentId.memberId(kind, name)
}

case class RootPackage(override val moduleId:ModuleId) extends Package {
  override def id = BoundEntityId(moduleId, RootUnboundEntityId(Kind.Package))
  override def name = ""
}

// TODO: self type annotation
case class Type(namespace:EntityId.bound.Container, override val name:String, val inherits:Seq[TypeRef]) extends ContainerEntity {
  override def id = namespace.memberId(Kind.Type, name)
  override val kind = Kind.Type
  def companion(implicit repo:Repository):Option[Objekt] = repo.find[Objekt](id.change(kind = Kind.Objekt))
}

case class Objekt(namespace:EntityId.bound.Container, override val name:String, val inherits:Seq[TypeRef]) extends ContainerEntity {
  override val id = namespace.memberId(Kind.Objekt, name)
  override val kind = Kind.Objekt
  def companion(implicit repo:Repository):Option[Type] = repo.find[Type](id.change(kind = Kind.Type))
}

case class TypeAlias(val qualifiedName:String, val params:TypeParams, val ref:TypeRef)

case class TypeRef(ref:EntityId.Type, args:TypeArgs) {
  def tpe(implicit repo:Repository):Option[Type] = repo.find[Type](ref)
}

case class TypeArgs(representation:String)

case class TypeParams(representation:String)
case class ValueParams(representation:String)

abstract class Method extends Entity {
  def namespace:EntityId.bound.Container
  def typeParams:TypeParams
  def valueParams:ValueParams
  def retType:TypeRef

  override val kind = Kind.Method
  override def id:EntityId.bound.Method = namespace.memberId(kind, name)
}

case class Def(
  override val namespace:EntityId.bound.Container,
  override val name:String,
  override val typeParams:TypeParams,
  override val valueParams:ValueParams,
  override val retType:TypeRef
) extends Method {
}

case class Val(
  override val namespace:EntityId.bound.Container,
  override val name:String,
  override val retType:TypeRef
) extends Method {
  override def typeParams = TypeParams("")
  override def valueParams = ValueParams("")
}

case class Var(
  override val namespace:EntityId.bound.Container,
  override val name:String,
  override val retType:TypeRef
) extends Method {
  override def typeParams = TypeParams("")
  override def valueParams = ValueParams("")
}
