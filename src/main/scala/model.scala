abstract class Module {
  def id:ModuleId
  def root:RootPackage
  // TODO: tree structure

  val repo:Repository
}

trait Repository {
  def find[T](id:EntityId[_ <: Kind[T]]):Option[T] = ???
}

sealed abstract class Kind[A](val name:String) {
  type EntityType = A
  override def toString = name
}
object Kind {
  abstract class ContainerKind[A](name:String) extends Kind[A](name)
  object Package   extends ContainerKind[Package]("package")
  object Type      extends ContainerKind[Type]("type")
  object TypeAlias extends Kind[TypeAlias]("type_alias")
  object Method    extends Kind[Method]("method")
  object Objekt    extends ContainerKind[Objekt]("objekt")
}

object EntityId {
  type Type = EntityId[_ <: Kind.Type.type]
  type Objekt = EntityId[_ <: Kind.Objekt.type]
  object bound {
    type Package = BoundEntityId[_ <: Kind.Package.type]
    type Method = BoundEntityId[_ <: Kind.Method.type]
    type Container = BoundEntityId[_ <: Kind.ContainerKind[_]]
  }
}
abstract class EntityId[K <: Kind[_]] {
  def kind:K
  def representation:String
}
case class BoundEntityId[K <: Kind[_]](moduleId:ModuleId, entityId:UnboundEntityId[K]) extends EntityId[K] {
  override def representation = s"${moduleId.representation}:${entityId.representation}"
  override def kind:K = entityId.kind
  def memberId[K2 <: Kind[_]](kind:K2, name:String):BoundEntityId[K2] = BoundEntityId(moduleId, entityId.memberId(kind, name))
  def change[K2 <: Kind[_]](kind:K2) = BoundEntityId[K2](moduleId, entityId.change(kind = kind))
}
abstract class UnboundEntityId[K <: Kind[_]] extends EntityId[K] {
  def memberId[K2 <: Kind[_]](kind:K2, name:String) = MemberUnboundEntityId(this, kind, name)
  def change[K2 <: Kind[_]](kind:K2):UnboundEntityId[K2]
}
case class RootUnboundEntityId[K <: Kind[_]](override val kind:K) extends UnboundEntityId[K] {
  override def representation:String = s".@${kind}"
  override def change[K2 <: Kind[_]](kind:K2) = RootUnboundEntityId[K2](kind)
}
case class MemberUnboundEntityId[K <: Kind[_], N <: Kind[_]](namespace:UnboundEntityId[N], override val kind:K, name:String) extends UnboundEntityId[K] {
  override def representation:String = s"${namespace.representation}.${name}@${kind}"
  override def change[K2 <: Kind[_]](kind:K2) = MemberUnboundEntityId[K2, N](namespace, kind, name)
}

case class ModuleId(representation:String)

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
