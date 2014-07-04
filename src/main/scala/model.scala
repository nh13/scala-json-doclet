trait TypeRepository {
  def findType(id:EntityId[Kind.Type.type]):Option[Type]
}
trait ObjektRepository {
  def findObjekt(id:EntityId[Kind.Objekt.type]):Option[Objekt]
}
trait PackageRepository {
  def memberPackages(qualifiedName:String):Seq[Package]
  def memberTypes(qualifiedName:String):Seq[Type]
  def memberMethods(qualifiedName:String):Seq[Method]
}

abstract class Module {
  def id:ModuleId
  def root:RootPackage
  // TODO: tree structure

  object repo {
    val packages:PackageRepository = ???
    val objects:ObjektRepository = ???
    val types:TypeRepository = ???
  }
}

trait Repository {
}

sealed abstract class Kind(val name:String) {
  override def toString = name
}
object Kind {
  abstract class ContainerKind(name:String) extends Kind(name)
  object Package   extends ContainerKind("package")
  object Type      extends ContainerKind("type")
  object TypeAlias extends Kind("type_alias")
  object Method    extends Kind("method")
  object Objekt    extends ContainerKind("objekt")
}

abstract class EntityId[K <: Kind] {
  def representation:String
}
case class BoundEntityId[K <: Kind](moduleId:ModuleId, entityId:UnboundEntityId[K]) extends EntityId[K] {
  override def representation = s"${moduleId.representation}:${entityId.representation}"
  def memberId[K2 <: Kind](kind:K2, name:String):BoundEntityId[K2] = BoundEntityId(moduleId, entityId.memberId(kind, name))
  def change[K2 <: Kind](kind:K2) = BoundEntityId[K2](moduleId, entityId.change(kind = kind))
}
abstract class UnboundEntityId[K <: Kind] extends EntityId[K] {
  def kind:K
  def memberId[K2 <: Kind](kind:K2, name:String) = MemberUnboundEntityId(this, kind, name)
  def change[K2 <: Kind](kind:K2):UnboundEntityId[K2]
}
case class RootUnboundEntityId[K <: Kind](override val kind:K) extends UnboundEntityId[K] {
  override def representation:String = s".@${kind}"
  override def change[K2 <: Kind](kind:K2) = RootUnboundEntityId(kind)
}
case class MemberUnboundEntityId[K <: Kind, N <: Kind](namespace:UnboundEntityId[N], override val kind:K, name:String) extends UnboundEntityId[K] {
  override def representation:String = s"${namespace.representation}.${name}@${kind}"
  override def change[K2 <: Kind](kind:K2) = MemberUnboundEntityId(namespace, kind, name)
}

case class ModuleId(representation:String)

abstract sealed class Entity {
  def kind:Kind
  def name:String
  def id:BoundEntityId[_]
  def representation:String = ??? // TODO: be abstract
}

/** Entity that contains types, methods, type aliases, or objects */
abstract class ContainerEntity extends Entity {
  // TODO: be abstract
  def memberTypes(implicit repo:TypeRepository):Seq[Type] = ???
  // TODO: be abstract
  def memberMethods:Seq[Method] = ???
}

abstract class Package extends ContainerEntity {
  override def kind = Kind.Package
  override def id:BoundEntityId[Kind.Package.type]
  def moduleId:ModuleId
}

case class SubPackage(parentId:BoundEntityId[Kind.Package.type], override val name:String) extends Package {
  override def moduleId = parentId.moduleId
  override def id:BoundEntityId[Kind.Package.type] = parentId.memberId(kind, name)
  def packages:Seq[Package] = ???
}

case class RootPackage(override val moduleId:ModuleId) extends Package {
  override def id = BoundEntityId(moduleId, RootUnboundEntityId(Kind.Package))
  override def name = ""
}

// TODO: self type annotation
case class Type(namespace:BoundEntityId[_ <: Kind.ContainerKind], override val name:String, val inherits:Seq[TypeRef]) extends ContainerEntity {
  override def id = namespace.memberId(Kind.Type, name)
  override val kind = Kind.Type
  def companion(implicit repo:ObjektRepository):Option[Objekt] = repo.findObjekt(id.change(kind = Kind.Objekt))
}

case class Objekt(namespace:BoundEntityId[_ <: Kind.ContainerKind], override val name:String, val inherits:Seq[TypeRef]) extends ContainerEntity {
  override val id = namespace.memberId(Kind.Objekt, name)
  override val kind = Kind.Objekt
  def companion(implicit repo:TypeRepository):Option[Type] = repo.findType(id.change(kind = Kind.Type))
}

case class TypeAlias(val qualifiedName:String, val params:TypeParams, val ref:TypeRef)

case class TypeRef(ref:EntityId[Kind.Type.type], args:TypeArgs) {
  def tpe(implicit repo:TypeRepository):Option[Type] = repo.findType(ref)
}

case class TypeArgs(representation:String)

case class TypeParams(representation:String)
case class ValueParams(representation:String)

abstract class Method(name:String, typeParams:TypeParams, valueParams:ValueParams, tpe:TypeRef)
case class Def(name:String, typeParams:TypeParams, valueParams:ValueParams, tpe:TypeRef) extends Method(name, typeParams, valueParams, tpe)
case class Val(name:String, tpe:TypeRef) extends Method(name, TypeParams(""), ValueParams(""), tpe)
case class Var(name:String, tpe:TypeRef) extends Method(name, TypeParams(""), ValueParams(""), tpe)

