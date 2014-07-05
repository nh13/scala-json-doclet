package com.todesking.scala_models

object EntityId {
  type Type   = EntityId[_ <: Kind.Type.type]
  type Objekt = EntityId[_ <: Kind.Objekt.type]
  object bound {
    type Package   = BoundEntityId[_ <: Kind.Package.type]
    type Method    = BoundEntityId[_ <: Kind.Method.type]
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
