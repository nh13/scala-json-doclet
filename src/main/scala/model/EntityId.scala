package com.todesking.scala_models

object EntityId {
  type Type   = EntityId[_ <: Kind.Type.type]
  type Objekt = EntityId[_ <: Kind.Objekt.type]
  type Bound[A <: Kind[_]] = BoundEntityId[A]
  object bound {
    type Package   = BoundEntityId[_ <: Kind.Package.type]
    type Method    = BoundEntityId[_ <: Kind.Method.type]
    type Container = BoundEntityId[_ <: Kind.ContainerKind[_]]
    def root[K <: Kind[_]](moduleId:ModuleId, kind:K):BoundEntityId[K] =
      BoundEntityId(moduleId, unbound.root(kind))
  }
  type Unbound[A <: Kind[_]] = UnboundEntityId[A]
  object unbound {
    def root[K <: Kind[_]](kind:K):RootUnboundEntityId[K] =
      RootUnboundEntityId(kind)
    def unknown(names:Seq[String]):Unbound[_] =
      names.foldLeft[EntityId.Unbound[_ <: Kind[_]]](root(Kind.Package)) {(ns, name) =>
        ns.memberId(Kind.Unknown, name)
      }
  }
}
abstract class EntityId[K <: Kind[_]] {
  def kind:K
  def representation:String
}
case class BoundEntityId[K <: Kind[_]](moduleId:ModuleId, unbound:UnboundEntityId[K]) extends EntityId[K] {
  override def representation = s"${moduleId.representation}:${unbound.representation}"
  override def kind:K = unbound.kind
  def memberId[K2 <: Kind[_]](kind:K2, name:String):BoundEntityId[K2] = BoundEntityId(moduleId, unbound.memberId(kind, name))
  def change[K2 <: Kind[_]](kind:K2) = BoundEntityId[K2](moduleId, unbound.change(kind = kind))
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
