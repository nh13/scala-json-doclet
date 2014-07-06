package com.todesking.scala_models

import scala.collection.mutable

trait Repository {
  def find[T <: Entity](id:EntityId[_ <: Kind[T]]):Option[T] = ???
  def findMembers[T <: Entity](id:EntityId[_ <: Kind[_]], kind:Kind[T]):Seq[T] = ???
}

class ModuleRepository(moduleId:ModuleId) extends Repository {
  val data:mutable.Map[(EntityId.Unbound[_], Kind[_]), Entity] = new mutable.HashMap
  def register(entity:Entity):Unit = {
    if(entity.id.moduleId != moduleId) throw new IllegalArgumentException("Trying to register another modules entity")
    else data(entity.id.unbound -> entity.id.kind) = entity
    println(s"registered: $entity")
  }
}
