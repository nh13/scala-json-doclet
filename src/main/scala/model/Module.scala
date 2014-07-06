package com.todesking.scala_models

case class ModuleId(representation:String)
class Module(val id:ModuleId, val repository:Repository) {
  object Implicit {
    implicit val repository = Module.this.repository
  }
  def rootPackage:Package = RootPackage(id)
}

object ModuleId {
  def temporal() = ModuleId("com.todesking.scala_models:_temporal_:0.0.0")
}
