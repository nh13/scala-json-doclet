package com.todesking.scala_models

case class ModuleId(representation:String)
abstract class Module {
  def id:ModuleId
  def root:RootPackage
  // TODO: tree structure

  val repo:Repository
}
