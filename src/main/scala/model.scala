package com.todesking.scala_models

trait Repository {
  def find[T](id:EntityId[_ <: Kind[T]]):Option[T] = ???
}

