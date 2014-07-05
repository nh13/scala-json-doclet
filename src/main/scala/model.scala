package com.todesking.doclet

trait Repository {
  def find[T](id:EntityId[_ <: Kind[T]]):Option[T] = ???
}

