package com.todesking

/** this is the comment for package object "com.todesking.example" **/
package object example {
  /** this method is package global **/
  def foo() = 100

  def foo(a:Int, b:String):Map[Int, Seq[String]] = ???

  def bar:this.type = ???

  def baz:A#C = ???

  def bax:A.O = ???
}

class A {
  type C = String
}

object A {
  type O = Int
}
