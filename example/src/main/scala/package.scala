package com.todesking

/** this is the comment for package object "com.todesking.example" **/
package object example {
  /** this method is package global **/
  def foo() = 100

  def foo(a:Int, b:String):Map[Int, Seq[String]] = ???

  def bar:this.type = ???
}

