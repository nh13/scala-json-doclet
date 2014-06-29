package com.todesking.json_doclet

import scala.tools.nsc.doc.doclet.{Generator, Universer, Indexer}
import java.io.{File => JFile }
import scala.tools.nsc.doc.model

object Ext {
  implicit class Tapper[A](self:A) {
    def change[B](proc:A => B):B = proc(self)
    def tap(proc:A => Unit):A = { proc(self); self }
  }
}

trait Doclet extends Generator with Universer with Indexer {
  def outTarget = new JFile(universe.settings.outdir.value)

  def root = universe.rootPackage

  def write(name:String)(content:(String => Unit) => Unit):Unit = {
    import java.io._
    val outPath = new JFile(outTarget, name)
    // val writer = new PrintWriter(new BufferedWriter(new FileWriter(outPath)))
    // try content(line => writer.println(line))
    // finally writer.close
    println(s"Write to $outPath")
    content(line => println(s"[$name] $line"))
  }
}


class JsonDoclet extends Doclet {
  import scala.tools.nsc.doc.model._
  override def generateImpl():Unit = {
    root.members.foreach(publish(_))
  }

  def publish(entity:model.Entity):Unit = {
    import Ext._
    entity match {
      case e:model.Package =>
        val fullname = e.toRoot.change(l => l.slice(0, l.size - 1)).reverse.map(_.name).mkString(".")
        write(s"${fullname}.package") {puts =>
          puts(s"# Package ${fullname}")
          commentOf(e).map(c => puts(c.toString))
        }
        e.templates.foreach(publish(_))
      case e:model.MemberTemplateEntity =>
        val fullname = e.toRoot.change(l => l.slice(0, l.size - 1)).reverse.map(_.name).mkString(".")
        write(s"${fullname}.${e.kind}") {puts =>
          puts(s"# ${e.kind} ${fullname} ${e.typeparams}")
          commentOf(e).map(c => puts(c.toString))
        }
      case x@_ => println(s"Skip unsupported model: ${x}")
    }
  }

  def commentOf(entity:model.Entity) = entity match { case e:model.MemberEntity => e.comment case _ => None }
}
