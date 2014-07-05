package com.todesking.json_doclet

import scala.tools.nsc.doc.doclet.{Generator, Universer, Indexer}
import java.io.{File => JFile }
import scala.tools.nsc.doc.model
import scala.tools.nsc.doc.base.comment

import com.todesking.scala_models._

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
    import CommentRenderer._
    import EntityEx._
    def section[A <: model.MemberEntity](title:String, data:Seq[A])(proc:A => Unit)(implicit puts:String => Unit):Unit = {
      val members = data.filter(_.isDefinedHere)
      if(members.isEmpty) return
      puts(title)
      members.foreach(proc)
    }
    entity match {
      case e:model.Package =>
        val fullname = e.toRoot.change(l => l.slice(0, l.size - 1)).reverse.map(_.name).mkString(".")
        write(s"${fullname}.package") {implicit puts =>
          puts(s"# Package ${fullname}")
          commentOf(e).map(c => c.render(e.kind).foreach(puts(_)))
          section("## Members", e.members) {member =>
            puts(s"* ${member.name}")
          }
        }
        e.templates.foreach(publish(_))
      case e:model.DocTemplateEntity =>
        val fullname = e.toRoot.change(l => l.slice(0, l.size - 1)).reverse.map(_.name).mkString(".")
        write(s"${fullname}.${e.kind}") {implicit puts =>
          puts(s"# ${e.kind} ${fullname}")
          puts("")
          commentOf(e).map(c => c.render(e.kind).foreach(puts(_)))

          section("## Methods", e.methods) {m =>
            puts("")
            puts(s"### ${m.name} ${m.typeParams} ${m.valueParams}: ${m.resultType}")
            puts("")
            commentOf(m).map(c => c.render(m.kind).foreach(puts(_)))
          }

          section("## values", e.values) {m =>
            puts("")
            puts(s"### ${m.name}: ${m.resultType}")
            puts("")
            commentOf(m).map(c => c.render(m.kind).foreach(puts(_)))
          }

          section("## type aliases", e.aliasTypes) {m =>
            puts("")
            puts(s"### ${m.name} = ${m.alias}")
          }
        }
      case x@_ => println(s"Skip unsupported model: ${x}")
    }
  }

  def commentOf(entity:model.Entity) = entity match { case e:model.MemberEntity => e.comment case _ => None }
}

object EntityEx {
  import model._
  implicit class MemberEntity_(val self:MemberEntity) {
    def isDefinedHere():Boolean =
      self.inDefinitionTemplates.head == self.inTemplate
  }
}

object CommentRenderer {
  import comment._
  implicit class CommentRenderer_(val self:Comment) extends AnyVal {
    def render(kind:String):Seq[String] = {
      kind match {
        case _ => self.body.blocks.flatMap(_.render)
      }
    }
  }
  implicit class BlockRenderer(val self:Block) extends AnyVal {
    def render():Seq[String] = {
      self match {
        case Paragraph(text) => Seq(text.render, "")
        case _ => Seq(self.toString)
      }
    }
  }
  implicit class InlineRenderer(val self:Inline) extends AnyVal {
    def render():String = {
      self match {
        case Chain(items) => items.flatMap(_.render).mkString("")
        case Italic(text) => s"<i>${text.render}</i>"
        case Bold(text) => s"<b>${text.render}</b>"
        case Underline(text) => s"<u>${text.render}</u>"
        case Superscript(text) => s"<sup>${text.render}</sup>"
        case Subscript(text) => s"<sub>${text.render}</sub>"
        case Link(target, title) => s" [${title.render}](${target}) "
        case Monospace(text) => s"`${text.render}`"
        case Text(text) => text
        case Summary(text) => text.render
        case _ => self.toString
      }
    }
  }
}
