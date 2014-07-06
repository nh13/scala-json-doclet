package com.todesking.scala_models

import scala.tools.nsc.doc.model
import scala.tools.nsc.doc.base

/** Build Module from scaladoc universe */
class DocAnalyzer(moduleId:ModuleId) {
  def analyze(root:model.Package):Module = {
    val repo = new ModuleRepository(moduleId)

    val rootId = EntityId.bound.root(moduleId, Kind.Package)

    analyzePackage(repo, root, rootId)

    new Module(moduleId, repo)
  }

  def analyzePackage(repo:ModuleRepository, pkg:model.Package, ns:EntityId.bound.Package) = {
    repo.register(SubPackage(ns, pkg.name))
    analyzeMethods(repo, pkg, ns)
    analyzePackages(repo, pkg, ns)
  }

  def analyzeMethods(repo:ModuleRepository, container:model.DocTemplateEntity, ns:BoundEntityId[_ <: Kind.ContainerKind[_]]):Unit = {
    container.methods.foreach {m =>
      repo.register(Def(ns, m.name, TypeParams(""), ValueParams(""), mkTypeRef(repo, m.resultType)))
    }
  }

  def analyzePackages(repo:ModuleRepository, container:model.Package, ns:EntityId.bound.Package):Unit = {
    container.packages.foreach {pkg =>
      analyzePackage(repo, pkg, ns)
    }
  }

  def mkTypeRef(repo:ModuleRepository, te:model.TypeEntity):TypeRef = {
    def resolveLinkTo(linkTo:base.LinkTo):EntityId.Type = linkTo match {
      case base.LinkToMember(mbr, tpl) =>
        throw new RuntimeException(s"Unsupported LinkTo type: ${linkTo}")
      case base.LinkToTpl(tpl:model.DocTemplateEntity) =>
        EntityId.unbound.unknown(tpl.name.split("\\.")).change(Kind.Type)
      case base.LinkToTpl(tpl) =>
        throw new RuntimeException(s"Unsupported LinkTo type: ${linkTo}")
      case base.LinkToExternal(name, url) =>
        throw new RuntimeException(s"Unsupported LinkTo type: ${linkTo}")
      case base.Tooltip(name) =>
        EntityId.unbound.unknown(name.split("\\.")).change(Kind.Type)
    }

    println(s"$te ${te.name} ${te.refEntity}")
    val baseType:EntityId.Type = resolveLinkTo(te.refEntity.values.head._1)

    TypeRef(baseType, TypeArgs("[...]"))
  }
}
