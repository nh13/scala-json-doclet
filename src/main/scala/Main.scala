package com.todesking.json_doclet

import scala.tools.nsc.doc.doclet.{Generator, Universer, Indexer}

class JsonDoclet extends Generator with Universer with Indexer {
  override def generateImpl():Unit = {
    println("doclet!!!!")
  }
}
