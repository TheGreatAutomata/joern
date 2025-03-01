package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.*
import io.shiftleft.semanticcpg.language.operatorextension.nodemethods.OpAstNodeMethods
import io.shiftleft.semanticcpg.other.UsefullNodeEdgeSet.*
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.jackson.JsonMethods.*
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import overflowdb.*
import overflowdb.traversal.*

import scala.collection.mutable.{HashSet, ListBuffer}

class TypeDeclMethods(val node : TypeDecl) extends AnyVal with NodeExtension {
  def isBuildIn: Boolean = node.filename==null || node.filename.isEmpty || node.filename.startsWith("<")
}
