package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.nodemethods.OpAstNodeMethods
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpAstNodeTraversal
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Expression, Identifier, Local, Member, Method, MethodParameterIn, NamespaceBlock, Type, TypeDecl}
import org.json4s.*
import org.json4s.jackson.JsonMethods.*
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import org.json4s.JsonDSL.*
import overflowdb.*
import overflowdb.traversal.*
import io.shiftleft.semanticcpg.language.operatorextension.*
import io.shiftleft.semanticcpg.other.UsefullNodeEdgeSet.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

class TypeMethods(val node : Type) extends AnyVal with NodeExtension {

  def searchThrowType(typ: Type, edges: Array[String]): Type = {
    if (typ == null) return null
    //走到不能再走
    if (Iterator.single(typ).exists(x =>
      x.out(edges: _*).isEmpty))
      return typ

    Iterator.single(typ).
      repeat(
        _.out(edges: _*))(
        _.until(_.filter(x =>
          x.id() == typ.id() || x.out(edges: _*).isEmpty)
        )).collectAll[Type].headOption.orNull
  }

  def getRootType: Type = searchThrowType(node, SearchEdgeForRemovePointerArrayReferenceAliasSpecialize)
  
  def isPointerType: Boolean = {
    node != null && node._pointerOfOut.nonEmpty
  }

  def isArrayType: Boolean = {
    node != null && node._arrayOfOut.nonEmpty
  }

}
