package io.shiftleft.semanticcpg.dependencyJsonGenerator

import io.shiftleft.semanticcpg.language.operatorextension.nodemethods.OpAstNodeMethods
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpAstNodeTraversal
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Identifier, Local, Member, Method, MethodParameterIn, NamespaceBlock, TypeDecl, Type}
import org.json4s.*
import org.json4s.jackson.JsonMethods.*
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import org.json4s.JsonDSL.*
import overflowdb.*
import overflowdb.traversal.*
import io.shiftleft.semanticcpg.language.operatorextension.*

class DependencyJsonGenerator(val traversal: Iterator[Method]) extends AnyVal {
//todo：没有填写的字段:
//param_variable::type_info::kind,size,source_end_line
//references 

  def dependencyJson: Iterator[String] = traversal.map(dependencyJson)
//  collectAll[Local].headOption.map(_.fullName).getOrElse("")
  def getVariableJson(identifiers: List[Identifier]): JArray = {

    val identifierPreMap = identifiers.groupBy(_._refOut.headOption.map(_.label).getOrElse(""))
    val localList = identifierPreMap.getOrElse(NodeTypes.LOCAL, List.empty[Identifier])
    val parameterList = identifierPreMap.getOrElse(NodeTypes.METHOD_PARAMETER_IN, List.empty[Identifier])
    val localMap = localList.groupBy(_._refOut.collectAll[Local].headOption.map(_.fullName).getOrElse(""))
    val parameterMap = parameterList.groupBy(_._refOut.collectAll[MethodParameterIn].headOption.map(_.name).getOrElse(""))
    //处理单纯的identifier情况
    val identifierDirectlyJson1 = localMap.map{ case (_, list) =>
      handleIdentifier(list)
    }
    val identifierDirectlyJson2 = parameterMap.map { case (_, list) =>
      handleIdentifier(list)
    }

    //获得identifier路线上所有的fieldAccess
    val identifiersFieldAccessMap = identifiers.repeat(_.in(EdgeTypes.AST))(_.emit.until(_.or(_.hasLabel(NodeTypes.METHOD), _.hasLabel(NodeTypes.NAMESPACE_BLOCK))))
      .collectAll[io.shiftleft.codepropertygraph.generated.nodes.Call].filter(x => allFieldAccessTypes.contains(x.methodFullName)).groupBy(_.code)
    //处理filedIdentifier情况
    val identifiersFieldAccessJson = identifiersFieldAccessMap.map { case (key, list) =>
      handleFieldAccess(list)
    }
    // 合并两个 Iterable
    val combinedIterable: Iterable[JObject] = identifierDirectlyJson1 ++ identifierDirectlyJson2 ++ identifiersFieldAccessJson
    // 将合并后的 Iterable 转换为 JArray
    JArray(combinedIterable.toList)
  }

  def dependencyJson(method: Method): String = {

    //end_line*
    val end_len: Long = method.lineNumberEnd.get.longValue()

    //global_variable*
    //函数中所有的对应于全局变量的identifier
    val globalIdentifiers = method.block.ast.isIdentifier
      .filter(i => {
        val localParent = i.refOut.collectAll[Local].astParent
        localParent.nonEmpty && (localParent.hasLabel(NodeTypes.TYPE_DECL).nonEmpty ||
          localParent.hasLabel(NodeTypes.NAMESPACE_BLOCK).nonEmpty)
      }).l

    val globalVariableJson = getVariableJson(globalIdentifiers)

    val parameters = method.parameter.l
    //param_list*
    val paramListJsonTemp = parameters.map(getParameterListJson)
    val paramListJson = JArray(paramListJsonTemp.toList)

    //param_variable*
    val paraIdentifiers = parameters._refIn.collectAll[Identifier].l
    val paraVariableJson = getVariableJson(paraIdentifiers)

    //references*

    //start_line*
    val start_line: Long = method.lineNumber.get.longValue()

    //uri*
    val uri = method.filename

    val reJson =
      (
        method.fullName ->
          ("end_len" -> end_len) ~
            ("global_variable" -> globalVariableJson) ~
            ("param_list" -> paramListJson) ~
            ("param_variable" -> paraVariableJson) ~
            ("start_line" -> start_line) ~
            ("uri" -> uri)
        )
    val jsonString = pretty(render(reJson))
    jsonString
  }

  def getTypeDecl(typ: Type): TypeDecl = {
    //走到不能再走
    if(Iterator.single(typ).exists(x =>
      x.out(EdgeTypes.POINTER_OF, EdgeTypes.ARRAY_OF, EdgeTypes.L_REFERENCE_OF, EdgeTypes.R_REFERENCE_OF, EdgeTypes.SPECIALIZE_OF).isEmpty))
      return typ._refOut.collectAll[TypeDecl].headOption.orNull
    
    Iterator.single(typ).
      repeat(
        _.out(EdgeTypes.POINTER_OF, EdgeTypes.ARRAY_OF, EdgeTypes.L_REFERENCE_OF, EdgeTypes.R_REFERENCE_OF, EdgeTypes.SPECIALIZE_OF))(
        _.until(_.filter(x =>
          x.out(EdgeTypes.POINTER_OF, EdgeTypes.ARRAY_OF, EdgeTypes.L_REFERENCE_OF, EdgeTypes.R_REFERENCE_OF, EdgeTypes.SPECIALIZE_OF).isEmpty)
        )).collectAll[Type]._refOut.collectAll[TypeDecl].headOption.orNull
  }

  def getTypeInfo(typeDecl: TypeDecl): JObject = {
    val filename = typeDecl.filename
    val kind = ""
    val size:Long = -1
    val source_beg_line:Long = typeDecl.lineNumber.map(_.longValue()).getOrElse(-1)
    val source_end_line:Long = -1
    val type_id:Long = typeDecl.id()
    val type_name = typeDecl.fullName
    ("type_info"->(
      ("filename"->filename)~
        ("kind"->kind)~
        ("size"->size)~
        ("source_beg_line"->source_beg_line)~
        ("source_end_line"->source_end_line)~
        ("type_id"->type_id)~
        ("type_name"->type_name)
    ))
  }

  def getTypeInfoAndNameHelper(astNode: AstNode): JObject = {
    val typ = astNode._evalTypeOut.collectAll[Type].headOption.orNull
    if(typ != null){
      val typeDecl = getTypeDecl(typ)
      if (typeDecl != null) {
        return getTypeInfo(typeDecl)
      }
    }
    ("type_info" -> (
      ("filename" -> "") ~
        ("kind" -> "") ~
        ("size" -> -1) ~
        ("source_beg_line" -> -1) ~
        ("source_end_line" -> -1) ~
        ("type_id" -> -1) ~
        ("type_name" -> "")
      ))
  }
  
  def getTypeInfoAndName(identifier: Identifier): JObject = {
    val name = identifier.name
    val type_info = getTypeInfoAndNameHelper(identifier)
    ("name"->name) ~ type_info
  }

  def getTypeInfoAndName(call: Call): JObject = {
    val name = call.code
    val type_info = getTypeInfoAndNameHelper(call)
    ("name" -> name) ~ type_info
  }
  
  def getAttributes(astNode: List[AstNode]): JObject = {
    val is_modified = astNode.find(isInLeftValue) match {
      case Some(identifier) =>
        true
      case None =>
        //todo：这里考察一些不直接在左值的情况，如被被解引用、index操作包括之后再放到左值上
        false
    }
    val used_in_branch = astNode.find(isInCondition) match {
      case Some(identifier) =>
        true
      case None =>
        false
    }
    val used_in_index = astNode.find(isInIndex) match {
      case Some(identifier) =>
        true
      case None =>
        false
    }
    ("attributes" ->
      ("is_modified" -> is_modified) ~
        ("used_in_branch" -> used_in_branch) ~
        ("used_in_index" -> used_in_index)
      )
  }

  //这个是在map中使用的，list不会为空
  def handleIdentifier(identifiers: List[Identifier]): JObject = {
    //get name and type_info
    val single = getTypeInfoAndName(identifiers.head)
    //get attributes
    val attributes = getAttributes(identifiers)
    attributes~single
  }

  //这个是在map中使用的，list不会为空
  def handleFieldAccess(call: List[Call]): JObject = {
    //get name and type_info
    val single = getTypeInfoAndName(call.head)
    //get attributes
    val attributes = getAttributes(call)
    attributes ~ single
  }
  
  def getUpThrowLeftValue(astNode: AstNode): AstNode = {
    if(Iterator.single(astNode).exists(x => !{
      x.order == 1 && {
        val xP = x.astParent
        xP.isCall && allLeftValueTypes.contains(xP.asInstanceOf[Call].methodFullName)
      }
    })){
      return astNode
    }

    Iterator.single(astNode).repeat(
      _.astParent)(
      _.until(_.filter(x => !{
        x.order == 1 && x.label != NodeTypes.METHOD && x.label != NodeTypes.NAMESPACE_BLOCK && {
          val xP = x.astParent
          xP.isCall && allLeftValueTypes.contains(xP.asInstanceOf[Call].methodFullName)
        }
      }))
    ).filter(x => x.label != NodeTypes.METHOD && x.label != NodeTypes.NAMESPACE_BLOCK).headOption.orNull
  }

  def isInIndex(astNode: AstNode): Boolean = {
    if(Iterator.single(astNode).exists((
      _.or(
        _.and(
          _.order(2),
          x => {
            val xP = x.astParent.headOption.orNull
            (xP!=null && xP.isCall && allArrayAccessTypes.contains(xP.asInstanceOf[Call].methodFullName)) return x
          }
        ),
        _.or(
          _.hasLabel(NodeTypes.METHOD),
          _.hasLabel(NodeTypes.NAMESPACE_BLOCK)
        )
      )
      ).exists(x => x.label != NodeTypes.METHOD && x.label != NodeTypes.NAMESPACE_BLOCK))) return true

    Iterator.single(astNode).repeat(_.astParent)(
      _.until(_.or(
        _.and(
          _.order(2),
          x => {
            val xP = x.astParent.headOption.orNull
            (xP != null && xP.isCall && allArrayAccessTypes.contains(xP.asInstanceOf[Call].methodFullName))
          }
        ),
        _.or(
          _.hasLabel(NodeTypes.METHOD),
          _.hasLabel(NodeTypes.NAMESPACE_BLOCK)
        )
      ))
    ).exists(x => x.label != NodeTypes.METHOD && x.label != NodeTypes.NAMESPACE_BLOCK)
  }
  
  def isInLeftValue(astNode: AstNode): Boolean = {
    val checkStmt = getUpThrowLeftValue(astNode)
    checkStmt!=null && isInLeftValueDirectly(checkStmt)
  }

  def isInLeftValueDirectly(astNode: AstNode): Boolean = {
    astNode.order == 1 && {
      val parent = astNode.astParent
      parent.isCall && allAssignmentTypes.contains(parent.asInstanceOf[Call].methodFullName)
    }
  }
  
  def isInCondition(astNode: AstNode): Boolean = {
    if(Iterator.single(astNode).exists(x => {
      x.in(EdgeTypes.CONDITION).nonEmpty
    })) return true
    Iterator.single(astNode).repeat(_.in(EdgeTypes.AST))(
      _.until(_.or(
        _.in(EdgeTypes.CONDITION),
        _.or(_.hasLabel(NodeTypes.METHOD), _.hasLabel(NodeTypes.NAMESPACE_BLOCK))
      ))
    ).cast[AstNode]._conditionIn.nonEmpty
  }

  def getParameterListJson(parameter: MethodParameterIn): JObject = {
    //data_type*
    //name*
    //size
    val data_type = parameter.typeFullName
    val name = parameter.name
    ("data_type" -> data_type)~ 
      ("name" -> name)~
      ("size" -> -1)~
      ("typeIid" -> parameter.id())
    
  }

}
