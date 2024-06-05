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
import io.shiftleft.semanticcpg.other.UsefullNodeEdgeSet._

class DependencyJsonGenerator(val traversal: Iterator[Method]) extends AnyVal {
//todo：没有填写的字段:
//param_variable::type_info::kind,size,source_end_line
//references 

  def dependencyJson: Iterator[String] = traversal.map(dependencyJson)

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
    val identifiersFieldAccessMap =
      identifiers.repeat(_.in(EdgeTypes.AST))(_.emit.until(_.or(_.hasLabel(NodeTypes.METHOD), _.hasLabel(NodeTypes.NAMESPACE_BLOCK))))
      .collectAll[io.shiftleft.codepropertygraph.generated.nodes.Call]
        .filter(x => allFieldAccessTypes.contains(x.methodFullName)).groupBy(_.code)
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
        val localParent = i.refOut.collectAll[Local].astParent.l
        localParent.nonEmpty && (localParent.hasLabel(NodeTypes.TYPE_DECL).nonEmpty ||
          localParent.hasLabel(NodeTypes.NAMESPACE_BLOCK).nonEmpty)
      }).l

    val globalVariableJson = getVariableJson(globalIdentifiers)

    val parameters = method.parameter.l
    //param_list*
    val paramListJsonTemp = parameters.map(getParameterListJson)
    val paramListJson = JArray(paramListJsonTemp)

    //param_variable*
    val paraIdentifiers = parameters._refIn.collectAll[Identifier].l
    val paraVariableJson = getVariableJson(paraIdentifiers)

    //references*
    val referenceJson = getReferencesJson(globalIdentifiers ++ paraIdentifiers)

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
            ("references" -> referenceJson)~
            ("start_line" -> start_line) ~
            ("uri" -> uri)
        )
    val jsonString = pretty(render(reJson))
    jsonString
  }

  def getReferencesJson(astNode: List[AstNode]): JArray = {

    val pointerAssignments = astNode.repeat(_.in(EdgeTypes.AST))(_.emit.until(_.or(_.hasLabel(NodeTypes.METHOD), _.hasLabel(NodeTypes.NAMESPACE_BLOCK))))
        .collectAll[io.shiftleft.codepropertygraph.generated.nodes.Expression]
        .filter(x => {
          val xT = x._evalTypeOut.headOption.orNull
          xT != null && {
            xT._pointerOfOut.nonEmpty || xT._arrayOfOut.nonEmpty || {
              Iterator.single(xT).repeat(
                _.out(EdgeTypes.L_REFERENCE_OF, EdgeTypes.R_REFERENCE_OF))(
                _.until(_.collectAll[Type].or(_._pointerOfOut, _._arrayOfOut))
              ).nonEmpty
            }
          }
        }).filter(x => {
          if (x.order != 1) false else
          {
            val xP = x.astParent
            xP != null && xP.isCall && allAssignmentTypes.contains(xP.asInstanceOf[Call].name)
          }
        }).astParent.collectAll[Call].l

      //处理pointerAssignments
      val pointerAssignmentsJson = pointerAssignments.map(getPointerAssignmentsLeftJson)

      //遍历右值的reachingDef，找到其中所有alloc
      val reachingDefLoc = pointerAssignments.astChildren.order(2)._reachingDefIn.collectAll[AstNode]
      val validLoc1 = reachingDefLoc.astParent.filter(xP => {
        xP.isCall && allAssignmentTypes.contains(xP.asInstanceOf[Call].name)
      })
    val validLoc2 = validLoc1.filter(x => {
      val rL = x.astChildren.order(2).headOption.orNull
      //todo:这里可能被类型转换打断？
      rL!=null && rL.isCall && allAllocTypes.contains(rL.asInstanceOf[Call].name)
    }).collectAll[Call]
    //validLoc2即为要加入json的右侧带alloc的赋值操作
    val allocJson = validLoc2.map(getPointerAssignmentsAllocTrueLeftJson)
      
      JArray(pointerAssignmentsJson ++ allocJson)
  }

  def getPointerAssignmentsLeftJsonHelper(call: Call): JObject={
    val code = call.code
    val line: Long = call.lineNumber.getOrElse(java.lang.Integer((-1))).longValue()
    val uri = call.method.filename
    val variable = getLeftValueCode(call)
    ("code" -> code) ~
      ("line" -> line) ~
      ("uri" -> uri) ~
      ("variable" -> variable)
  }

  def getPointerAssignmentsLeftJson(call: Call): JObject = {
      getPointerAssignmentsLeftJsonHelper(call)~("is_malloc" -> isRightValueMalloc(call))
  }

  def getPointerAssignmentsAllocTrueLeftJson(call: Call): JObject = {
    getPointerAssignmentsLeftJsonHelper(call) ~ ("is_malloc" -> true)
  }

  def getLeftValueCode(call: Call):String = {
    call.astChildren.order(1).code.headOption.getOrElse("")
  }

  def isRightValueMalloc(call: Call): Boolean = {
    call.astChildren.order(2).ast.collectAll[Call].name.exists(x => allAllocTypes.contains(x))
  }

  def searchThrowType(typ:Type, edges:Array[String]):Type={
    if(typ==null)return null
    //走到不能再走
    if(Iterator.single(typ).exists(x =>
      x.out(edges:_*).isEmpty))
      return typ

    Iterator.single(typ).
      repeat(
        _.out(edges:_*))(
        _.until(_.filter(x =>
          x.out(edges:_*).isEmpty)
        )).collectAll[Type].headOption.orNull
  }

  def getTypeInfoTypeDeclPart(typeDecl: TypeDecl): JObject = {
    if(typeDecl==null){
      ("base_type_id"->"")~
        ("index_in_base"-> -1)~
        ("filename"->"")
    }
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
  
  def getTypeInfoAndName(identifier: Identifier): JObject = {
    val typ = identifier._evalTypeOut.collectAll[Type].headOption.orNull
    ("name"->identifier.name) ~ addTypeInfoTitle(getTypeInfo(typ))
  }

  def getTypeInfoAndName(call: Call): JObject = {
    val typ = call._evalTypeOut.collectAll[Type].headOption.orNull
    ("name" -> call.code) ~ addTypeInfoTitle(getTypeInfo(typ)~getTypeInfoAddition(call))
  }

  def getTypeInfoAddition(call:Call):JObject={
    val member = call._refOut.collectAll[Member].headOption.orNull
    val memberTypeDecl = Iterator.single(member.astParent).collectAll[TypeDecl].headOption.orNull
    val base_type_id:Long = if(memberTypeDecl==null) -1 else memberTypeDecl.id()
    //todo:补完
    ("base_type_id" -> base_type_id)~
      ("index_in_base"-> -1)~
      ("offset_in_base"-> -1)
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

  def isInIndexDirectly(astNode: AstNode):Boolean = {
    (astNode.order==2 && {
      val xP = astNode.astParent
      (xP != null && xP.isCall && allArrayAccessTypes.contains(xP.asInstanceOf[Call].methodFullName))
    }) ||
      (astNode.label == NodeTypes.METHOD || astNode.label == NodeTypes.NAMESPACE_BLOCK)
  }

  def isInIndex(astNode: AstNode): Boolean = {
    if(astNode.label == NodeTypes.METHOD || astNode.label == NodeTypes.NAMESPACE_BLOCK) return false
    if(Iterator.single(astNode).exists(isInIndexDirectly)) return true

    Iterator.single(astNode).repeat(_.astParent)(
      _.until(_.filter(isInIndexDirectly))
    ).exists(x => x.label != NodeTypes.METHOD && x.label != NodeTypes.NAMESPACE_BLOCK)
  }
  
  def isInLeftValue(astNode: AstNode): Boolean = {
    val checkStmt = getUpThrowLeftValue(astNode)
    checkStmt!=null && isInLeftValueDirectly(checkStmt)
  }

  def isInLeftValueDirectly(astNode: AstNode): Boolean = {
    astNode.order == 1 && {
      val parent = astNode.astParent
      parent.isCall && (allAssignmentTypes.contains(parent.asInstanceOf[Call].methodFullName) ||
        additionAssignmentTypes.contains(parent.asInstanceOf[Call].name))
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
    val paraType = parameter._evalTypeOut.collectAll[Type].headOption.orNull
    val name = parameter.name
    ("name"->name)~addTypeInfoTitle(getTypeInfo(paraType))
  }

  //offset_in_base、index_in_base要另外填写
  //存在疑问的：source_beg_line、source_end_line
  /*
  可以根据当前type《type1》得出的：
  kind：pointer、array、左右引用，只考察第一层
  size：当前type的大小，本来的

  需要根据当前type去掉指针、array、引用后的type《type2》得出的：
  type_name：

  需要将type2去掉所有别名、引用、特化、pinter、array得到的type《type3》得出的
  type_alias_name：

  需要将type3的typeDecl得出的：
  type_id：typeDecl的id
  filename：typeDecl所在的位置
  source_beg_line：typeDecl的范围
  source_end_line：typeDecl的范围

  如果是memberAccess，需要找到基类typeDecl得出的
  base_type_id：基类的id
  index_in_base：基类中的第几个
  offset_in_base：基类中的地址开始
   */
  def getTypeInfo(theType: Type): JObject = {
    val oriTypeJson = getTypeInfoForOriType(theType)
    val typeThrowPointerArrayReference =
      searchThrowType(theType, SearchEdgeForRemovePointerArrayReference)
    val throwPointerArrayReferenceTypeJson =
      getTypeInfoForTypeThrowPointerArrayReference(typeThrowPointerArrayReference)
    val myTypeDeclType =
      searchThrowType(typeThrowPointerArrayReference,
        SearchEdgeForRemovePointerArrayReferenceAliasSpecialize)
    val typeDeclTypeJson =
      getTypeInfoForTypeDeclType(myTypeDeclType, typeThrowPointerArrayReference)
    val myTypeDecl = getDirectlyTypeDecl(myTypeDeclType)
    val myTypeDeclJson = getTypeInfoForTypeDecl(myTypeDecl)
    oriTypeJson~throwPointerArrayReferenceTypeJson~typeDeclTypeJson~myTypeDeclJson
  }

  def addTypeInfoTitle(j:JObject):JObject={
    ("type_info"->j)
  }

  def getTypeInfoForTypeDecl(myTypeDecl:TypeDecl):JObject={
    if(myTypeDecl==null){
      ("type_id"-> -1)~
        ("filename" -> "") ~
        ("source_beg_line"-> -1)~
        ("source_end_line"-> -1)
    }else{
      val source_beg_line:Long = myTypeDecl.lineNumber.getOrElse(java.lang.Integer(-1)).longValue()
      //todo:补完
      val source_end_line:Long = -1
      ("type_id" -> myTypeDecl.id()) ~
        ("filename" -> myTypeDecl.filename) ~
        ("source_beg_line" -> source_beg_line) ~
        ("source_end_line" -> source_end_line)
    }
  }

  def getDirectlyTypeDecl(theType:Type):TypeDecl={
    if(theType==null) return null
    theType._refOut.collectAll[TypeDecl].headOption.orNull
  }

  def getTypeInfoForTypeDeclType(theType:Type, beforeType:Type):JObject={
    if(theType==null||beforeType==null){
      ("type_alias_name"->"")
    }else{
      //todo：泛型在这里应该会丢掉实例化，需要修复
      val theValue = theType.name
      val va = if(theValue==beforeType.name) "" else theValue
      ("type_alias_name"->va)
    }
  }

  def getTypeInfoForTypeThrowPointerArrayReference(theType:Type):JObject = {
    if(theType==null){
      ("type_name"->"")
    }else{
      //todo：泛型在这里应该会丢掉实例化，需要修复
      ("type_name" -> theType.name)
    }
  }

  def getTypeInfoForOriType(theType:Type):JObject={
    if(theType==null){
      ("kind"->"")~
        ("size"-> -1)
    }else{
      ("kind"->getTypeInfoKind(theType))~
      //todo:补全
        ("size"-> -1)
    }
  }

  def getTypeInfoKind(theType:Type):String={
    val isPointer = theType._pointerOfOut.nonEmpty
    if(isPointer) return "Pointer"
    val isArray = theType._arrayOfOut.nonEmpty
    if(isArray) return "Array"
    val isLR = theType._lReferenceOfOut.nonEmpty
    if(isLR) return "LValueReference"
    val isRR = theType._rReferenceOfOut.nonEmpty
    if(isRR) return "RValueReference"
    "Elaborated"
  }
}
