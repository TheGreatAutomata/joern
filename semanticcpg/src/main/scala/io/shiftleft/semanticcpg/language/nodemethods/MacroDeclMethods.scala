package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.Implicits.IterableOnceDeco
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.nodemethods.AstNodeMethods.lastExpressionInBlock
import io.shiftleft.semanticcpg.utils.MemberAccess

class MacroDeclMethods(val node: MacroDecl) extends AnyVal with NodeExtension {
  def expandAstNodes: Iterator[AstNode] = node._inMacroIn.collectAll[AstNode]

  def body: Iterator[Block] = node.astChildren.collectAll[Block]

  def methodUsed: Iterator[Method] = node.astChildren.ast.collectAll[MethodRef]._refOut.collectAll[Method]

  def macroUsed: Iterator[MacroDecl] = node.astChildren.ast.collectAll[MacroRef]._refOut.collectAll[MacroDecl]

  def localUsed: Iterator[Local] = node.astChildren.ast.collectAll[Identifier]._refOut.collectAll[Local]
  
  def macroUsingMe: Iterator[MacroDecl] = node._refIn._astIn.collectAll[Block]._astIn.collectAll[MacroDecl]

  def helperForMacroUsingMeForRecursion(currentNode: MacroDecl, visited: Set[MacroDecl]): Set[MacroDecl] = {
    if (visited.contains(currentNode)) {
      Set.empty
    } else {
      val directReferences = currentNode._refIn._astIn.collectAll[Block]._astIn.collectAll[MacroDecl].toSet
      val newVisited = visited + currentNode
      val indirectReferences = directReferences.flatMap { macroDecl =>
        helperForMacroUsingMeForRecursion(macroDecl, newVisited)
      }
      directReferences ++ indirectReferences
    }
  }

  def helperForMacroUsedForRecursion(currentNode: MacroDecl, visited: Set[MacroDecl]): Set[MacroDecl] = {
    if (visited.contains(currentNode)) {
      // 如果当前节点已经访问过，直接返回空集合
      Set.empty
    } else {
      // 收集当前节点直接引用的所有宏定义
      val directReferences = currentNode.astChildren.ast.collectAll[MacroRef]._refOut.collectAll[MacroDecl].toSet

      // 更新已访问集合
      val newVisited = visited + currentNode

      // 递归地查找每个直接引用的宏定义
      val indirectReferences = directReferences.flatMap { macroDecl =>
        helperForMacroUsedForRecursion(macroDecl, newVisited)
      }

      // 返回当前宏定义的直接引用和间接引用的并集
      directReferences ++ indirectReferences
    }
  }

  def macroUsedForRecursion: Iterator[MacroDecl] = helperForMacroUsedForRecursion(node, Set.empty).iterator

  def macroUsingMeForRecursion: Iterator[MacroDecl] = helperForMacroUsingMeForRecursion(node, Set.empty).iterator

  }
