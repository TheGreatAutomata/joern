package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

val resolver = NoResolve

class MethodMethods(val method: Method) extends AnyVal with NodeExtension with HasLocation {


  /** Traverse to annotations of method
    */
  def annotation: Iterator[Annotation] =
    method._annotationViaAstOut

  def local: Iterator[Local] =
    method._blockViaContainsOut.local
    
  def isBuildIn: Boolean = method.name.startsWith("<") || method.filename==null || method.filename.isEmpty || method.filename.startsWith("<")

  /** All control structures of this method
    */
  def controlStructure: Iterator[ControlStructure] =
    method.ast.isControlStructure

  def numberOfLines: Int = {
    if (method.lineNumber.isDefined && method.lineNumberEnd.isDefined) {
      method.lineNumberEnd.get - method.lineNumber.get + 1
    } else {
      0
    }
  }

  def isVariadic: Boolean = {
    method.parameter.exists(_.isVariadic)
  }

  def cfgNode: Iterator[CfgNode] =
    method._containsOut.collectAll[CfgNode]

  /** List of CFG nodes in reverse post order
    */
  def reversePostOrder: Iterator[CfgNode] = {
    def expand(x: CfgNode) = { x.cfgNext.iterator }
    NodeOrdering.reverseNodeList(NodeOrdering.postOrderNumbering(method, expand).toList).iterator
  }

  /** List of CFG nodes in post order
    */
  def postOrder: Iterator[CfgNode] = {
    def expand(x: CfgNode) = { x.cfgNext.iterator }
    NodeOrdering.nodeList(NodeOrdering.postOrderNumbering(method, expand).toList).iterator
  }

  /** The type declaration associated with this method, e.g., the class it is defined in.
    */
  def definingTypeDecl: Option[TypeDecl] =
    Iterator.single(method).definingTypeDecl.headOption

  /** The type declaration associated with this method, e.g., the class it is defined in. Alias for 'definingTypeDecl'
    */
  def typeDecl: Option[TypeDecl] = definingTypeDecl

  /** Traverse to method body (alias for `block`) */
  def body: Block =
    method.block

  override def location: NewLocation = {
    LocationCreator(method, method.name, method.label, method.lineNumber, method)
  }

  def content: Option[String] = {
    for {
      content <- method.file.content.headOption
      if content != File.PropertyDefaults.Content
      offset    <- method.offset
      offsetEnd <- method.offsetEnd
    } yield content.slice(offset, offsetEnd)
  }

  def allCalledMethods: Iterator[Method] = {
    val visited = collection.mutable.Set[Method]()
    def helper(m: Method): Iterator[Method] = {
      if (visited.contains(m)) {
        Iterator.empty
      } else {
        visited.add(m)
        val directCalledMethods = m.call.filterNot(_.isBuildIn).callee(resolver).l
        if (directCalledMethods.isEmpty) {
          Iterator.empty
        } else {
          val indirectCalledMethods = directCalledMethods.flatMap(m => helper(m)).l
          (directCalledMethods ++ indirectCalledMethods).iterator
        }
      }
    }
    helper(method)
  }

  def allAssignments: Iterator[Call] = {
    method.call.filter(_.name == "<operator>.assignment").iterator
  }
}
